# generate_pages.R
library(data.table)
library(fs)
library(quarto)

# Slugify (URL-safe)
slugify <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("&", " and ", x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-|-$)", "", x)
  x <- gsub("-{2,}", "-", x)
  x
}

# Load data
geobike <- readRDS("data/geobike.rds")  # adjust path
stopifnot(all(c("brand","model","year") %in% names(geobike)))
geobike[, brand := as.character(brand)]
geobike[, model := as.character(model)]
geobike[, year  := as.character(year)]

# Output dir
dir_create("bikes")

# Unique pages + slugs
pages <- unique(geobike[, .(brand, model, year)])
pages[, slug := slugify(paste(brand, model, year))]
if (any(duplicated(pages$slug))) {
  pages[, dup_n := ave(seq_len(.N), slug, FUN = seq_along)]
  pages[!is.na(dup_n), slug := paste0(slug, "-", dup_n)]
}

site_base_url <- "https://thegravelbikegeometryproject.com"  # UPDATE
template_file <- "bike_template.qmd"

# Render each page
for (i in seq_len(nrow(pages))) {
  row <- pages[i]
  quarto_render(
    input = template_file,
    execute_params = list(
      brand         = row$brand,
      model         = row$model,
      year          = row$year,
      slug          = row$slug,
      data_path     = "data/geobike.rds",
      site_base_url = site_base_url
    ),
    output_file = file.path("bikes", paste0(row$slug, ".html"))
  )
}

# Build index
bix <- pages[, .(
  brand, model, year,
  link = sprintf('<a href="/bikes/%s.html">%s %s %s</a>', slug, brand, model, year)
)]
fwrite(bix, "bikes_index.tsv", sep = "\t")

index_qmd <- '
---
title: "All Bike Models"
format: html
---

# All Bike Models

```{r, include=FALSE}
library(data.table); library(knitr)
dt <- fread("bikes_index.tsv")
```

```{r, echo=FALSE, results="asis"}
cat(knitr::kable(dt[, .(brand, model, year, link)],
                 format = "html",
                 table.attr = "class=\\"table table-sm table-striped\\""))
```
'
writeLines(index_qmd, "bikes/index.qmd")
quarto_render("bikes/index.qmd", output_file = "bikes/index.html")

# sitemap.xml + robots.txt
s_urls <- sprintf("<url><loc>%s/bikes/%s.html</loc></url>", site_base_url, pages$slug)
sitemap <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
  sprintf('<url><loc>%s/</loc></url>', site_base_url),
  sprintf('<url><loc>%s/bikes/index.html</loc></url>', site_base_url),
  s_urls,
  '</urlset>'
)
writeLines(sitemap, "sitemap.xml")

robots <- c(
  "User-agent: *",
  "Allow: /",
  sprintf("Sitemap: %s/sitemap.xml", site_base_url)
)
writeLines(robots, "robots.txt")

message("Done: model pages, bikes index, sitemap.xml, robots.txt")
