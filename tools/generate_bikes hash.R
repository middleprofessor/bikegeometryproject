# tools/generate_bikes.R

suppressPackageStartupMessages({
  library(here)
  library(fs)
  library(data.table)
  library(jsonlite)
  library(digest)   # NEW: for hashing
})

# Work from project root
setwd(here::here())

site_base_url   <- "https://thegravelbikegeometryproject.com"
data_rds_abs    <- here::here("rds", "geobike.Rds")
if (!file.exists(data_rds_abs)) {
  stop("Data RDS not found at: ", data_rds_abs)
}

# Optional: debug limit (only create first N bikes)
BIKE_LIMIT <- as.integer(Sys.getenv("BIKE_LIMIT", "0"))

# NEW: bump this when you make a significant change to _Rmd/bike_template.qmd
template_version <- 1L

slugify <- function(x) {
  x <- iconv(x, "", "ASCII//TRANSLIT")
  x <- tolower(gsub("&", " and ", x))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-|-$)", "", x)
  gsub("-{2,}", "-", x)
}

to_chr <- function(v) {
  if (!is.list(v)) return(as.character(v))
  vapply(v, function(x) if (length(x)) as.character(x[[1]]) else NA_character_, "")
}

# ------------- load data -------------

geobike <- readRDS(data_rds_abs)
if (!is.data.table(geobike)) setDT(geobike)

req <- c("brand", "model_name", "year")
missing <- setdiff(req, names(geobike))
if (length(missing)) stop("Missing columns in geobike: ", paste(missing, collapse = ", "))

geobike[, (req) := lapply(.SD, to_chr), .SDcols = req]

pages <- unique(geobike[, ..req])
pages[, slug := slugify(paste(brand, model_name, year))]

if (BIKE_LIMIT > 0L) {
  pages <- pages[seq_len(min(BIKE_LIMIT, nrow(pages)))]
}

dir_create(here::here("bikes"))

# ------------- incremental hashing -------------

# Which columns should trigger a rebuild if they change?
likely_delta <- c(
  "reach", "stack", "head_tube_angle", "seat_tube_angle", "wheelbase",
  "chainstay", "bb_drop", "trail", "fork_offset", "head_tube", "top_tube"
)
delta_cols <- c(req, intersect(likely_delta, names(geobike)))

setkeyv(geobike, req)

hash_row <- function(dt_row) {
  # Include template_version so bumping it forces a rebuild for all bikes
  vals <- as.list(dt_row[, ..delta_cols])
  vals$template_version <- template_version
  digest(vals, algo = "xxhash64")
}

pages[, hash := geobike[.SD, on = req, mult = "first"][, hash_row(.SD)], by = .I]

# Manifest file to remember previous hashes
manifest_path <- here::here("bikes", "bikes_manifest.json")
manifest <- if (file.exists(manifest_path)) {
  jsonlite::read_json(manifest_path, simplifyVector = TRUE)
} else {
  list()
}

# ------------- write per-bike wrappers (incremental) -------------

write_wrapper_qmd <- function(path_abs, brand, model, year, slug,
                              data_rds_abs, site_base_url, template_version) {
  title <- sprintf("%s %s %s", brand, model, year)
  
  txt <- paste0(
    "---\n",
    'title: "', title, "\"\n",
    "format:\n  html:\n    toc: false\n",
    "freeze: auto\n",
    "template_version: ", template_version, "\n",
    "params:\n",
    "  brand: ",         jsonlite::toJSON(brand,        auto_unbox = TRUE), "\n",
    "  model: ",         jsonlite::toJSON(model,        auto_unbox = TRUE), "\n",
    "  year: ",          jsonlite::toJSON(year,         auto_unbox = TRUE), "\n",
    "  slug: ",          jsonlite::toJSON(slug,         auto_unbox = TRUE), "\n",
    "  data_path: ",     jsonlite::toJSON(data_rds_abs, auto_unbox = TRUE), "\n",
    "  site_base_url: ", jsonlite::toJSON(site_base_url, auto_unbox = TRUE), "\n",
    "---\n\n",
    # wrapper is in bikes/, template is in _Rmd/, so go up one level
    "```{r, child=\"../_Rmd/bike_template.qmd\"}\n",
    "```\n"
  )
  
  writeLines(txt, path_abs)
}

message("Creating/updating wrappers for ", nrow(pages), " bikes (incremental)")

for (i in seq_len(nrow(pages))) {
  b <- pages$brand[i]
  m <- pages$model_name[i]
  y <- pages$year[i]
  s <- pages$slug[i]
  h <- pages$hash[i]
  
  key <- paste(b, m, y, sep = "||")
  wrapper_abs <- here::here("bikes", paste0(s, ".qmd"))
  
  # Incremental logic: skip if hash unchanged
  if (!is.null(manifest[[key]]) && identical(manifest[[key]], h)) {
    # No data/template change for this bike → don't touch its QMD
    next
  }
  
  message("  updating ", b, " ", m, " ", y, " -> bikes/", s, ".qmd")
  
  write_wrapper_qmd(wrapper_abs, b, m, y, s, data_rds_abs, site_base_url, template_version)
  manifest[[key]] <- h
}

# Save updated manifest
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

# ------------- write bikes/bike_index.qmd (always) -------------

rows_html <- sprintf(
  '<tr><td>%s</td><td>%s</td><td>%s</td><td><a href="/bikes/%s.html">%s %s %s</a></td></tr>',
  pages$brand, pages$model_name, pages$year,
  pages$slug,
  pages$brand, pages$model_name, pages$year
)

table_html <- paste0(
  '<table class="table table-sm table-striped" style="width:100%">',
  '<thead><tr><th>Brand</th><th>Model</th><th>Year</th><th>Page</th></tr></thead>',
  '<tbody>', paste(rows_html, collapse = ""), '</tbody></table>'
)

bike_index_qmd <- paste0(
  "---\n",
  'title: "All Bike Models"\n',
  "format:\n  html: default\n",
  "freeze: auto\n",
  "---\n\n",
#  "# All Bike Models\n\n",
  table_html, "\n"
)

writeLines(bike_index_qmd, here::here("bikes", "bike_index.qmd"))

message("Done writing wrappers and bike_index.qmd (incremental).")
