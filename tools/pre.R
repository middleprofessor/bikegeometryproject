# tools/pre.R
if (Sys.getenv("QUARTO_INNER", "0") == "1") quit(save = "no", status = 0)
source("tools/generate_bikes.R", chdir = FALSE)
