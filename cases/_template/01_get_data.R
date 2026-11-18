# 01_get_data.R — fetch and cache raw data ------------------------------------
# Case: {{CASE_ID}}
# Run from the case folder:  Rscript 01_get_data.R
# Output: data_raw/<source>_<YYYY-MM-DD>.* plus data_raw/_access_log.csv
# Rule: no manual downloads. If unavoidable, document the click-path in 00_meta.yml.

stopifnot("Run this script from the case folder" = file.exists("00_meta.yml"))
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
meta <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")
dir.create("data_raw", showWarnings = FALSE)
options(acutils.data_dir = "data_raw")   # all acutils fetchers cache here

# --- Fetch -------------------------------------------------------------------
# Uncomment and adapt. Each call caches with the access date in the file name and
# returns the cached copy on re-runs (refresh = TRUE forces a new download).

# tfp  <- get_usda_tfp()                                         # country TFP 1961–2023
# fao  <- get_faostat("QCL", items = c("Wheat", "Maize (corn)"),
#                     areas = c("ETH", "KEN", "TZA"), years = 2000:2024)
# eaa  <- get_eurostat("aact_eaa01", filters = list(geo = c("DE", "FR", "IT")))
# wdi  <- get_wdi(c(ag_va_share = "NV.AGR.TOTL.ZS"), countries = c("VNM", "IDN"), years = 2000:2024)
# pink <- get_pinksheet()                                        # monthly commodity prices
# t2m  <- get_era5_monthly("2m_temperature", bbox = c(5, 35, 19, 48), years = 2000:2024)

# --- Sanity checks: rows, year range, NA share, units --------------------------
# print(nrow(tfp)); print(range(tfp$year)); print(mean(is.na(tfp$value)))

# --- Record access dates for 00_meta.yml -----------------------------------------
log <- access_log()
if (nrow(log) > 0) {
  message("Access log (copy versions/dates into 00_meta.yml -> sources):")
  print(log)
} else {
  message("No downloads recorded yet: edit the fetch section above.")
}
