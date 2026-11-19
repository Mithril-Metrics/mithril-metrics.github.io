# 01_get_data.R — fetch and cache raw data ------------------------------------
# Case: 2026-11-elnino-rice-tails (local folder: AgEcon/Issue 1/)
# Output: data_raw/faostat_QCL_<date>.zip, data_raw/noaa-oni_<date>.txt,
#         data_raw/_access_log.csv, output/cache/raw.rds
# Everything is written inside this folder. Expected size of data_raw/: ~35 MB.

stopifnot("Run this script from the issue folder (AgEcon/Issue 1)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw", timeout = 600)

# --- Packages: install only what is missing ---------------------------------------
if (!requireNamespace("acutils", quietly = TRUE)) {
  stop("Install acutils first: remotes::install_github('Mithril-Metrics/mithril-metrics.github.io', subdir = 'acutils')")
}
needed  <- c("dplyr", "tidyr", "readr", "ggplot2", "quantreg", "yaml", "svglite")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
dir.create("output/cache", recursive = TRUE, showWarnings = FALSE)

# --- Countries: FAO area codes (verified against area names below) -----------------
# Using FAO codes avoids an extra package for ISO3 -> FAO translation.
countries <- tibble::tribble(
  ~iso3, ~fao_code, ~fao_name,
  "VNM", 237, "Viet Nam",
  "IDN", 101, "Indonesia",
  "PHL", 171, "Philippines",
  "THA", 216, "Thailand",
  "MYS", 131, "Malaysia",
  "KHM", 115, "Cambodia"
)

# --- FAOSTAT QCL: rice yield, area, production, 1961-2024 ----------------------------
# Downloads the whole QCL bulk file once (~33 MB), then filters. Re-runs use the cache.
rice <- get_faostat(
  domain   = "QCL",
  items    = "Rice",                                   # item code 27 (formerly "Rice, paddy")
  elements = c("Yield", "Area harvested", "Production"),
  areas    = as.character(countries$fao_code),
  years    = 1961:2024
)
src_fao <- attr(rice, "source")

# Sanity: the codes must map to the expected countries, all three elements present
found <- distinct(rice, area_code, area)
chk <- countries |> left_join(found, by = c("fao_code" = "area_code"))
print(chk)
stopifnot("FAO area codes do not match expected countries" =
            all(!is.na(chk$area)) &&
              all(mapply(function(n, a) grepl(substr(n, 1, 5), a, fixed = TRUE), chk$fao_name, chk$area)))
stopifnot(setequal(unique(rice$element), c("Yield", "Area harvested", "Production")))
cat("\nFAOSTAT rows:", nrow(rice), "| years:", paste(range(rice$year), collapse = "-"),
    "| units:", paste(unique(paste(rice$element, rice$unit, sep = " = ")), collapse = "; "), "\n")
cat("FAOSTAT dataset version (DateUpdate):", src_fao$version, "\n")
print(table(rice$element, rice$flag, useNA = "ifany"))

# --- NOAA CPC Oceanic Niño Index -------------------------------------------------------
oni_url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
oni_path <- cached_fetch(
  "noaa-oni", "txt",
  fetch = function(p) utils::download.file(oni_url, p, mode = "wb", quiet = TRUE),
  source = "NOAA CPC Oceanic Nino Index", url = oni_url
)
oni <- utils::read.table(oni_path, header = TRUE, stringsAsFactors = FALSE)
stopifnot(identical(names(oni), c("SEAS", "YR", "TOTAL", "ANOM")))
cat("\nONI rows:", nrow(oni), "| from", oni$SEAS[1], oni$YR[1], "to",
    oni$SEAS[nrow(oni)], oni$YR[nrow(oni)], "| latest anomaly:", oni$ANOM[nrow(oni)], "\n")

# --- UCDP/PRIO Armed Conflict Dataset v26.1 (1946-2025) -----------------------------
# Used in the "second look" (03b) to flag country-years with major armed conflict
# (intensity_level 2 = at least 1,000 battle-related deaths in the year).
# Note: `gwno_loc` codes the country whose GOVERNMENT is contested, not the battlefield.
ucdp_url <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-261-csv.zip"
ucdp_path <- cached_fetch(
  "ucdp-prio-acd-261", "zip",
  fetch = function(p) utils::download.file(ucdp_url, p, mode = "wb", quiet = TRUE),
  source = "UCDP/PRIO Armed Conflict Dataset v26.1", url = ucdp_url, version = "26.1 (1946-2025)"
)
ucdp_csv <- utils::unzip(ucdp_path, list = TRUE)$Name
ucdp_csv <- ucdp_csv[grepl("\\.csv$", ucdp_csv)][1]
ucdp <- readr::read_csv(unz(ucdp_path, ucdp_csv), show_col_types = FALSE,
                        col_types = readr::cols(.default = "c"))
stopifnot(all(c("conflict_id", "location", "gwno_loc", "year", "intensity_level") %in% names(ucdp)))
cat("\nUCDP rows:", nrow(ucdp), "| years:", paste(range(as.integer(ucdp$year)), collapse = "-"), "\n")

saveRDS(list(rice = rice, oni = oni, countries = countries, ucdp = ucdp,
             fao_version = src_fao$version, fao_accessed = as.character(src_fao$accessed),
             oni_accessed = as.character(attr(oni_path, "accessed"))),
        "output/cache/raw.rds")

message("Access log (for 00_meta.yml -> sources):")
print(access_log())
