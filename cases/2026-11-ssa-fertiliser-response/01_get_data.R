# 01_get_data.R — fetch and cache raw data ------------------------------------
# Case: 2026-11-ssa-fertiliser-response (local folder: AgEcon/Issue 2/)
# Output: data_raw/<source>_<date>.* plus data_raw/_access_log.csv, output/cache/raw.rds
# Everything is written inside this folder. Expected size of data_raw/: ~45 MB.

stopifnot("Run this script from the issue folder (AgEcon/Issue 2)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw", timeout = 600)

if (!requireNamespace("acutils", quietly = TRUE)) {
  stop("Install acutils first: remotes::install_github('Mithril-Metrics/mithril-metrics.github.io', subdir = 'acutils')")
}
needed  <- c("dplyr", "tidyr", "readr", "readxl", "ggplot2", "fixest", "yaml", "svglite")
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

# ---- Countries ----
# FAOSTAT area names. MAFAP is matched on the same names in 02_clean.R.
countries <- tibble::tribble(
  ~iso3, ~fao_name,
  "MWI", "Malawi",
  "KEN", "Kenya",
  "ETH", "Ethiopia",
  "TZA", "United Republic of Tanzania",
  "ZMB", "Zambia",
  "ZWE", "Zimbabwe",
  "MOZ", "Mozambique",
  "UGA", "Uganda",
  "NGA", "Nigeria",
  "GHA", "Ghana",
  "BFA", "Burkina Faso",
  "MLI", "Mali",
  "BEN", "Benin",
  "RWA", "Rwanda"
)

check_areas <- function(d, label) {
  found <- unique(d$area)
  miss <- setdiff(countries$fao_name, found)
  if (length(miss)) stop(label, ": countries not found: ", paste(miss, collapse = ", "))
  cat(label, ": rows", nrow(d), "| years", paste(range(d$year), collapse = "-"),
      "| units:", paste(unique(paste(d$element, d$unit, sep = " = ")), collapse = "; "), "\n")
}

# ---- FAOSTAT: nitrogen use (RFN) and maize (QCL) ----
rfn <- get_faostat("RFN", items = "Nutrient nitrogen N (total)",
                   elements = c("Agricultural Use", "Use per area of cropland"),
                   areas = countries$fao_name, years = 1990:2024)
check_areas(rfn, "RFN nitrogen")

maize <- get_faostat("QCL", items = "Maize (corn)",
                     elements = c("Yield", "Area harvested", "Production"),
                     areas = countries$fao_name, years = 1990:2024)
check_areas(maize, "QCL maize")

# ---- World Bank Pink Sheet: urea and maize, monthly ----
pink <- get_pinksheet(commodities = c("Urea", "Maize"))
cat("Pink Sheet:", attr(pink, "source")$version, "| series:",
    paste(unique(paste(pink$commodity, pink$unit)), collapse = "; "),
    "| last month:", format(max(pink$date[!is.na(pink$price)])), "\n")
stopifnot("Expected exactly one urea and one maize series" =
            setequal(unique(pink$commodity), c("Urea", "Maize")))

# ---- NOAA CPC Oceanic Niño Index ----
oni_url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
oni_path <- cached_fetch(
  "noaa-oni", "txt",
  fetch = function(p) utils::download.file(oni_url, p, mode = "wb", quiet = TRUE),
  source = "NOAA CPC Oceanic Nino Index", url = oni_url
)
oni <- utils::read.table(oni_path, header = TRUE, stringsAsFactors = FALSE)
stopifnot(identical(names(oni), c("SEAS", "YR", "TOTAL", "ANOM")))
cat("ONI: from", oni$SEAS[1], oni$YR[1], "to", oni$SEAS[nrow(oni)], oni$YR[nrow(oni)], "\n")

# ---- FAO MAFAP public expenditure (one wide sheet, March 2026 revision) ----
mafap_url <- paste0("https://www.fao.org/media/docs/mafaplibraries/default-document-library/",
                    "public-expenditure-dataset_fao-mafap-programme_march2026rev.xlsx")
mafap_path <- cached_fetch(
  "fao-mafap-pe", "xlsx",
  fetch = function(p) utils::download.file(mafap_url, p, mode = "wb", quiet = TRUE),
  source = "FAO MAFAP public expenditure bulk file", url = mafap_url, version = "March 2026 revision"
)
mafap <- readxl::read_excel(mafap_path, sheet = 1, col_types = "text")
need_cols <- c("Country", "Year", "Budgeted/actual", "Donor/national/total", "B1. Variable inputs")
stopifnot("MAFAP layout changed" = all(need_cols %in% names(mafap)))
cat("MAFAP: rows", nrow(mafap), "| countries", length(unique(mafap$Country)),
    "| 'Amounts expressed in':", paste(unique(mafap$`Amounts expressed in`), collapse = "; "), "\n")

saveRDS(list(rfn = rfn, maize = maize, pink = pink, oni = oni, mafap = mafap, countries = countries,
             versions = list(rfn = attr(rfn, "source")$version,
                             qcl = attr(maize, "source")$version,
                             pink = attr(pink, "source")$version,
                             mafap = "March 2026 revision"),
             accessed = as.character(Sys.Date())),
        "output/cache/raw.rds")

message("Access log (for 00_meta.yml -> sources):")
print(access_log())
