# 01_get_data.R — fetch and cache raw data ------------------------------------
# Case: 2026-11-support-composition-tfp (local folder: AgEcon/Issue 5/)
# Output: data_raw/<source>_<date>.* plus data_raw/_access_log.csv, output/cache/raw.rds
# Everything is written inside this folder. Expected size of data_raw/: ~30 MB.

stopifnot("Run this script from the issue folder (AgEcon/Issue 5)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw", timeout = 600)

if (!requireNamespace("acutils", quietly = TRUE)) {
  stop("Install acutils first: remotes::install_github('Mithril-Metrics/mithril-metrics.github.io', subdir = 'acutils')")
}
needed <- c("dplyr", "tidyr", "readr", "ggplot2", "yaml", "svglite", "sandwich",
            "clubSandwich", "WDI", "ggrepel")
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

# ---- OECD PSE/CSE database (2026 edition) ----
# The API answers HTTP 429 after many quick requests: each table is fetched once, with a
# pause before every new download and one retry after a minute.
oecd_url <- function(flow) {
  paste0("https://sdmx.oecd.org/public/rest/data/OECD.TAD.ARP,DSD_AGR_POLIND@", flow,
         ",/all?startPeriod=1990&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
}
get_oecd <- function(flow) {
  url <- oecd_url(flow)
  path <- cached_fetch(
    paste0("oecd-", flow), "csv",
    fetch = function(p) {
      Sys.sleep(15)
      ok <- tryCatch(utils::download.file(url, p, mode = "wb", quiet = TRUE), error = function(e) 1)
      if (ok != 0) {
        message(flow, ": first attempt failed, retrying in 60 s")
        Sys.sleep(60)
        utils::download.file(url, p, mode = "wb", quiet = TRUE)
      }
    },
    source = paste("OECD PSE/CSE database,", flow), url = url, version = "2026 edition"
  )
  d <- readr::read_csv(path, show_col_types = FALSE, guess_max = 1e5)
  cat(flow, ": rows", nrow(d), "| units", length(unique(d$REF_AREA)),
      "| years", paste(range(d$TIME_PERIOD), collapse = "-"),
      "| non-missing", round(mean(!is.na(d$OBS_VALUE)), 2), "\n")
  d
}
flows <- c("DF_TSE", "DF_GSSE", "DF_COMGSSE", "DF_COMM", "DF_PSE")
oecd <- lapply(flows, get_oecd)
names(oecd) <- flows

for (f in c("DF_COMGSSE", "DF_COMM")) {
  cat("\n", f, "measures and units:\n")
  print(distinct(oecd[[f]], MEASURE, Measure, UNIT_MEASURE), n = 60)
}
cat("\nDF_COMM commodities:\n")
print(distinct(oecd$DF_COMM, COMMODITY, Commodity), n = 60)
cat("\nDF_PSE measures (first 60):\n")
print(distinct(oecd$DF_PSE, MEASURE, Measure, UNIT_MEASURE), n = 60)

# The OECD total is kept only for the benchmark check in 03_analyse.R
units <- unique(oecd$DF_GSSE$REF_AREA)
countries <- setdiff(units, c("EU", "OECD"))
cat("\nPSE units:", length(units), "| countries outside the EU block:", length(countries), "\n")

# ---- USDA ERS International Agricultural Productivity ----
tfp_all <- get_usda_tfp()
tfp_accessed <- format(attr(tfp_all, "source")$accessed)
tfp <- filter(tfp_all, variable == "TFP_Index", iso3 %in% countries, year >= 1990)
cat("USDA TFP accessed", tfp_accessed, "| countries", length(unique(tfp$iso3)),
    "| years", paste(range(tfp$year), collapse = "-"), "| NA share", round(mean(is.na(tfp$value)), 3), "\n")
stopifnot("TFP missing for some PSE countries" = setequal(unique(tfp$iso3), countries))

# ---- WDI: GDP per capita, PPP, constant international $ ----
wdi <- get_wdi(c(gdppc = "NY.GDP.PCAP.PP.KD"), countries = countries, years = 2000:2004)
cat("WDI GDP per capita: countries", length(unique(wdi$iso3c)), "| NA share",
    round(mean(is.na(wdi$gdppc)), 3), "\n")

# GDP deflators (EUU = European Union aggregate) to compare AKIS spending in real national currency
defl <- get_wdi(c(defl = "NY.GDP.DEFL.ZS"), countries = c(countries, "EUU"), years = 2000:2025)
cat("WDI GDP deflator: units", length(unique(defl$iso3c)), "| last year with data",
    max(defl$year[!is.na(defl$defl)]), "\n")

saveRDS(list(oecd = oecd, tfp = tfp, wdi = wdi, defl = defl, units = units, countries = countries,
             accessed = as.character(Sys.Date())),
        "output/cache/raw.rds")

message("Access log (for 00_meta.yml -> sources):")
print(access_log())
