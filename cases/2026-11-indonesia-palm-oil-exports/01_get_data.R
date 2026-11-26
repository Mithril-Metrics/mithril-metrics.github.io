# 01_get_data.R — FPMA monthly prices and the IDR/USD rate, cached in data_raw/
# Case: 2026-11-indonesia-palm-oil-exports (local folder: AgEcon/Issue 6/)
# Output: data_raw/fpma-*_<date>.json, data_raw/_access_log.csv. Expected size ~5 MB.

stopifnot("Run this script from the issue folder (AgEcon/Issue 6)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw", timeout = 600)

if (!requireNamespace("acutils", quietly = TRUE)) {
  stop("Install acutils first: remotes::install_github('Mithril-Metrics/mithril-metrics.github.io', subdir = 'acutils')")
}
needed <- c("dplyr", "tidyr", "stringr", "jsonlite", "curl", "ggplot2", "sandwich", "lmtest", "yaml", "svglite")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(jsonlite)
})

# FPMA has no documented API; these are the endpoints behind the FPMA tool (checked 2026-11-25)
api <- "https://fpma.fao.org/giews/v4/global/price_module/api/v1/"
series <- c(
  idn_vegoil = "c0b2e88d-8010-4f45-b989-fdf1eb126c10",
  idn_flour = "7819292a-caeb-4e2b-8d8a-2fbf4eeadd02",
  world_cpo = "fb4e1fc4-965d-4bc9-8092-6ccfb4923cbc",
  world_wheat = "8a111fb0-79f8-4237-bde5-504373d20dfc"
)

fetch_json <- function(name, url) {
  cached_fetch(
    name = paste0("fpma_", name),
    ext = "json",
    fetch = function(path) curl::curl_download(url, path),
    source = "FAO GIEWS FPMA",
    url = url
  )
}

# ---- Prices ----
paths <- vapply(names(series), function(s) {
  url <- paste0(api, "FpmaSeriePrice/?format=json&periodicity=monthly&uuid__in=", series[[s]])
  fetch_json(s, url)
}, character(1))

# ---- Exchange rate ----
# The filter is ignored by some versions of the API, so 02_clean.R filters IDR again
fx_path <- fetch_json("fxrate_idr", paste0(api, "FxRate/?format=json&nominal_currency_code=IDR"))

# ---- Sanity checks ----
for (s in names(paths)) {
  pts <- fromJSON(paths[[s]])$results$datapoints[[1]]
  d <- as.Date(pts$date)
  cat(sprintf("%-12s %3d months  %s to %s  NA share %.2f\n",
              s, nrow(pts), min(d), max(d), mean(is.na(pts$price_value))))
}
fx <- fromJSON(fx_path)$results
fx <- fx[fx$nominal_currency_code == "IDR" & fx$reference_currency_code == "USD", ]
cat(sprintf("fx IDR/USD    %3d months  %s to %s\n", nrow(fx), min(fx$date), max(fx$date)))

print(access_log())
