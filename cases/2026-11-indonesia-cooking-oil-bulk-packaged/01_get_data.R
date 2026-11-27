# 01_get_data.R - Bank Indonesia PIHPS cooking-oil prices, cached in data_raw/
# Case: 2026-11-indonesia-cooking-oil-bulk-packaged (local folder: AgEcon/Issue 7/)
# Output: data_raw/pihps-*_<date>.json, data_raw/_access_log.csv. Expected size < 1 MB.

stopifnot("Run this script from the issue folder (AgEcon/Issue 7)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw", timeout = 300)

if (!requireNamespace("acutils", quietly = TRUE)) {
  stop("Install acutils first: remotes::install_github('Mithril-Metrics/mithril-metrics.github.io', subdir = 'acutils')")
}
needed <- c("dplyr", "tidyr", "jsonlite", "curl", "ggplot2", "clubSandwich", "yaml", "readr", "svglite")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
suppressPackageStartupMessages({
  library(acutils)
  library(jsonlite)
})
meta <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")
dec <- meta$decisions

# PIHPS has no documented API; this is the endpoint behind its price tables (checked 2026-11-26).
# price_type_id 1 = traditional markets; tipe_laporan 3 = monthly, 4 = weekly;
# province_id empty = national row plus all provinces, 0 = national row only.
pihps_url <- function(com, start, end, report, province) {
  paste0(dec$api, "?price_type_id=1&comcat_id=", com, "&province_id=", province,
         "&regency_id=&market_id=&tipe_laporan=", report,
         "&start_date=", start, "&end_date=", end)
}

fetch_pihps <- function(oil, report, start, end, province) {
  url <- pihps_url(dec$commodities[[oil]], start, end, report, province)
  freq <- if (report == 4) "weekly" else "monthly"
  path <- cached_fetch(
    name = paste("pihps", oil, freq, sep = "_"),
    ext = "json",
    fetch = function(p) {
      curl::curl_download(url, p)
      Sys.sleep(2)
    },
    source = "Bank Indonesia PIHPS",
    url = url
  )
  path
}

oils <- names(dec$commodities)
weekly <- vapply(oils, fetch_pihps, character(1), report = 4,
                 start = dec$weekly_start, end = dec$weekly_end, province = "")
monthly <- vapply(oils, fetch_pihps, character(1), report = 3,
                  start = dec$monthly_start, end = dec$monthly_end, province = "0")

# ---- Sanity checks ----
for (o in oils) {
  w <- fromJSON(weekly[[o]])$data
  m <- fromJSON(monthly[[o]])$data
  cat(sprintf("%-5s weekly: %2d regions x %2d weeks (%s to %s) | monthly national: %3d months\n",
              o, nrow(w), ncol(w) - 3, names(w)[4], names(w)[ncol(w)], ncol(m) - 3))
}

print(access_log())
