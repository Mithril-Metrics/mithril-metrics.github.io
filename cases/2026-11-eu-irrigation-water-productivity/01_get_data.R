# 01_get_data.R — fetch and cache raw data ------------------------------------
# Case: 2026-11-eu-irrigation-water-productivity (local folder: AgEcon/Issue 3/)
# Output: data_raw/<source>_<date>.* plus data_raw/_access_log.csv, output/cache/raw.rds
# Everything is written inside this folder. Expected size of data_raw/: ~40 MB.

stopifnot("Run this script from the issue folder (AgEcon/Issue 3)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw", timeout = 600)

if (!requireNamespace("acutils", quietly = TRUE)) {
  stop("Install acutils first: remotes::install_github('Mithril-Metrics/mithril-metrics.github.io', subdir = 'acutils')")
}
needed  <- c("dplyr", "tidyr", "readr", "ggplot2", "yaml", "svglite", "knitr", "eurostat",
             "countrycode", "jsonlite", "Benchmarking", "ggrepel")
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
# Eurostat uses EL for Greece; FAOSTAT and the World Bank use ISO3.
eu <- tibble::tribble(
  ~geo, ~iso3, ~name,
  "AT", "AUT", "Austria",
  "BE", "BEL", "Belgium",
  "BG", "BGR", "Bulgaria",
  "CY", "CYP", "Cyprus",
  "CZ", "CZE", "Czechia",
  "DE", "DEU", "Germany",
  "DK", "DNK", "Denmark",
  "EE", "EST", "Estonia",
  "EL", "GRC", "Greece",
  "ES", "ESP", "Spain",
  "FI", "FIN", "Finland",
  "FR", "FRA", "France",
  "HR", "HRV", "Croatia",
  "HU", "HUN", "Hungary",
  "IE", "IRL", "Ireland",
  "IT", "ITA", "Italy",
  "LT", "LTU", "Lithuania",
  "LU", "LUX", "Luxembourg",
  "LV", "LVA", "Latvia",
  "MT", "MLT", "Malta",
  "NL", "NLD", "Netherlands",
  "PL", "POL", "Poland",
  "PT", "PRT", "Portugal",
  "RO", "ROU", "Romania",
  "SE", "SWE", "Sweden",
  "SI", "SVN", "Slovenia",
  "SK", "SVK", "Slovakia"
)
yrs <- 2015:2025

show <- function(d, label, unit_col = NULL) {
  cat(label, ": rows", nrow(d), "| countries", n_distinct(d$geo),
      "| years", paste(range(d$time), collapse = "-"),
      "| NA share", round(mean(is.na(d$values)), 3),
      if (!is.null(unit_col)) paste("| units:", paste(unique(d[[unit_col]]), collapse = ", ")), "\n")
}
version_of <- function(d) {
  v <- attr(d, "source")$version
  if (is.null(v)) NA_character_ else as.character(v)
}

# ---- Eurostat ----
# Whole tables, filtered here: the filtered API answers large requests with
# "413 asynchronous" (seen for aact_eaa01 at feasibility).
wat_all <- get_eurostat("env_wat_abs", labels = FALSE)
water <- wat_all |>
  filter(geo %in% eu$geo, wat_proc %in% c("ABS_AGR_IR", "ABS_AGR"), wat_src == "FRW",
         unit == "MIO_M3", time %in% yrs) |>
  select(geo, wat_proc, time, values)
show(water, "Water abstraction (million m3)")

irr_all <- get_eurostat("ef_fsi_irri", labels = FALSE)
irr <- irr_all |>
  filter(geo %in% eu$geo, irr_area == "UAA_IT") |>
  select(geo, unit, time, values)
show(irr, "Irrigated area", "unit")

wei_all <- get_eurostat("sdg_06_60", labels = FALSE)
wei <- wei_all |>
  filter(geo %in% eu$geo, statinfo == "VAL_A", unit == "PC", time %in% yrs) |>
  select(geo, time, values)
show(wei, "WEI+ (%)")

# Crop output. The item dimension is found by its code (10000 = crop output) rather than
# by name: the bulk table does not call it itm_newa. Valuation: basic prices.
eaa <- tryCatch(get_eurostat("aact_eaa01", labels = FALSE), error = function(e) {
  message("aact_eaa01 failed: ", conditionMessage(e))
  NULL
})
crop <- NULL
crop_version <- NA_character_
if (!is.null(eaa)) {
  crop_version <- version_of(eaa)
  dims <- setdiff(names(eaa), c("geo", "time", "values", "freq", "flags"))
  cat("aact_eaa01 dimensions:", paste(dims, collapse = ", "), "\n")
  item_col <- dims[vapply(dims, function(v) "10000" %in% eaa[[v]], logical(1))]
  stopifnot("No aact_eaa01 dimension holds code 10000 (crop output)" = length(item_col) == 1)
  eaa <- eaa[eaa[[item_col]] == "10000" & eaa$geo %in% eu$geo & eaa$time %in% yrs, ]
  for (v in setdiff(dims, item_col)) {
    vals <- unique(eaa[[v]])
    cat("  ", v, ":", paste(utils::head(vals, 12), collapse = ", "), "\n")
    keep <- intersect(c("MIO_EUR", "PROD_BP"), vals)
    if (length(vals) > 1 && length(keep) == 1) eaa <- eaa[eaa[[v]] == keep, ]
  }
  crop <- eaa |>
    transmute(geo, time, values, measure = "EUR million, basic prices", source = "eaa")
  if (anyDuplicated(crop[c("geo", "time")])) {
    message("aact_eaa01: more than one crop-output value per country-year (see above).")
    crop <- NULL
  }
}
if (is.null(crop) || n_distinct(crop$geo[!is.na(crop$values)]) < 20) {
  message("Crop output from aact_eaa01 incomplete: switching to FAOSTAT QV.")
  qv <- get_faostat("QV", areas = eu$iso3, years = yrs)
  qv_item <- unique(qv$item[grepl("^crops", qv$item, ignore.case = TRUE)])
  qv_el <- unique(qv$element[grepl("constant 2014-2016 thousand I\\$", qv$element)])
  cat("QV items found:", paste(qv_item, collapse = " | "), "\nQV elements found:",
      paste(qv_el, collapse = " | "), "\n")
  stopifnot("FAOSTAT QV: expected one crops aggregate and one constant-price element" =
              length(qv_item) == 1, length(qv_el) == 1)
  crop <- qv |>
    filter(item == qv_item, element == qv_el) |>
    mutate(iso3 = countrycode::countrycode(area_code, "fao", "iso3c", warn = FALSE)) |>
    inner_join(eu, by = "iso3") |>
    transmute(geo, time = year, values = value / 1000,
              measure = "million constant 2014-2016 international $", source = "qv")
  crop_version <- version_of(qv)
}
show(crop, "Crop output", "measure")

# ---- FAOSTAT cropland ----
rl <- get_faostat("RL", items = "Cropland", elements = "Area", areas = eu$iso3, years = yrs)
cat("RL cropland: rows", nrow(rl), "| years", paste(range(rl$year), collapse = "-"),
    "| unit", unique(rl$unit), "\n")
cropland <- rl |>
  mutate(iso3 = countrycode::countrycode(area_code, "fao", "iso3c", warn = FALSE)) |>
  inner_join(eu, by = "iso3") |>
  transmute(geo, time = year, values = value)
stopifnot("Cropland missing for some countries" = n_distinct(cropland$geo) == nrow(eu))

# ---- AQUASTAT through the World Bank API (gap fill) ----
# The WDI package hung at feasibility; one direct request per indicator, then one per
# country if the batch request times out.
wb_get <- function(ind) {
  path <- cached_fetch(paste0("wb-", ind), "csv", function(p) {
    one <- function(iso) {
      url <- sprintf("https://api.worldbank.org/v2/country/%s/indicator/%s?format=json&per_page=2000&date=2010:2025",
                     paste(iso, collapse = ";"), ind)
      js <- jsonlite::fromJSON(url)
      if (length(js) < 2 || is.null(js[[2]])) return(NULL)
      tibble::tibble(iso3 = js[[2]]$countryiso3code, year = as.integer(js[[2]]$date),
                     value = as.numeric(js[[2]]$value))
    }
    old <- options(timeout = 90)
    on.exit(options(old))
    d <- tryCatch(one(eu$iso3), error = function(e) NULL)
    if (is.null(d)) {
      message(ind, ": batch request failed, trying one country at a time.")
      d <- bind_rows(lapply(eu$iso3, function(i) tryCatch(one(i), error = function(e) NULL)))
    }
    if (nrow(d) == 0) stop("World Bank API returned nothing for ", ind, call. = FALSE)
    readr::write_csv(d, p)
  }, source = paste0("World Bank API (AQUASTAT): ", ind), url = "https://api.worldbank.org/v2/")
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate(indicator = ind)
}
aqua <- bind_rows(wb_get("ER.H2O.FWAG.ZS"), wb_get("ER.H2O.FWTL.K3"))
print(aqua |>
        filter(!is.na(value)) |>
        group_by(indicator) |>
        summarise(countries = n_distinct(iso3), latest = max(year), .groups = "drop"))

# ---- Save ----
log <- access_log()
raw <- list(
  eu = eu, water = water, irr = irr, wei = wei, crop = crop, cropland = cropland, aqua = aqua,
  versions = list(water = version_of(wat_all), irr = version_of(irr_all), wei = version_of(wei_all),
                  crop = crop_version, cropland = version_of(rl)),
  accessed = max(as.Date(log$accessed))
)
saveRDS(raw, "output/cache/raw.rds")
message("Access log (copy versions/dates into 00_meta.yml -> sources):")
print(log)
