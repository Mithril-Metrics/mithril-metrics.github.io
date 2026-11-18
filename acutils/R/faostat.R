# FAOSTAT ------------------------------------------------------------------------
# Uses the FAOSTAT bulk service (verified 2026-11-18):
#   catalogue  https://bulks-faostat.fao.org/production/datasets_E.json
#   files      https://bulks-faostat.fao.org/production/<Domain>_E_All_Data_(Normalized).zip
# The bulk files are the full domain in long ("normalized") format. FAO launched a new
# API developer portal in April 2026 (fenixservices.fao.org/faostat/api/v1); the bulk
# route is used here because it needs no token and is stable for whole domains.

.faostat_catalog_url <- "https://bulks-faostat.fao.org/production/datasets_E.json"

#' FAOSTAT bulk-download catalogue
#'
#' @inheritParams cached_fetch
#' @return Tibble with dataset_code, dataset_name, topic, date_update, file_location, ...
#' @export
faostat_catalog <- function(dir = NULL, refresh = FALSE) {
  url <- getOption("acutils.faostat_catalog_url", .faostat_catalog_url)
  path <- cached_fetch("faostat_catalog", "json", function(p) .download(url, p, quiet = TRUE),
                       source = "FAOSTAT bulk catalogue", url = url, dir = dir, refresh = refresh)
  js <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  cat_df <- tibble::as_tibble(js$Datasets$Dataset)
  names(cat_df) <- .clean_names(gsub("([a-z])([A-Z])", "\\1_\\2", names(cat_df)))
  cat_df
}

#' Get a FAOSTAT domain (bulk file), filtered and tidy
#'
#' Downloads the whole domain once (cached with the access date), then filters.
#'
#' @param domain FAOSTAT dataset code, e.g. `"QCL"` (crops and livestock), `"RFN"`
#'   (fertilisers by nutrient), `"RL"` (land use), `"PP"` (producer prices), `"FBS"`.
#'   See [faostat_catalog()].
#' @param items Item names (case-insensitive) or item codes; `NULL` keeps all.
#' @param areas Area names, FAO area codes, M49 codes, or ISO3 codes (ISO3 needs the
#'   `countrycode` package); `NULL` keeps all.
#' @param years Integer vector of years; `NULL` keeps all.
#' @param elements Element names (e.g. `"Yield"`, `"Area harvested"`) or codes; `NULL` keeps all.
#' @inheritParams cached_fetch
#' @return Long tibble with snake_case columns (`area_code`, `area`, `item_code`, `item`,
#'   `element`, `year`, `unit`, `value`, `flag`, ...) plus `is_aggregate`
#'   (FAO area codes >= 5000 are regional aggregates). Attribute `source` holds the
#'   dataset name, FAO `DateUpdate` and access date.
#' @section Pitfalls:
#' * Aggregates (World, regions) sit in the same file: drop `is_aggregate` before panels.
#' * China: area code 41 = mainland, 351 = China incl. Taiwan, Hong Kong, Macao. Do not sum.
#' * Units change across elements (yield in kg/ha since 2023 releases, earlier hg/ha): read `unit`.
#' * Values flagged "E"/"I" are estimates/imputations; check `flag` before using tails.
#' @examples
#' \dontrun{
#' wheat <- get_faostat("QCL", items = "Wheat", elements = "Yield",
#'                      areas = c("ETH", "KEN"), years = 2000:2024)
#' }
#' @export
get_faostat <- function(domain, items = NULL, areas = NULL, years = NULL,
                        elements = NULL, dir = NULL, refresh = FALSE) {
  domain <- toupper(domain)
  catalog <- faostat_catalog(dir = dir, refresh = refresh)
  entry <- catalog[toupper(catalog$dataset_code) == domain, , drop = FALSE]
  if (nrow(entry) == 0) {
    stop("Domain '", domain, "' not in the FAOSTAT bulk catalogue. See faostat_catalog().",
         call. = FALSE)
  }
  url <- entry$file_location[1]
  path <- cached_fetch(paste0("faostat_", domain), "zip",
                       function(p) .download(url, p),
                       source = paste0("FAOSTAT ", domain, ": ", entry$dataset_name[1]),
                       url = url, version = entry$date_update[1], dir = dir, refresh = refresh)
  df <- .read_faostat_zip(path)
  df <- .filter_faostat(df, items = items, areas = areas, years = years, elements = elements)
  .with_source(df, dataset = paste0("FAOSTAT ", domain, ": ", entry$dataset_name[1]),
               version = entry$date_update[1], accessed = attr(path, "accessed"), url = url)
}

.read_faostat_zip <- function(path) {
  files <- utils::unzip(path, list = TRUE)$Name
  main <- files[grepl("All_Data", files) & grepl("\\.csv$", files) &
                  !grepl("AreaCodes|ItemCodes|Elements|Flags|Symboles|Currencys", files)]
  if (length(main) == 0) stop("No data CSV found in ", basename(path), call. = FALSE)
  ex <- utils::unzip(path, files = main[1], exdir = tempdir(), overwrite = TRUE)
  df <- readr::read_csv(ex, col_types = readr::cols(.default = "c"), progress = FALSE,
                        locale = readr::locale(encoding = "UTF-8"))
  area_col <- if ("Area" %in% names(df)) "Area" else names(df)[1]
  if (any(!validUTF8(stats::na.omit(df[[area_col]])))) {
    df <- readr::read_csv(ex, col_types = readr::cols(.default = "c"), progress = FALSE,
                          locale = readr::locale(encoding = "latin1"))
  }
  unlink(ex)
  .tidy_faostat(df)
}

.tidy_faostat <- function(df) {
  names(df) <- .clean_names(names(df))
  # M49 and CPC codes carry a leading apostrophe in the bulk files ("'004")
  for (v in intersect(c("area_code_m49", "item_code_cpc"), names(df))) {
    df[[v]] <- sub("^'", "", df[[v]])
  }
  num <- intersect(c("area_code", "item_code", "element_code", "year_code", "year", "value"), names(df))
  for (v in num) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  if ("year" %in% names(df)) df$year <- as.integer(df$year)
  if ("area_code" %in% names(df)) df$is_aggregate <- df$area_code >= 5000
  tibble::as_tibble(df)
}

.filter_faostat <- function(df, items = NULL, areas = NULL, years = NULL, elements = NULL) {
  match_any <- function(x_name, x_code, wanted) {
    w <- tolower(as.character(wanted))
    tolower(x_name) %in% w | as.character(x_code) %in% w
  }
  warn_unmatched <- function(wanted, found_name, found_code, what) {
    w <- as.character(wanted)
    miss <- w[!(tolower(w) %in% tolower(found_name) | w %in% as.character(found_code))]
    if (length(miss)) warning("FAOSTAT: ", what, " not found: ", paste(miss, collapse = ", "),
                              call. = FALSE)
  }
  if (!is.null(items)) {
    warn_unmatched(items, unique(df$item), unique(df$item_code), "items")
    df <- df[match_any(df$item, df$item_code, items), , drop = FALSE]
  }
  if (!is.null(elements)) {
    warn_unmatched(elements, unique(df$element), unique(df$element_code), "elements")
    df <- df[match_any(df$element, df$element_code, elements), , drop = FALSE]
  }
  if (!is.null(areas)) {
    areas <- as.character(areas)
    iso <- grepl("^[A-Z]{3}$", areas)
    if (any(iso)) {
      .need("countrycode", "to translate ISO3 codes to FAO area codes")
      fao <- suppressWarnings(countrycode::countrycode(areas[iso], "iso3c", "fao"))
      if (anyNA(fao)) warning("No FAO code for: ", paste(areas[iso][is.na(fao)], collapse = ", "),
                              call. = FALSE)
      areas <- c(areas[!iso], as.character(fao[!is.na(fao)]))
    }
    m49 <- if ("area_code_m49" %in% names(df)) df$area_code_m49 else NULL
    keep <- tolower(df$area) %in% tolower(areas) | as.character(df$area_code) %in% areas
    if (!is.null(m49)) keep <- keep | m49 %in% areas
    df <- df[keep, , drop = FALSE]
  }
  if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  df
}
