# Eurostat and World Bank WDI ----------------------------------------------------

#' Get a Eurostat table with caching and labels
#'
#' Thin wrapper on [eurostat::get_eurostat()] (package >= 4.0, dissemination API).
#' The filtered result is cached as an `.rds` with the access date; labels from
#' [eurostat::label_eurostat()] are joined as `<dim>_label` columns next to the codes.
#'
#' Note: this masks `eurostat::get_eurostat()` if both packages are attached; call
#' `acutils::get_eurostat()` explicitly in that case.
#'
#' @param code Eurostat dataset code, e.g. `"aact_eaa01"`, `"apri_pi15_inq"`, `"aei_pr_gnb"`.
#' @param filters Named list of dimension filters, e.g.
#'   `list(geo = c("DE", "FR"), itm_newa = "20000", unit = "MIO_EUR")`. `NULL` = full table.
#' @param labels Add `<dim>_label` columns.
#' @param time_format Passed to `eurostat::get_eurostat()`; `"num"` gives numeric years.
#' @inheritParams cached_fetch
#' @return Tibble: dimension codes, `<dim>_label`, `time`, `values`. Attribute `source`
#'   includes the Eurostat "last update of data" date when available.
#' @section Pitfalls:
#' * Flags (p = provisional, e = estimated, b = break) are dropped unless you call
#'   eurostat directly with `keepFlags = TRUE`.
#' * Price indices: check the base year in `unit` (e.g. I15 = 2015 = 100).
#' * Quarterly tables return `time` as decimal years with `time_format = "num"`.
#' @export
get_eurostat <- function(code, filters = NULL, labels = TRUE, time_format = "num",
                         dir = NULL, refresh = FALSE) {
  .need("eurostat", "for get_eurostat()")
  key <- paste0("eurostat_", code,
                if (!is.null(filters)) paste0("_", substr(rlang::hash(filters), 1, 8)) else "")
  url <- paste0("https://ec.europa.eu/eurostat/databrowser/view/", code)
  version_fun <- function() {
    tryCatch({
      toc <- eurostat::get_eurostat_toc()
      v <- toc[toc$code == code, , drop = FALSE]
      col <- grep("last.update.of.data", names(v), value = TRUE)[1]
      if (nrow(v) && !is.na(col)) as.character(v[[col]][1]) else NA_character_
    }, error = function(e) NA_character_)
  }
  path <- cached_fetch(key, "rds", function(p) {
    args <- list(id = code, time_format = time_format, cache = FALSE)
    if (!is.null(filters)) args$filters <- filters
    raw <- do.call(eurostat::get_eurostat, args)
    if (is.null(raw) || nrow(raw) == 0) stop("Eurostat returned no rows for ", code, call. = FALSE)
    if ("TIME_PERIOD" %in% names(raw)) names(raw)[names(raw) == "TIME_PERIOD"] <- "time"
    if (labels) {
      lab <- eurostat::label_eurostat(raw, fix_duplicated = TRUE)
      dims <- setdiff(names(raw), c("time", "values", "flags", "freq"))
      for (d in intersect(dims, names(lab))) raw[[paste0(d, "_label")]] <- as.character(lab[[d]])
    }
    saveRDS(tibble::as_tibble(raw), p)
  }, source = paste0("Eurostat ", code), url = url, version = version_fun,
  dir = dir, refresh = refresh)
  df <- readRDS(path)
  log <- access_log(dir)
  v <- utils::tail(log$version[log$file == basename(path)], 1)
  .with_source(df, dataset = paste0("Eurostat ", code), version = v %||% NA,
               accessed = attr(path, "accessed"), url = url)
}

#' Get World Development Indicators with caching
#'
#' Wraps [WDI::WDI()].
#'
#' @param indicators Character vector of WDI codes; names become column names,
#'   e.g. `c(ag_va_share = "NV.AGR.TOTL.ZS", rural_pop = "SP.RUR.TOTL.ZS")`.
#' @param countries ISO2/ISO3 codes or `"all"`.
#' @param years Integer vector (min and max are used) or `NULL` for all years.
#' @param extra Add region, income group, lending type (`TRUE` recommended with `"all"`
#'   so that aggregates can be dropped via `region == "Aggregates"`).
#' @inheritParams cached_fetch
#' @return Tibble with `iso2c`, `iso3c`, `country`, `year` and one column per indicator.
#' @export
get_wdi <- function(indicators, countries = "all", years = NULL, extra = FALSE,
                    dir = NULL, refresh = FALSE) {
  .need("WDI", "for get_wdi()")
  start <- if (is.null(years)) 1960 else min(years)
  end   <- if (is.null(years)) as.integer(format(Sys.Date(), "%Y")) else max(years)
  key <- paste0("wdi_", substr(rlang::hash(list(indicators, countries, start, end, extra)), 1, 10))
  path <- cached_fetch(key, "rds", function(p) {
    d <- WDI::WDI(country = countries, indicator = indicators, start = start, end = end,
                  extra = extra)
    if (is.null(d) || nrow(d) == 0) stop("WDI returned no rows.", call. = FALSE)
    saveRDS(tibble::as_tibble(d), p)
  }, source = paste0("World Bank WDI: ", paste(indicators, collapse = ", ")),
  url = "https://api.worldbank.org/v2/", dir = dir, refresh = refresh)
  df <- readRDS(path)
  if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  .with_source(df, dataset = "World Bank World Development Indicators",
               accessed = attr(path, "accessed"))
}
