#' acutils: helpers for the Mithril Metrics case studies
#'
#' Fetchers cache raw downloads in `getOption("acutils.data_dir", "data_raw")` with
#' the access date in the file name and log each download in `_access_log.csv`.
#'
#' Options:
#' * `acutils.data_dir`: cache directory (default `"data_raw"`).
#' * `acutils.timeout`: download timeout in seconds (default 300).
#' * `acutils.usda_tfp_url`, `acutils.pinksheet_url`, `acutils.faostat_catalog_url`:
#'   override source URLs after a provider moves a file.
#'
#' @keywords internal
"_PACKAGE"
