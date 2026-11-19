# Caching, downloading and access logging -------------------------------------
# Every fetcher stores its raw download as data_raw/<name>_<YYYY-MM-DD>.<ext> and
# appends a row to data_raw/_access_log.csv. Re-runs reuse the newest cached file
# unless refresh = TRUE, so a case is reproducible offline once fetched.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Directory used to cache raw downloads
#'
#' Resolves the cache directory: the `dir` argument if given, else
#' `getOption("acutils.data_dir")`, else `"data_raw"` (relative to the working
#' directory, i.e. the case folder).
#'
#' @param dir Optional directory.
#' @return A directory path (created if missing).
#' @export
ac_data_dir <- function(dir = NULL) {
  d <- dir %||% getOption("acutils.data_dir", "data_raw")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

.cache_path <- function(name, ext, dir, date = Sys.Date()) {
  file.path(dir, paste0(.safe_name(name), "_", format(as.Date(date), "%Y-%m-%d"), ".", ext))
}

.latest_cache <- function(name, ext, dir) {
  pat <- paste0("^", .safe_name(name), "_(\\d{4}-\\d{2}-\\d{2})\\.", ext, "$")
  f <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  dates <- as.Date(sub(pat, "\\1", basename(f)))
  f[which.max(dates)]
}

.cache_date <- function(path) {
  as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.[^.]+$", "\\1", basename(path)))
}

.download <- function(url, dest, quiet = FALSE) {
  tmp <- paste0(dest, ".part")
  h <- curl::new_handle(
    timeout = getOption("acutils.timeout", 300),
    connecttimeout = 30,
    useragent = "acutils (R; Mithril Metrics)"
  )
  res <- tryCatch(
    curl::curl_download(url, tmp, handle = h, quiet = quiet || getOption("acutils.quiet", FALSE)),
    error = function(e) {
      if (file.exists(tmp)) unlink(tmp)
      stop("Download failed for ", url, "\n  ", conditionMessage(e),
           "\n  Check that the URL is still current (providers move files).", call. = FALSE)
    }
  )
  file.rename(tmp, dest)
  invisible(dest)
}

.log_access <- function(dir, source, name, url, file, version = NA_character_) {
  log_file <- file.path(dir, "_access_log.csv")
  row <- tibble::tibble(
    source = source, name = name, url = url %||% NA_character_,
    file = basename(file), accessed = format(Sys.Date()),
    version = as.character(version %||% NA_character_)
  )
  readr::write_csv(row, log_file, append = file.exists(log_file))
  invisible(row)
}

#' Fetch a file through the dated cache
#'
#' Low-level helper used by all `get_*()` functions. If a cached copy exists and
#' `refresh = FALSE`, it is returned; otherwise `fetch(path)` is called to create it.
#'
#' @param name Cache name (sanitised), e.g. `"faostat_QCL"`.
#' @param ext File extension without dot.
#' @param fetch Function taking the destination path and writing the file there.
#' @param source,url,version Recorded in `_access_log.csv`.
#' @param dir Cache directory, see [ac_data_dir()].
#' @param refresh Force a new download.
#' @return Path of the cached file, with attribute `accessed` (Date).
#' @export
cached_fetch <- function(name, ext, fetch, source = name, url = NA_character_,
                         version = NA_character_, dir = NULL, refresh = FALSE) {
  dir <- ac_data_dir(dir)
  hit <- if (refresh) NULL else .latest_cache(name, ext, dir)
  if (!is.null(hit)) {
    message("Using cached ", basename(hit), " (accessed ", .cache_date(hit), ").")
    path <- hit
  } else {
    path <- .cache_path(name, ext, dir)
    fetch(path)
    if (!file.exists(path)) stop("Fetch did not create ", path, call. = FALSE)
    v <- if (is.function(version)) version() else version
    .log_access(dir, source, name, url, path, v)
    message("Downloaded ", basename(path), ".")
  }
  attr(path, "accessed") <- .cache_date(path)
  path
}

#' Read the access log of a case
#'
#' @inheritParams ac_data_dir
#' @return A tibble with source, name, url, file, accessed and version (empty if none).
#' @export
access_log <- function(dir = NULL) {
  f <- file.path(ac_data_dir(dir), "_access_log.csv")
  if (!file.exists(f)) {
    return(tibble::tibble(source = character(), name = character(), url = character(),
                          file = character(), accessed = character(), version = character()))
  }
  readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE)
}

.clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

.need <- function(pkg, why) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is needed ", why, ". Install it with install.packages(\"",
         pkg, "\").", call. = FALSE)
  }
}

.with_source <- function(x, ...) {
  attr(x, "source") <- list(...)
  x
}
