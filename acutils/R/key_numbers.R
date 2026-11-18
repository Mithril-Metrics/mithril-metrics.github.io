# Key numbers: one CSV feeds the text, figure titles and the LinkedIn post ---------
# Columns: key, value, unit, digits, big_mark, description, source, updated

.kn_cols <- readr::cols(key = "c", value = "d", unit = "c", digits = "i", big_mark = "c",
                        description = "c", source = "c", updated = "c")

#' Key-number store
#'
#' `kn_add()` inserts or replaces a number in `output/key_numbers.csv`; `kn_reset()`
#' empties the file (call it at the top of `03_analyse.R`); `kn_read()` returns the
#' table; `kn()` returns one formatted number for inline use in `report.qmd` and in
#' figure titles. A missing key is an error, so text can never cite a number the
#' analysis did not produce.
#'
#' @param key Identifier, snake_case (e.g. `"tfp_growth_ssa_pp"`).
#' @param value Numeric value (store percentages as percentages, e.g. `-12.3`).
#' @param unit In `kn_add()`: unit label (`"%"`, `"pp"`, `"EUR/ha"`, `"t/ha"`, `"countries"`, or `""`).
#'   In `kn()`: `TRUE` appends the stored unit (`"%"` is glued: "12.3%"; others get a space).
#' @param digits Default number of decimals when formatting.
#' @param big_mark Thousands separator (`""` for years).
#' @param description What the number is, in words (used in the brief's table).
#' @param source Dataset and table the number comes from.
#' @param file Path of the CSV.
#' @name key_numbers
NULL

#' @rdname key_numbers
#' @export
kn_reset <- function(file = "output/key_numbers.csv") {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  empty <- tibble::tibble(key = character(), value = numeric(), unit = character(),
                          digits = integer(), big_mark = character(), description = character(),
                          source = character(), updated = character())
  readr::write_csv(empty, file)
  invisible(file)
}

#' @rdname key_numbers
#' @export
kn_read <- function(file = "output/key_numbers.csv") {
  if (!file.exists(file)) stop("Key-number file not found: ", file,
                               ". Run 03_analyse.R first.", call. = FALSE)
  x <- readr::read_csv(file, col_types = .kn_cols, na = "NA", progress = FALSE)
  x$unit[is.na(x$unit)] <- ""
  x$big_mark[is.na(x$big_mark)] <- ""
  x
}

#' @rdname key_numbers
#' @export
kn_add <- function(key, value, unit = "", digits = 1, description = "", source = "",
                   big_mark = ",", file = "output/key_numbers.csv") {
  stopifnot(length(key) == 1, length(value) == 1, is.numeric(value))
  if (!grepl("^[a-z][a-z0-9_]*$", key)) stop("Keys must be snake_case: ", key, call. = FALSE)
  if (!file.exists(file)) kn_reset(file)
  x <- kn_read(file)
  x <- x[x$key != key, , drop = FALSE]
  row <- tibble::tibble(key = key, value = value, unit = unit, digits = as.integer(digits),
                        big_mark = big_mark, description = description, source = source,
                        updated = format(Sys.time(), "%Y-%m-%d %H:%M"))
  x <- dplyr::bind_rows(x, row)
  readr::write_csv(x, file, na = "NA")
  invisible(row)
}

#' @rdname key_numbers
#' @param digits_override Decimals to use instead of the stored `digits`.
#' @param sign Show a leading "+" for positive values.
#' @param abs Format the absolute value (for "fell by 12%" phrasing).
#' @export
kn <- function(key, file = "output/key_numbers.csv", unit = FALSE, digits_override = NULL,
               sign = FALSE, abs = FALSE) {
  x <- kn_read(file)
  r <- x[x$key == key, , drop = FALSE]
  if (nrow(r) == 0) stop("Key '", key, "' not in ", file,
                         ". Add it with kn_add() in 03_analyse.R.", call. = FALSE)
  v <- r$value[1]
  if (is.na(v)) return("n/a")
  d <- digits_override %||% r$digits[1]
  if (abs) v <- base::abs(v)
  s <- formatC(round(v, d), format = "f", digits = d, big.mark = r$big_mark[1])
  if (sign && v > 0) s <- paste0("+", s)
  if (isTRUE(unit) && nzchar(r$unit[1])) {
    s <- if (r$unit[1] == "%") paste0(s, "%") else paste(s, r$unit[1])
  }
  s
}

#' Write sessionInfo() to the output folder
#'
#' @param dir Output directory.
#' @export
write_session_info <- function(dir = "output") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, "sessionInfo.txt")
  writeLines(c(paste("Run:", format(Sys.time())), utils::capture.output(utils::sessionInfo())), f)
  invisible(f)
}
