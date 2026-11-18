#' Panel diagnostics: N, T, balance, gaps, duplicates and missing shares
#'
#' Prints a compact report and returns the numbers invisibly. Run it after every
#' reshape: unbalanced panels, duplicated unit-periods and silent NAs are the most
#' common reasons a fixed-effects or frontier estimate is wrong.
#'
#' @param df A data frame.
#' @param id Character vector of the column(s) identifying the unit (e.g. `"iso3"`,
#'   or `c("region", "farm_type")`).
#' @param time Name of the time column (numeric year, integer, or Date).
#' @param vars Columns for NA shares; default all columns except `id` and `time`.
#' @param quiet Suppress printing.
#' @return Invisibly, a list of class `panel_check` with elements `n_units`,
#'   `n_periods`, `n_obs`, `time_range`, `t_per_unit` (summary), `balanced`,
#'   `n_duplicates`, `duplicates` (data frame of offending keys), `units_with_gaps`,
#'   and `na_share` (named numeric).
#' @examples
#' df <- data.frame(id = rep(c("A", "B"), each = 3), year = rep(2001:2003, 2),
#'                  y = c(1, NA, 3, 4, 5, 6))
#' check_panel(df, id = "id", time = "year")
#' @export
check_panel <- function(df, id, time, vars = NULL, quiet = FALSE) {
  stopifnot(is.data.frame(df), is.character(id), length(time) == 1)
  miss <- setdiff(c(id, time), names(df))
  if (length(miss)) stop("Columns not in data: ", paste(miss, collapse = ", "), call. = FALSE)
  df <- as.data.frame(df)
  vars <- vars %||% setdiff(names(df), c(id, time))

  unit_key <- if (length(id) == 1) as.character(df[[id]]) else
    do.call(paste, c(lapply(df[id], as.character), sep = " | "))
  key <- paste(unit_key, df[[time]], sep = " @ ")
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  dups <- unique(df[dup, c(id, time), drop = FALSE])

  units <- unique(unit_key)
  periods <- sort(unique(df[[time]]))
  t_unit <- tapply(df[[time]], unit_key, function(x) length(unique(x)))
  balanced <- all(t_unit == length(periods)) && !any(dup)

  # Gaps: a unit observed at t and t+k (k > 1 step) but not in between,
  # relative to the overall grid of observed periods.
  gaps <- vapply(split(df[[time]], unit_key), function(x) {
    pos <- match(sort(unique(x)), periods)
    length(pos) > 1 && any(diff(pos) > 1)
  }, logical(1))

  na_share <- vapply(vars, function(v) mean(is.na(df[[v]])), numeric(1))

  out <- structure(list(
    n_units = length(units), n_periods = length(periods), n_obs = nrow(df),
    time_range = range(periods), t_per_unit = summary(as.numeric(t_unit)),
    balanced = balanced, n_duplicates = sum(duplicated(key)), duplicates = dups,
    units_with_gaps = names(gaps)[gaps], na_share = na_share
  ), class = "panel_check")
  if (!quiet) print(out)
  invisible(out)
}

#' @export
print.panel_check <- function(x, ...) {
  cat("Panel check\n")
  cat(sprintf("  Units (N): %d   Periods (T): %d   Obs: %d   Time: %s to %s\n",
              x$n_units, x$n_periods, x$n_obs, format(x$time_range[1]), format(x$time_range[2])))
  cat(sprintf("  Periods per unit: min %g, median %g, max %g\n",
              x$t_per_unit[["Min."]], x$t_per_unit[["Median"]], x$t_per_unit[["Max."]]))
  cat("  Balanced:", if (x$balanced) "yes" else "NO", "\n")
  if (x$n_duplicates > 0) {
    cat(sprintf("  DUPLICATES: %d extra rows on %d unit-period keys, e.g.:\n",
                x$n_duplicates, nrow(x$duplicates)))
    print(utils::head(x$duplicates, 5), row.names = FALSE)
  } else {
    cat("  Duplicates: none\n")
  }
  if (length(x$units_with_gaps)) {
    cat(sprintf("  Units with internal gaps: %d (%s%s)\n", length(x$units_with_gaps),
                paste(utils::head(x$units_with_gaps, 5), collapse = ", "),
                if (length(x$units_with_gaps) > 5) ", ..." else ""))
  }
  if (length(x$na_share)) {
    cat("  NA share by variable:\n")
    s <- sort(x$na_share, decreasing = TRUE)
    w <- max(nchar(names(s)))
    for (v in names(s)) {
      flag <- if (s[[v]] > 0.3) "  <-- high" else ""
      cat(sprintf("    %-*s %5.1f%%%s\n", w, v, 100 * s[[v]], flag))
    }
  }
  invisible(x)
}
