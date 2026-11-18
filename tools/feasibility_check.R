#!/usr/bin/env Rscript
# feasibility_check.R — can this question be answered with this data, today, in 4 hours?
#
# Usage (repository root):  Rscript tools/feasibility_check.R tools/specs/example_usda_tfp.yml
#                           Rscript tools/feasibility_check.R spec.yml --budget 120 --out feasibility_reports
#
# Reads a small YAML spec, fetches the data through acutils (cached in data_raw/), and reports:
#   reachable (Y/N) | latest year available | coverage matrix (units x years) |
#   missing share per variable | GREEN / AMBER / RED verdict (rules below, editable).
# Writes <out>/<spec>_<date>.md and <spec>_<date>_coverage.csv.
#
# Spec fields (see tools/specs/*.yml):
#   source:    faostat | eurostat | wdi | usda_tfp | pinksheet | csv
#   table:     FAOSTAT domain ("QCL"), Eurostat code ("aact_eaa01"), or path for csv
#   units:     countries/regions (ISO3, FAO names, Eurostat geo codes) or commodities (pinksheet)
#   years:     [first, last]   (inclusive range)
#   variables: indicators / USDA variables / Eurostat or FAOSTAT combos (see below)
#   items, elements: FAOSTAT only;  filters: Eurostat only (named list)
#   csv only:  unit_col, year_col, variable_col, value_col  (long file) — or wide: value columns = variables

# ==== RULES: edit freely ======================================================================
RULES <- list(
  budget_seconds   = 120,   # overall time budget for the check
  # GREEN requires ALL of these; AMBER requires the 'amber_' versions; otherwise RED.
  max_missing      = 0.10,  # max share of missing unit-year cells, worst variable
  amber_max_missing = 0.35,
  max_lag_years    = 2,     # latest year with data must be >= current year - lag
  amber_max_lag    = 4,
  min_unit_cover   = 0.90,  # share of requested units with any data
  amber_min_unit_cover = 0.60,
  min_years        = 10,    # years with data (any variable) within the requested window
  amber_min_years  = 5,
  red_if_variable_absent = TRUE   # a requested variable with zero observations => RED
)
# =============================================================================================

suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: Rscript tools/feasibility_check.R <spec.yml> [--budget 120] [--out dir]")
spec_file <- args[1]
opt <- function(name, default) {
  i <- match(paste0("--", name), args)
  if (is.na(i)) default else args[i + 1]
}
RULES$budget_seconds <- as.numeric(opt("budget", RULES$budget_seconds))
out_dir <- opt("out", "feasibility_reports")
options(acutils.data_dir = opt("data", "data_raw"),
        acutils.timeout = RULES$budget_seconds)

spec <- yaml::read_yaml(spec_file, fileEncoding = "UTF-8")
stopifnot("spec needs 'source'" = !is.null(spec$source))
yrs <- if (is.null(spec$years)) NULL else seq(min(unlist(spec$years)), max(unlist(spec$years)))
units_req <- as.character(unlist(spec$units))
t0 <- Sys.time()
elapsed <- function() as.numeric(difftime(Sys.time(), t0, units = "secs"))

# ---- Adapters: each returns a long tibble(unit, year, variable, value) ---------------------
fetch_long <- function(spec) {
  src <- tolower(spec$source)
  switch(src,
    usda_tfp = {
      d <- get_usda_tfp()
      d <- if (length(units_req)) d[d$iso3 %in% units_req | d$country %in% units_req, ] else d[d$is_country, ]
      if (!is.null(spec$variables)) d <- d[d$variable %in% unlist(spec$variables), ]
      transmute(d, unit = iso3, year, variable, value)
    },
    faostat = {
      d <- get_faostat(spec$table, items = unlist(spec$items), elements = unlist(spec$elements),
                       areas = if (length(units_req)) units_req else NULL, years = yrs)
      d <- d[!d$is_aggregate | length(units_req) > 0, ]
      unit_key <- if (length(units_req) && all(grepl("^[A-Z]{3}$", units_req)) &&
                      requireNamespace("countrycode", quietly = TRUE)) {
        countrycode::countrycode(d$area_code, "fao", "iso3c", warn = FALSE)
      } else d$area
      transmute(d, unit = unit_key, year, variable = paste(item, element, sep = " | "), value)
    },
    eurostat = {
      f <- spec$filters %||% list()
      if (length(units_req)) f$geo <- units_req
      d <- get_eurostat(spec$table, filters = if (length(f)) f else NULL, labels = FALSE)
      dims <- setdiff(names(d), c("geo", "time", "values", "freq"))
      d$variable <- if (length(dims)) do.call(paste, c(d[dims], sep = " | ")) else spec$table
      transmute(d, unit = geo, year = as.integer(floor(time)), variable, value = values)
    },
    wdi = {
      ind <- unlist(spec$variables)
      d <- get_wdi(ind, countries = if (length(units_req)) units_req else "all", years = yrs)
      vcols <- intersect(c(names(ind), ind), names(d))
      d |> select(unit = iso3c, year, all_of(vcols)) |>
        pivot_longer(all_of(vcols), names_to = "variable", values_to = "value")
    },
    pinksheet = {
      d <- get_pinksheet(commodities = if (length(units_req)) units_req else NULL)
      # Annual coverage = share of months with a price, kept as value for the matrix
      d |> mutate(year = as.integer(format(date, "%Y"))) |>
        group_by(unit = commodity, year) |>
        summarise(value = if (all(is.na(price))) NA_real_ else mean(price, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(variable = "price (annual mean of monthly)")
    },
    csv = {
      d <- readr::read_csv(spec$table, show_col_types = FALSE, progress = FALSE)
      uc <- spec$unit_col %||% "unit"; yc <- spec$year_col %||% "year"
      if (!is.null(spec$variable_col)) {
        d <- transmute(d, unit = as.character(.data[[uc]]), year = as.integer(.data[[yc]]),
                       variable = as.character(.data[[spec$variable_col]]),
                       value = as.numeric(.data[[spec$value_col %||% "value"]]))
      } else {
        vc <- unlist(spec$variables) %||% setdiff(names(d), c(uc, yc))
        d <- d |> transmute(unit = as.character(.data[[uc]]), year = as.integer(.data[[yc]]),
                            across(all_of(vc), as.numeric)) |>
          pivot_longer(all_of(vc), names_to = "variable", values_to = "value")
      }
      if (length(units_req)) d <- d[d$unit %in% units_req, ]
      if (!is.null(spec$variables) && !is.null(spec$variable_col)) d <- d[d$variable %in% unlist(spec$variables), ]
      d
    },
    stop("Unknown source '", spec$source, "'. Use faostat | eurostat | wdi | usda_tfp | pinksheet | csv.",
         " (ERA5: check by hand; CDS requests queue and do not fit a 2-minute check.)")
  )
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Fetch under a time budget ------------------------------------------------------------------
reachable <- TRUE; err <- NULL
long <- tryCatch({
  setTimeLimit(elapsed = RULES$budget_seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  fetch_long(spec)
}, error = function(e) { reachable <<- FALSE; err <<- conditionMessage(e); NULL })
setTimeLimit(elapsed = Inf)

line <- function(...) cat(..., "\n", sep = "")
md <- character()
say <- function(...) { txt <- paste0(...); md <<- c(md, txt); line(txt) }
spec_name <- tools::file_path_sans_ext(basename(spec_file))
say("# Feasibility check: ", spec$label %||% spec_name)
say("")
say("- Spec: `", spec_file, "` | source: **", spec$source, "** | table: `", spec$table %||% "-", "`")
say("- Run: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " | elapsed: ", round(elapsed(), 1), " s")
say("- Reachable: **", if (reachable) "Y" else "N", "**", if (!reachable) paste0(" — ", err) else "")

if (!reachable || is.null(long) || nrow(long) == 0) {
  if (reachable) say("- The query returned **no rows**: check unit codes, variable names and years.")
  say(""); say("## Verdict: **RED**")
  say("- Data could not be retrieved (budget ", RULES$budget_seconds, " s). Fix access or the spec before planning.")
  dir.create(out_dir, showWarnings = FALSE)
  writeLines(md, file.path(out_dir, paste0(spec_name, "_", Sys.Date(), ".md")))
  quit(status = 0)
}

# ---- Coverage --------------------------------------------------------------------------------------
long <- long |> filter(!is.na(unit))
if (!is.null(yrs)) long <- long |> filter(year %in% yrs)
vars_found <- sort(unique(long$variable))
vars_req   <- as.character(unlist(spec$variables))
units_all  <- if (length(units_req)) units_req else sort(unique(long$unit))
years_all  <- if (!is.null(yrs)) yrs else seq(min(long$year), max(long$year))

grid <- expand_grid(unit = units_all, year = years_all, variable = vars_found) |>
  left_join(long |> group_by(unit, year, variable) |> summarise(value = first(value), .groups = "drop"),
            by = c("unit", "year", "variable"))
obs <- long |> filter(!is.na(value))

latest <- if (nrow(obs)) max(obs$year) else NA
latest_by_var <- obs |> group_by(variable) |> summarise(latest = max(year), .groups = "drop")
miss_by_var <- grid |> group_by(variable) |> summarise(missing_share = mean(is.na(value)), .groups = "drop") |>
  left_join(latest_by_var, by = "variable")
absent <- if (length(vars_req)) setdiff(vars_req, vars_found) else character()
if (length(vars_req) && tolower(spec$source) %in% c("faostat", "eurostat")) absent <- character()

cov <- grid |> group_by(unit, year) |> summarise(share = mean(!is.na(value)), .groups = "drop")
unit_cover  <- mean(units_all %in% unique(obs$unit))
years_with  <- length(unique(obs$year))
worst_miss  <- max(miss_by_var$missing_share)
lag         <- as.integer(format(Sys.Date(), "%Y")) - latest

say("- Latest year available: **", latest, "** (lag ", lag, " years)")
say("- Units with any data: ", sum(units_all %in% obs$unit), "/", length(units_all),
    " | years with data: ", years_with, "/", length(years_all), " | variables found: ", length(vars_found))
if (length(absent)) say("- Requested variables with NO data: ", paste(absent, collapse = ", "))
units_missing <- setdiff(units_all, obs$unit)
if (length(units_missing)) say("- Units with no data: ", paste(utils::head(units_missing, 15), collapse = ", "),
                               if (length(units_missing) > 15) " ..." else "")
say("")
say("## Missing share per variable (requested units x years)")
say("")
say("| Variable | Missing | Latest year |")
say("|---|---:|---:|")
for (i in seq_len(nrow(miss_by_var))) {
  say("| ", miss_by_var$variable[i], " | ", sprintf("%.1f%%", 100 * miss_by_var$missing_share[i]),
      " | ", miss_by_var$latest[i] %||% NA, " |")
}

# Text heat-map: '#' all variables present, '+' some, '.' none
say("")
say("## Coverage matrix (# all variables, + some, . none)")
say("")
yr_lab <- years_all
step <- if (length(yr_lab) > 40) ceiling(length(yr_lab) / 40) else 1
show_years <- yr_lab[seq(1, length(yr_lab), by = step)]
w <- max(nchar(units_all), 4)
say("```")
say(formatC("", width = w), " ", show_years[1], strrep(" ", max(0, length(show_years) - 8)),
    show_years[length(show_years)])
for (u in units_all) {
  s <- cov$share[cov$unit == u][match(show_years, cov$year[cov$unit == u])]
  ch <- ifelse(is.na(s) | s == 0, ".", ifelse(s == 1, "#", "+"))
  say(formatC(u, width = w, flag = "-"), " ", paste(ch, collapse = ""))
}
if (step > 1) say("(every ", step, ". year shown)")
say("```")

# ---- Verdict ------------------------------------------------------------------------------------------
reasons <- character()
grade <- function(val, green_ok, amber_ok, label) {
  if (green_ok) return(3)
  reasons <<- c(reasons, label)
  if (amber_ok) 2 else 1
}
g <- c(
  grade(worst_miss, worst_miss <= RULES$max_missing, worst_miss <= RULES$amber_max_missing,
        sprintf("worst variable is %.0f%% missing", 100 * worst_miss)),
  grade(lag, lag <= RULES$max_lag_years, lag <= RULES$amber_max_lag,
        sprintf("latest year %s (lag %s)", latest, lag)),
  grade(unit_cover, unit_cover >= RULES$min_unit_cover, unit_cover >= RULES$amber_min_unit_cover,
        sprintf("only %.0f%% of units have data", 100 * unit_cover)),
  grade(years_with, years_with >= RULES$min_years, years_with >= RULES$amber_min_years,
        sprintf("only %d years with data", years_with))
)
if (RULES$red_if_variable_absent && length(absent)) {
  g <- c(g, 1); reasons <- c(reasons, "requested variable(s) absent")
}
verdict <- c("RED", "AMBER", "GREEN")[min(g)]
say("")
say("## Verdict: **", verdict, "**")
if (length(reasons)) for (r in reasons) say("- ", r)
if (verdict == "AMBER") say("- Narrow the question: drop the worst units/variables or shorten the window, then re-run.")
say("")
say("_Elapsed: ", round(elapsed(), 1), " s. Rules: edit RULES at the top of tools/feasibility_check.R._")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stamp <- paste0(spec_name, "_", Sys.Date())
writeLines(md, file.path(out_dir, paste0(stamp, ".md")))
readr::write_csv(cov, file.path(out_dir, paste0(stamp, "_coverage.csv")))
line("\nReport: ", file.path(out_dir, paste0(stamp, ".md")))
