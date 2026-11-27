# 02_clean.R - weekly province panel of packaged and bulk prices, monthly national series
# Case: 2026-11-indonesia-cooking-oil-bulk-packaged
# Input: data_raw/ (via the acutils cache), 00_meta.yml   Output: output/cache/*.rds

stopifnot("Run this script from the issue folder (AgEcon/Issue 7)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})
dir.create("output/cache", recursive = TRUE, showWarnings = FALSE)
meta <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")
dec <- meta$decisions

latest <- function(oil, freq) {
  f <- list.files("data_raw", pattern = paste0("^pihps-", oil, "-", freq, "_.*\\.json$"),
                  full.names = TRUE)
  if (length(f) == 0) stop("No cached file for ", oil, " ", freq, ": run 01_get_data.R first.")
  sort(f, decreasing = TRUE)[1]
}

# PIHPS returns one row per region and one column per period; values are strings such as
# "18,650", and "-" where no price was recorded (mostly holiday weeks)
read_grid <- function(oil, freq) {
  fromJSON(latest(oil, freq))$data |>
    select(-no) |>
    pivot_longer(-c(name, level), names_to = "period", values_to = "price") |>
    mutate(price = suppressWarnings(as.numeric(gsub(",", "", price))), oil = oil)
}

oils <- names(dec$commodities)

# ---- Weekly panel ----
wk <- bind_rows(lapply(oils, read_grid, freq = "weekly")) |>
  mutate(week = as.Date(period, format = "%d/%m/%Y")) |>
  select(name, level, week, oil, price) |>
  pivot_wider(names_from = oil, values_from = price)

stopifnot("Weeks must start on Mondays" = !anyNA(wk$week) && all(format(wk$week, "%u") == "1"))

# Outlier rule fixed at P4: a week-on-week jump above 40% in either series removes that
# province-week from the gap (the jump is measured from the last recorded week)
jump <- function(p) {
  prev <- rep(NA_real_, length(p))
  last <- NA_real_
  for (i in seq_along(p)) {
    prev[i] <- last
    if (!is.na(p[i])) last <- p[i]
  }
  abs(p / prev - 1) > dec$outlier_jump
}

prov <- wk |>
  filter(level == 1) |>
  rename(province = name) |>
  arrange(province, week) |>
  group_by(province) |>
  mutate(
    out_bulk = jump(bulk) %in% TRUE,
    out_pkg1 = jump(pkg1) %in% TRUE,
    out_pkg2 = jump(pkg2) %in% TRUE
  ) |>
  ungroup()

dropped <- prov |>
  filter(out_bulk | out_pkg1) |>
  select(province, week, bulk, pkg1, out_bulk, out_pkg1)
readr::write_csv(dropped, "output/tab_outliers_dropped.csv")

ev0 <- as.Date(dec$event_week)
prod <- unlist(meta$producer_2021)
unknown <- setdiff(unique(prov$province), names(prod))
if (length(unknown)) stop("Province names not in producer_2021: ", paste(unknown, collapse = ", "))

panel <- prov |>
  mutate(
    ln_bulk = if_else(out_bulk, NA_real_, log(bulk)),
    ln_pkg1 = if_else(out_pkg1, NA_real_, log(pkg1)),
    ln_pkg2 = if_else(out_pkg2, NA_real_, log(pkg2)),
    gap = ln_pkg1 - ln_bulk,
    gap2 = ln_pkg2 - ln_bulk,
    k = as.integer(difftime(week, ev0, units = "weeks")),
    prod_t = prod[province],
    prod_share = prod_t / sum(prod),
    producer = as.integer(prod_share >= dec$producer_threshold),
    eastern = province %in% unlist(meta$eastern_provinces)
  ) |>
  filter(week <= as.Date(dec$window_end))

has_bulk <- panel |>
  group_by(province) |>
  summarise(n_bulk = sum(!is.na(bulk)), .groups = "drop")
no_bulk <- has_bulk$province[has_bulk$n_bulk == 0]

national <- wk |>
  filter(level == 0) |>
  transmute(week, bulk, pkg1, pkg2, gap = log(pkg1) - log(bulk),
            k = as.integer(difftime(week, ev0, units = "weeks")))

# ---- Monthly national series ----
# Month labels may be Indonesian ("Mei", "Agu", "Okt", "Des") or English
mon <- c(jan = 1, feb = 2, mar = 3, apr = 4, mei = 5, may = 5, jun = 6, jul = 7, agu = 8,
         aug = 8, sep = 9, okt = 10, oct = 10, nov = 11, des = 12, dec = 12)
parse_month <- function(x) {
  m <- mon[tolower(substr(trimws(x), 1, 3))]
  y <- as.integer(sub(".*(\\d{4}).*", "\\1", x))
  as.Date(sprintf("%d-%02d-01", y, m))
}

monthly <- bind_rows(lapply(oils, read_grid, freq = "monthly")) |>
  filter(level == 0) |>
  mutate(month = parse_month(period)) |>
  select(month, oil, price) |>
  pivot_wider(names_from = oil, values_from = price) |>
  arrange(month) |>
  mutate(gap = log(pkg1) - log(bulk))

stopifnot(!anyNA(monthly$month))

# ---- Sanity checks ----
cat("Weekly panel:", n_distinct(panel$province), "provinces,", n_distinct(panel$week), "weeks,",
    format(min(panel$week)), "to", format(max(panel$week)), "\n")
cat("Provinces without any bulk price:", paste(no_bulk, collapse = ", "), "\n")
cat("Province-weeks with a usable gap:", sum(!is.na(panel$gap)), "of", nrow(panel), "\n")
cat("Share missing, bulk / pkg1:", round(mean(is.na(panel$bulk)), 3), "/",
    round(mean(is.na(panel$pkg1)), 3), "\n")
cat("Outlier rule dropped", nrow(dropped), "province-weeks (see tab_outliers_dropped.csv)\n")
cat("Producer provinces (>= 2% of 2021 output):",
    paste(sort(unique(panel$province[panel$producer == 1])), collapse = ", "), "\n")
cat("Event weeks k:", min(panel$k), "to", max(panel$k), "\n")
cat("Units: Rp per litre (PIHPS FAQ). National pkg1 range",
    range(national$pkg1, na.rm = TRUE), "| bulk range", range(national$bulk, na.rm = TRUE), "\n")
cat("Monthly national:", nrow(monthly), "months,", format(min(monthly$month)), "to",
    format(max(monthly$month)), "| missing gap:", sum(is.na(monthly$gap)), "\n")

saveRDS(panel, "output/cache/panel.rds")
saveRDS(national, "output/cache/national_weekly.rds")
saveRDS(monthly, "output/cache/monthly.rds")
saveRDS(no_bulk, "output/cache/no_bulk.rds")
