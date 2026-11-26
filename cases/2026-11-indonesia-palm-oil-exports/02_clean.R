# 02_clean.R — monthly analysis table: domestic and world prices in rupiah, phase dummies
# Case: 2026-11-indonesia-palm-oil-exports
# Input: data_raw/ (via the acutils cache), 00_meta.yml   Output: output/cache/analysis_data.rds

stopifnot("Run this script from the issue folder (AgEcon/Issue 6)" = file.exists("00_meta.yml"))
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

latest <- function(name) {
  f <- list.files("data_raw", pattern = paste0("^fpma-", name, "_.*\\.json$"), full.names = TRUE)
  if (length(f) == 0) stop("No cached file for ", name, ": run 01_get_data.R first.")
  sort(f, decreasing = TRUE)[1]
}

read_series <- function(name) {
  pts <- fromJSON(latest(name))$results$datapoints[[1]]
  tibble(date = as.Date(pts$date), price = pts$price_value) |>
    filter(!is.na(price)) |>
    distinct(date, .keep_all = TRUE) |>
    rename(!!name := price)
}

prices <- read_series("idn-vegoil") |>
  full_join(read_series("idn-flour"), by = "date") |>
  full_join(read_series("world-cpo"), by = "date") |>
  full_join(read_series("world-wheat"), by = "date")
names(prices) <- c("date", "vegoil", "flour", "cpo_usd", "wheat_usd")

fx <- fromJSON(latest("fxrate-idr"))$results |>
  filter(nominal_currency_code == "IDR", reference_currency_code == "USD") |>
  transmute(date = as.Date(date), fx = value) |>
  distinct(date, .keep_all = TRUE)

# World prices in rupiah per tonne. The tonne-to-litre/kg factor is a constant and ends up
# in the intercept of the log models, so no density or milling assumption is needed.
df <- prices |>
  inner_join(fx, by = "date") |>
  arrange(date) |>
  mutate(
    cpo_idr = cpo_usd * fx,
    wheat_idr = wheat_usd * fx,
    ln_vegoil = log(vegoil),
    ln_flour = log(flour),
    ln_cpo = log(cpo_idr),
    ln_wheat = log(wheat_idr),
    ln_cpo_l1 = lag(ln_cpo, 1),
    ln_cpo_l2 = lag(ln_cpo, 2),
    ln_wheat_l1 = lag(ln_wheat, 1),
    ln_wheat_l2 = lag(ln_wheat, 2)
  )

# ---- Sample and phase dummies (fixed at P4, read from 00_meta.yml) ----
d <- function(x) as.Date(x)
ph <- dec$phases
df <- df |>
  filter(date >= d(dec$sample[1]), date <= d(dec$sample[2])) |>
  mutate(
    window = as.integer(date >= d(ph$window[1]) & date <= d(ph$window[2])),
    window_a = as.integer(date >= d(ph$window_a[1]) & date <= d(ph$window_a[2])),
    window_b = as.integer(date >= d(ph$window_b[1]) & date <= d(ph$window_b[2])),
    post = as.integer(date >= d(ph$post_start)),
    bps2023 = as.integer(date >= d(ph$bps_change)),
    eid = as.integer(date %in% d(unlist(dec$eid_al_fitr_months))),
    phase = case_when(
      window == 1 ~ "Policy window (Jan-May 2022)",
      post == 1 & bps2023 == 0 ~ "After (Jun-Dec 2022)",
      bps2023 == 1 ~ "After (2023 onward)",
      TRUE ~ "Before (Nov 2020-Dec 2021)"
    ),
    # log gaps for the descriptive figure and R2; their level has no meaning (units differ)
    gap_oil = ln_vegoil - ln_cpo,
    gap_flour = ln_flour - ln_wheat,
    gap_diff = gap_oil - gap_flour
  )

# ---- Checks ----
cat("Months:", nrow(df), " from", format(min(df$date)), "to", format(max(df$date)), "\n")
cat("NA share by column:\n")
print(round(colMeans(is.na(df[, c("vegoil", "flour", "cpo_idr", "wheat_idr", "ln_cpo_l2", "ln_wheat_l2")])), 3))
print(count(df, phase))
cat("Eid months in sample:", sum(df$eid), "\n")
cat("Units: vegoil IDR/litre, flour IDR/kg, cpo_idr and wheat_idr IDR/tonne\n")
print(summary(df[, c("vegoil", "flour", "cpo_usd", "fx")]))

expected <- length(seq(d(dec$sample[1]), d(dec$sample[2]), by = "month"))
stopifnot(
  "Months missing in the sample" = nrow(df) == expected,
  "Missing values in model variables" = !anyNA(df[, c("ln_vegoil", "ln_flour", "ln_cpo_l2", "ln_wheat_l2")]),
  "Window must have 5 months" = sum(df$window) == 5
)

saveRDS(df, "output/cache/analysis_data.rds")
