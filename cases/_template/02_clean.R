# 02_clean.R — tidy, harmonise units, check the panel --------------------------
# Case: {{CASE_ID}}
# Input:  data_raw/ (via the acutils cache)   Output: output/cache/analysis_data.rds

stopifnot("Run this script from the case folder" = file.exists("00_meta.yml"))
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})
options(acutils.data_dir = "data_raw")
dir.create("output/cache", recursive = TRUE, showWarnings = FALSE)

# --- Load (re-uses the cached files written by 01_get_data.R) ------------------
# tfp <- get_usda_tfp()

# --- Tidy ----------------------------------------------------------------------
# Economics first: decide the unit of observation (country-year, region-year, farm group)
# and the units of every variable BEFORE reshaping. Convert to interpretable units
# (t/ha, EUR/ha, %, kcal/person/day) here, not in the analysis script.
# df <- tfp |>
#   filter(variable %in% c("TFP_Index", "Outall_Index", "Input_Index"), !is.na(iso3)) |>
#   select(iso3, country, region, year, variable, value) |>
#   pivot_wider(names_from = variable, values_from = value)

# --- Checks --------------------------------------------------------------------
# check_panel(df, id = "iso3", time = "year")   # N, T, balance, duplicates, NA shares
# stopifnot(!anyDuplicated(df[c("iso3", "year")]))

# saveRDS(df, "output/cache/analysis_data.rds")
message("02_clean.R: template stub — nothing to clean yet.")
