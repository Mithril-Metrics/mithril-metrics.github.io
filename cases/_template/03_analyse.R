# 03_analyse.R — estimation and key numbers ------------------------------------
# Case: {{CASE_ID}}
# Input:  output/cache/analysis_data.rds
# Output: output/key_numbers.csv (the ONLY source of numbers for text and figures),
#         output/tab_*.html|.csv, output/sessionInfo.txt

stopifnot("Run this script from the case folder" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")   # downloads can only land inside this folder
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
set.seed(20261118)
kn_file <- "output/key_numbers.csv"

# --- Load ----------------------------------------------------------------------
# df <- readRDS("output/cache/analysis_data.rds")

# --- Estimate --------------------------------------------------------------------
# State the estimand in one line, e.g.:
#   "Average annual TFP growth 2011–2023 minus 2001–2010, by region (percentage points)."
# Method guardrails (06_analysis_conventions_R.md):
#   * SFA: check skewness of OLS residuals first; report the distribution of u; mean TE + dispersion.
#   * Group-mean frontiers = benchmarking of groups, not farms (aggregation bias).
#   * Causal claims only with a stated identifying assumption; otherwise "associated with".
# Example:
# library(fixest)
# m1 <- feols(log(output) ~ log(fert) + log(land) | iso3 + year, data = df, cluster = ~iso3)
# modelsummary::modelsummary(list("FE" = m1), output = "output/tab_main.html")

# --- Key numbers -----------------------------------------------------------------
# One row per number used anywhere (text, titles, LinkedIn). Always state units.
kn_reset(kn_file)   # start clean on every run so stale numbers cannot survive
kn_add("example_change_pct", value = -12.34, unit = "%", digits = 1,
       description = "TEMPLATE PLACEHOLDER: change in X since 2021",
       source = "Placeholder — replace", file = kn_file)
kn_add("example_n_units", value = 27, unit = "countries", digits = 0,
       description = "TEMPLATE PLACEHOLDER: number of units", source = "Placeholder", file = kn_file)
kn_add("example_period_start", value = 2005, unit = "", digits = 0, big_mark = "",
       description = "TEMPLATE PLACEHOLDER: first year", source = "Placeholder", file = kn_file)
kn_add("example_period_end", value = 2024, unit = "", digits = 0, big_mark = "",
       description = "TEMPLATE PLACEHOLDER: last year", source = "Placeholder", file = kn_file)

print(kn_read(kn_file))
write_session_info("output")
