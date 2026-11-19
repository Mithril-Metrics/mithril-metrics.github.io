# 03_analyse.R — elasticities, subsidy split, robustness, back-of-envelope -------
# Case: 2026-11-ssa-fertiliser-response (local folder: AgEcon/Issue 2/)
# Input:  output/cache/panel.rds, ratio_month.rds, ratio_year.rds
# Output: output/key_numbers.csv, output/tab_models.csv, tab_loo.csv, tab_back_of_envelope.csv

stopifnot("Run this script from the issue folder (AgEcon/Issue 2)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(fixest)
})
panel <- readRDS("output/cache/panel.rds")
ratio_month <- readRDS("output/cache/ratio_month.rds")
ratio_year <- readRDS("output/cache/ratio_year.rds")
raw <- readRDS("output/cache/raw.rds")
z90 <- qnorm(0.95)
src_pink <- paste("World Bank Pink Sheet,", raw$versions$pink)
src_fao <- paste0("FAOSTAT QCL (", substr(raw$versions$qcl, 1, 10), "), RFN (", substr(raw$versions$rfn, 1, 10), ")")
src_mafap <- "FAO MAFAP public expenditure, March 2026 revision"

# Driscoll-Kraay errors: shocks are shared across countries within a year and persist over time
setFixest_estimation(panel.id = ~iso3 + year)
setFixest_vcov(panel = "DK")

grab <- function(m, term, outcome, model, sample = "All 14 countries") {
  tibble::tibble(outcome = outcome, model = model, sample = sample, term = term,
                 est = unname(coef(m)[term]), se = unname(se(m)[term]),
                 lo = est - z90 * se, hi = est + z90 * se, n = nobs(m))
}

# ---- Spec A: country FE + country trends + ENSO ----
# iso3[t] = country fixed effect plus a country-specific linear trend
a_y <- feols(log_yield ~ log_ratio + oni | iso3[t], data = panel)
a_n <- feols(log_n ~ log_ratio + oni | iso3[t], data = panel)
a_y_lo <- feols(log_yield ~ log_ratio + oni | iso3[t], data = filter(panel, !high))
a_y_hi <- feols(log_yield ~ log_ratio + oni | iso3[t], data = filter(panel, high))
a_n_lo <- feols(log_n ~ log_ratio + oni | iso3[t], data = filter(panel, !high))
a_n_hi <- feols(log_n ~ log_ratio + oni | iso3[t], data = filter(panel, high))

# ---- Spec B: year FE absorb every common shock, so only the high-low difference is left ----
b_y <- feols(log_yield ~ ratio_high | iso3 + year, data = panel)
b_n <- feols(log_n ~ ratio_high | iso3 + year, data = panel)

# ---- Robustness ----
r1_n <- feols(log_n ~ log_ratio + oni | iso3[t], data = filter(panel, n_official))
r2_y <- feols(log_yield ~ log_ratio_lag + oni | iso3[t], data = panel)
r2_n <- feols(log_n ~ log_ratio_lag + oni | iso3[t], data = panel)

# ---- Added after the first results (2026-11-19), see 00_meta.yml -> after_results ----
# D1: the two prices separately. Nominal $/t, so common inflation sits in both; the country
# trends absorb its smooth part. If yields follow maize prices but not urea, the ratio
# result is not about fertiliser cost.
d1_y <- feols(log_yield ~ log_urea + log_maize + oni | iso3[t], data = panel)
d1_n <- feols(log_n ~ log_urea + log_maize + oni | iso3[t], data = panel)
# R4 (exploratory): without Zimbabwe 2000-2009, the economic collapse that peaked with the
# 2008 price spike
no_zwe <- filter(panel, !(iso3 == "ZWE" & year %in% 2000:2009))
r4_y <- feols(log_yield ~ log_ratio + oni | iso3[t], data = no_zwe)
r4_by <- feols(log_yield ~ ratio_high | iso3 + year, data = no_zwe)
r4_bn <- feols(log_n ~ ratio_high | iso3 + year, data = no_zwe)

etable(a_y, a_n, a_y_lo, a_y_hi, b_y, b_n, r1_n, r2_y, d1_y, d1_n, r4_y, r4_by)

models <- bind_rows(
  grab(a_y, "log_ratio", "Maize yield", "A"),
  grab(a_n, "log_ratio", "N use per ha", "A"),
  grab(a_y_lo, "log_ratio", "Maize yield", "A", "Low input subsidies"),
  grab(a_y_hi, "log_ratio", "Maize yield", "A", "High input subsidies"),
  grab(a_n_lo, "log_ratio", "N use per ha", "A", "Low input subsidies"),
  grab(a_n_hi, "log_ratio", "N use per ha", "A", "High input subsidies"),
  grab(b_y, "ratio_high", "Maize yield", "B: high minus low"),
  grab(b_n, "ratio_high", "N use per ha", "B: high minus low"),
  grab(r1_n, "log_ratio", "N use per ha", "R1: official N values only"),
  grab(r2_y, "log_ratio_lag", "Maize yield", "R2: ratio lagged one season"),
  grab(r2_n, "log_ratio_lag", "N use per ha", "R2: ratio lagged one season"),
  grab(d1_y, "log_urea", "Maize yield", "D1: urea price"),
  grab(d1_y, "log_maize", "Maize yield", "D1: maize price"),
  grab(d1_n, "log_urea", "N use per ha", "D1: urea price"),
  grab(d1_n, "log_maize", "N use per ha", "D1: maize price"),
  grab(r4_y, "log_ratio", "Maize yield", "R4: without Zimbabwe 2000-2009"),
  grab(r4_by, "ratio_high", "Maize yield", "R4: high minus low, without Zimbabwe 2000-2009"),
  grab(r4_bn, "ratio_high", "N use per ha", "R4: high minus low, without Zimbabwe 2000-2009")
)
readr::write_csv(models |> mutate(across(where(is.double), ~ round(.x, 4))), "output/tab_models.csv")
print(models)

# ---- R3: leave one country out ----
loo <- bind_rows(lapply(unique(panel$iso3), function(c) {
  d <- filter(panel, iso3 != c)
  bind_rows(
    grab(feols(log_yield ~ log_ratio + oni | iso3[t], data = d), "log_ratio", "Maize yield", "A"),
    grab(feols(log_yield ~ ratio_high | iso3 + year, data = d), "ratio_high", "Maize yield", "B: high minus low")
  ) |> mutate(dropped = c)
}))
loo <- loo |>
  left_join(models |> filter(sample == "All 14 countries") |> select(outcome, model, full = est),
            by = c("outcome", "model")) |>
  mutate(shift = est - full)
readr::write_csv(loo |> mutate(across(where(is.double), ~ round(.x, 4))), "output/tab_loo.csv")
cat("\nLeave-one-country-out, largest shifts:\n")
print(loo |> group_by(model) |> slice_max(abs(shift), n = 2) |> select(model, dropped, est, full, shift))

# ---- Back-of-envelope: 2026 crop year against the 1990-2019 norm ----
base <- median(ratio_year$ratio[ratio_year$year %in% 1990:2019])
cy_now <- max(ratio_year$year)
ratio_now <- ratio_year$ratio[ratio_year$year == cy_now]
gap <- log(ratio_now / base)
last <- tail(ratio_month, 1)
prod24 <- panel |>
  filter(year == 2024) |>
  group_by(group) |>
  summarise(prod = sum(prod, na.rm = TRUE), .groups = "drop")
implied <- function(b) 100 * (exp(b * gap) - 1)

boe_rows <- list(
  list("All 14 countries", "All 14 countries", sum(prod24$prod)),
  list("Low input subsidies", "Low input subsidies", prod24$prod[prod24$group == "Low input subsidies"]),
  list("High input subsidies", "High input subsidies", prod24$prod[prod24$group == "High input subsidies"])
)
boe <- bind_rows(lapply(boe_rows, function(r) {
  m <- filter(models, outcome == "Maize yield", model == "A", sample == r[[1]])
  tibble::tibble(sample = r[[2]], prod_2024_t = r[[3]],
                 yield_pct = implied(m$est),
                 yield_pct_lo = pmin(implied(m$lo), implied(m$hi)),
                 yield_pct_hi = pmax(implied(m$lo), implied(m$hi))) |>
    mutate(tonnes = prod_2024_t * yield_pct / 100,
           tonnes_lo = prod_2024_t * yield_pct_lo / 100,
           tonnes_hi = prod_2024_t * yield_pct_hi / 100)
}))
readr::write_csv(boe |> mutate(across(where(is.double), ~ round(.x, 2))), "output/tab_back_of_envelope.csv")
print(boe)

# ---- Key numbers ----
kn_reset()
m <- function(o, mod, s = "All 14 countries") filter(models, outcome == o, model == mod, sample == s)
pct10 <- function(b) 100 * (1.1^b - 1)       # % change in the outcome for a 10% higher ratio

kn_add("n_countries", n_distinct(panel$iso3), "", 0, "Countries in the panel")
kn_add("year_start", 1990, "", 0, "First year", big_mark = "")
kn_add("year_end", 2024, "", 0, "Last year with yield and N data", big_mark = "")
kn_add("n_obs_yield", nobs(a_y), "", 0, "Country-years, yield model")
kn_add("n_obs_n", nobs(a_n), "", 0, "Country-years, N model")
kn_add("n_imputed_pct", 100 * mean(!panel$n_official[!is.na(panel$log_n)]), "%", 0,
       "Share of N-use values not from official or international sources (FAO flags I, E)", src_fao)

kn_add("ratio_base", base, "t maize per t urea", 2, "Median crop-year urea/maize price ratio 1990-2019", src_pink)
kn_add("ratio_now", ratio_now, "t maize per t urea", 2, "Urea/maize ratio, crop year Oct 2025-Sep 2026", src_pink)
kn_add("ratio_now_multiple", ratio_now / base, "x", 2, "Crop year 2026 ratio relative to 1990-2019 median", src_pink)
kn_add("crop_year_now", cy_now, "", 0, "Latest complete crop year (Oct-Sep)", big_mark = "")
kn_add("ratio_latest", last$ratio, "t maize per t urea", 2, "Urea/maize ratio in the latest month", src_pink)
kn_add("ratio_latest_multiple", last$ratio / base, "x", 2, "Latest month relative to 1990-2019 median", src_pink)
kn_add("latest_month", last$month, "", 0, "Latest month in the Pink Sheet (1-12)")
kn_add("latest_year", last$year, "", 0, "Year of the latest month", big_mark = "")
# One peak per price episode, so a spike that straddles a calendar year is not cut in two
episodes <- list(p0708 = c("2007-01-01", "2009-06-01"),
                 p2122 = c("2021-01-01", "2022-12-01"),
                 p26 = c("2025-07-01", "2026-12-01"))
for (e in names(episodes)) {
  w <- as.Date(episodes[[e]])
  pk <- ratio_month |>
    filter(date >= w[1], date <= w[2]) |>
    slice_max(ratio, n = 1, with_ties = FALSE)
  kn_add(paste0("ratio_", e), pk$ratio, "t maize per t urea", 2, paste("Peak monthly ratio, episode", e), src_pink)
  kn_add(paste0("ratio_", e, "_month"), pk$month, "", 0, paste("Month of the peak, episode", e))
  kn_add(paste0("ratio_", e, "_year"), pk$year, "", 0, paste("Year of the peak, episode", e), big_mark = "")
}
since90 <- filter(ratio_month, year >= 1990)
p26_is_max <- max(since90$ratio) == max(since90$ratio[since90$year >= 2025])
kn_add("ratio_p26_is_max", as.numeric(p26_is_max),
       "", 0, "1 if the 2026 episode peak is the highest monthly ratio since 1990")

add_b <- function(key, row, what) {
  kn_add(key, row$est, "", 3, paste("Elasticity:", what), src_fao)
  kn_add(paste0(key, "_lo"), row$lo, "", 3, paste("90% CI low:", what))
  kn_add(paste0(key, "_hi"), row$hi, "", 3, paste("90% CI high:", what))
  kn_add(paste0(key, "_10pct"), pct10(row$est), "%", 1, paste("% change for a 10% higher ratio:", what))
  kn_add(paste0(key, "_10pct_lo"), pct10(row$lo), "%", 1, paste("90% CI low, % for +10%:", what))
  kn_add(paste0(key, "_10pct_hi"), pct10(row$hi), "%", 1, paste("90% CI high, % for +10%:", what))
}
add_b("b_yield", m("Maize yield", "A"), "maize yield, all countries")
add_b("b_n", m("N use per ha", "A"), "N use per ha, all countries")
add_b("b_yield_low", m("Maize yield", "A", "Low input subsidies"), "maize yield, low-subsidy countries")
add_b("b_yield_high", m("Maize yield", "A", "High input subsidies"), "maize yield, high-subsidy countries")
add_b("b_n_low", m("N use per ha", "A", "Low input subsidies"), "N use, low-subsidy countries")
add_b("b_n_high", m("N use per ha", "A", "High input subsidies"), "N use, high-subsidy countries")
add_b("theta_yield", m("Maize yield", "B: high minus low"), "yield, high minus low subsidy (year FE)")
add_b("theta_n", m("N use per ha", "B: high minus low"), "N use, high minus low subsidy (year FE)")
add_b("b_n_official", m("N use per ha", "R1: official N values only"), "N use, official values only")
add_b("b_yield_lag", m("Maize yield", "R2: ratio lagged one season"), "maize yield, ratio lagged one season")
add_b("b_urea_yield", m("Maize yield", "D1: urea price"), "maize yield on urea price (maize price held fixed)")
add_b("b_maize_yield", m("Maize yield", "D1: maize price"), "maize yield on maize price (urea price held fixed)")
add_b("b_urea_n", m("N use per ha", "D1: urea price"), "N use on urea price (maize price held fixed)")
add_b("b_maize_n", m("N use per ha", "D1: maize price"), "N use on maize price (urea price held fixed)")
add_b("b_yield_nozwe", m("Maize yield", "R4: without Zimbabwe 2000-2009"), "maize yield, without Zimbabwe 2000-2009")
add_b("theta_yield_nozwe", m("Maize yield", "R4: high minus low, without Zimbabwe 2000-2009"),
      "yield, high minus low subsidy, without Zimbabwe 2000-2009")
add_b("theta_n_nozwe", m("N use per ha", "R4: high minus low, without Zimbabwe 2000-2009"),
      "N use, high minus low subsidy, without Zimbabwe 2000-2009")

loo_a <- filter(loo, model == "A")
loo_b <- filter(loo, model == "B: high minus low")
kn_add("loo_yield_min", min(loo_a$est), "", 3, "Leave-one-out minimum, yield elasticity")
kn_add("loo_yield_max", max(loo_a$est), "", 3, "Leave-one-out maximum, yield elasticity")
kn_add("loo_theta_min", min(loo_b$est), "", 3, "Leave-one-out minimum, high-low yield difference")
kn_add("loo_theta_max", max(loo_b$est), "", 3, "Leave-one-out maximum, high-low yield difference")

g <- read.csv("output/tab_subsidy_groups.csv")
kn_add("b1_share_high", mean(g$b1_share[g$high]), "%", 0, "Mean B1 share of support, high group", src_mafap)
kn_add("b1_share_low", mean(g$b1_share[!g$high]), "%", 0, "Mean B1 share of support, low group", src_mafap)

all_boe <- boe[boe$sample == "All 14 countries", ]
kn_add("prod_2024_mt", all_boe$prod_2024_t / 1e6, "million t", 1, "Maize production 2024, 14 countries", src_fao)
kn_add("boe_yield_pct", all_boe$yield_pct, "%", 1, "Implied yield change at the 2026 ratio, if past responses hold")
kn_add("boe_yield_pct_lo", all_boe$yield_pct_lo, "%", 1, "90% CI low")
kn_add("boe_yield_pct_hi", all_boe$yield_pct_hi, "%", 1, "90% CI high")
kn_add("boe_kt", all_boe$tonnes / 1e3, "thousand t", 0, "Implied change in maize output, 14 countries")
kn_add("boe_kt_lo", all_boe$tonnes_lo / 1e3, "thousand t", 0, "90% CI low")
kn_add("boe_kt_hi", all_boe$tonnes_hi / 1e3, "thousand t", 0, "90% CI high")

write_session_info()
cat("\nKey numbers written:", nrow(kn_read("output/key_numbers.csv")), "\n")
