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

# ---- Second red team (P7, 2026-11-20: policy and econometric reviews) ----
# B2: subsidy split with country trends and an ENSO-by-group term next to the year FE, on
# 2005-2024 only, the years the MAFAP budgets that define the groups actually cover
panel <- panel |> mutate(oni_high = oni * high)
b2_y <- feols(log_yield ~ ratio_high + oni_high | iso3[t] + year, data = filter(panel, year >= 2005))
b2_n <- feols(log_n ~ ratio_high + oni_high | iso3[t] + year, data = filter(panel, year >= 2005))
# R5: ratio over the six months before each region's main maize planting, so the price is
# known when fertiliser is bought. Southern Africa plants Nov-Dec (window Jun-Nov of t-1),
# East Africa Mar-Apr (Sep t-1 to Feb t), West Africa May-Jul (Dec t-1 to May t).
plant_region <- c(MWI = "S", ZMB = "S", ZWE = "S", MOZ = "S",
                  KEN = "E", ETH = "E", TZA = "E", UGA = "E", RWA = "E",
                  NGA = "W", GHA = "W", BFA = "W", MLI = "W", BEN = "W")
stopifnot(all(unique(panel$iso3) %in% names(plant_region)))
plant_window <- list(S = 6:11, E = c(9:12, 1:2), W = c(12, 1:5))
plant_offset <- list(S = rep(1, 6), E = c(1, 1, 1, 1, 0, 0), W = c(1, 0, 0, 0, 0, 0))
ratio_plant <- bind_rows(lapply(names(plant_window), function(r) {
  mo <- plant_window[[r]]
  off <- plant_offset[[r]]
  ratio_month |>
    filter(month %in% mo) |>
    mutate(harvest_year = year + off[match(month, mo)]) |>
    group_by(harvest_year) |>
    summarise(ratio_plant = mean(ratio), n_m = n(), .groups = "drop") |>
    filter(n_m == length(mo)) |>
    transmute(region = r, year = harvest_year, log_ratio_plant = log(ratio_plant))
}))
panel <- panel |>
  mutate(region = plant_region[iso3]) |>
  left_join(ratio_plant, by = c("region", "year"))
r5_y <- feols(log_yield ~ log_ratio_plant + oni | iso3[t], data = panel)
r5_n <- feols(log_n ~ log_ratio_plant + oni | iso3[t], data = panel)

# T1: the ratio is one world series, so the effective sample is the number of years. Partial out
# country effects, trends and ENSO, average the residuals by year and regress the 35 yearly means
# on each other, with Newey-West errors (2 lags) and t critical values.
ts_d <- filter(panel, !is.na(log_yield), !is.na(log_ratio), !is.na(oni))
ts_d$y_res <- resid(feols(log_yield ~ oni | iso3[t], data = ts_d))
ts_d$r_res <- resid(feols(log_ratio ~ oni | iso3[t], data = ts_d))
ts_y <- ts_d |>
  group_by(year) |>
  summarise(y = mean(y_res), r = mean(r_res), .groups = "drop")
ts_fit <- lm(y ~ r, data = ts_y)
nw_se <- function(fit, L) {
  X <- model.matrix(fit)
  u <- X * resid(fit)
  n <- nrow(X)
  S <- crossprod(u)
  for (l in seq_len(L)) {
    G <- crossprod(u[(l + 1):n, , drop = FALSE], u[1:(n - l), , drop = FALSE])
    S <- S + (1 - l / (L + 1)) * (G + t(G))
  }
  B <- solve(crossprod(X))
  sqrt(diag(B %*% S %*% B))
}
ts_se <- nw_se(ts_fit, 2)[["r"]]
t90 <- qt(0.95, df = nrow(ts_y) - 2)
ts_row <- tibble::tibble(outcome = "Maize yield", model = "T1: 35 yearly means, Newey-West",
                         sample = "All 14 countries", term = "log_ratio",
                         est = unname(coef(ts_fit)[["r"]]), se = ts_se,
                         lo = est - t90 * se, hi = est + t90 * se, n = nrow(ts_y))

# ---- Red-team checks (P7, 2026-11-19) ----
# F1: does the yield link survive without each earlier spike episode? (crop years)
f1a_y <- feols(log_yield ~ log_ratio + oni | iso3[t], data = filter(panel, !year %in% 2008:2009))
f1b_y <- feols(log_yield ~ log_ratio + oni | iso3[t], data = filter(panel, !year %in% 2021:2022))
# F2: yields where N use is officially reported, to see if the yield-N gap is a data artefact
f2_y <- feols(log_yield ~ log_ratio + oni | iso3[t], data = filter(panel, n_official))

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
  grab(r4_bn, "ratio_high", "N use per ha", "R4: high minus low, without Zimbabwe 2000-2009"),
  grab(f1a_y, "log_ratio", "Maize yield", "F1: without crop years 2008-2009"),
  grab(f1b_y, "log_ratio", "Maize yield", "F1: without crop years 2021-2022"),
  grab(f2_y, "log_ratio", "Maize yield", "F2: years with official N data"),
  grab(b2_y, "ratio_high", "Maize yield", "B2: high minus low, trends + ENSO by group, 2005-2024"),
  grab(b2_n, "ratio_high", "N use per ha", "B2: high minus low, trends + ENSO by group, 2005-2024"),
  grab(r5_y, "log_ratio_plant", "Maize yield", "R5: ratio before planting"),
  grab(r5_n, "log_ratio_plant", "N use per ha", "R5: ratio before planting"),
  ts_row
)
readr::write_csv(models |> mutate(across(where(is.double), ~ round(.x, 4))), "output/tab_models.csv")
print(models, n = Inf, width = Inf)

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

# F4: same benchmark with the elasticity estimated without Zimbabwe 2000-2009
r4_row <- filter(models, model == "R4: without Zimbabwe 2000-2009")
boe_nozwe <- c(est = implied(r4_row$est),
               lo = min(implied(r4_row$lo), implied(r4_row$hi)),
               hi = max(implied(r4_row$lo), implied(r4_row$hi)))

# ---- F3: did input-subsidy spending rise during the 2008 spike? (MAFAP, budget years) ----
sub_yrs <- readRDS("output/cache/subsidy_years.rds")
spike08 <- sub_yrs |>
  filter(year %in% c(2007, 2009)) |>
  tidyr::pivot_wider(id_cols = c(iso3, country), names_from = year,
                     values_from = c(share, b1_usd17)) |>
  filter(!is.na(share_2007), !is.na(share_2009)) |>
  mutate(share_change_pp = share_2009 - share_2007,
         real_change_pct = 100 * (b1_usd17_2009 / b1_usd17_2007 - 1))
readr::write_csv(spike08 |> mutate(across(where(is.double), ~ round(.x, 1))), "output/tab_subsidy_2008.csv")
cat("\nMAFAP B1 (variable inputs), budget year 2007 vs 2009:\n")
print(spike08)
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
       "Share of N-use values not flagged A or X (imputed, estimated or no flag on total use)", src_fao)
kn_add("n_years_ratio", n_distinct(panel$year[!is.na(panel$log_ratio) & !is.na(panel$log_yield)]), "", 0,
       "Crop years of the world price ratio in the yield model (effective time-series length)")

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
top_b <- loo_b[which.max(loo_b$est), ]
kn_add("loo_theta_max_lo", top_b$lo, "", 3, "90% CI low, leave-one-out maximum of the high-low yield difference")
kn_add("loo_theta_max_hi", top_b$hi, "", 3, "90% CI high, leave-one-out maximum of the high-low yield difference")
kn_add("loo_theta_max_10pct", pct10(top_b$est), "%", 1, "Leave-one-out maximum, high-low yield difference, % for +10%")
kn_add("loo_theta_max_10pct_lo", pct10(top_b$lo), "%", 1, "90% CI low, % for +10%")
kn_add("loo_theta_max_10pct_hi", pct10(top_b$hi), "%", 1, "90% CI high, % for +10%")
kn_add("loo_theta_max_is_zwe", as.numeric(top_b$dropped == "ZWE"), "", 0,
       "1 if dropping Zimbabwe gives the largest high-low yield difference")
kn_add("loo_theta_n_excl0", sum(loo_b$lo > 0 | loo_b$hi < 0), "", 0,
       "Leave-one-out runs where the high-low yield interval excludes zero")

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
kn_add("boe_yield_pct_nozwe", boe_nozwe[["est"]], "%", 1,
       "Implied yield change at the 2026 ratio, elasticity without Zimbabwe 2000-2009")
kn_add("boe_yield_pct_nozwe_lo", boe_nozwe[["lo"]], "%", 1, "90% CI low")
kn_add("boe_yield_pct_nozwe_hi", boe_nozwe[["hi"]], "%", 1, "90% CI high")
kn_add("boe_kt_nozwe", all_boe$prod_2024_t * boe_nozwe[["est"]] / 100 / 1e3, "thousand t", 0,
       "Implied change in maize output, elasticity without Zimbabwe 2000-2009")

add_b("b_yield_no0809", m("Maize yield", "F1: without crop years 2008-2009"), "maize yield, without crop years 2008-2009")
add_b("b_yield_no2122", m("Maize yield", "F1: without crop years 2021-2022"), "maize yield, without crop years 2021-2022")
add_b("b_yield_offn", m("Maize yield", "F2: years with official N data"), "maize yield, years with official N data")
kn_add("n_obs_offn", nobs(f2_y), "", 0, "Country-years with official N data in the yield model")

add_b("theta_yield_b2", m("Maize yield", "B2: high minus low, trends + ENSO by group, 2005-2024"),
      "yield, high minus low subsidy, trends and ENSO by group, 2005-2024")
add_b("theta_n_b2", m("N use per ha", "B2: high minus low, trends + ENSO by group, 2005-2024"),
      "N use, high minus low subsidy, trends and ENSO by group, 2005-2024")
add_b("b_yield_plant", m("Maize yield", "R5: ratio before planting"), "maize yield, ratio in the six months before planting")
add_b("b_n_plant", m("N use per ha", "R5: ratio before planting"), "N use, ratio in the six months before planting")
add_b("b_yield_ts", m("Maize yield", "T1: 35 yearly means, Newey-West"), "maize yield, yearly means, Newey-West")
kn_add("n_years_ts", nrow(ts_y), "", 0, "Years in the time-series check")

kn_add("s08_n", nrow(spike08), "", 0, "Countries with MAFAP B1 data for budget years 2007 and 2009", src_mafap)
kn_add("s08_share_up", sum(spike08$share_change_pp > 0), "", 0, "Of these, B1 share of support higher in 2009", src_mafap)
kn_add("s08_real_up", sum(spike08$real_change_pct > 0, na.rm = TRUE), "", 0,
       "Of these, B1 in constant 2017 USD higher in 2009", src_mafap)
kn_add("s08_n_real", sum(!is.na(spike08$real_change_pct)), "", 0, "Countries with B1 in constant USD for both years", src_mafap)
kn_add("s08_share_change_med", median(spike08$share_change_pp), "pp", 1,
       "Median change in B1 share of support, 2007 to 2009", src_mafap)

write_session_info()
cat("\nKey numbers written:", nrow(kn_read("output/key_numbers.csv")), "\n")
