# 03b_second_look.R — why doesn't El Niño show up? (ADDED AFTER THE MAIN RESULTS) ----
# Case: 2026-11-elnino-rice-tails (local folder: AgEcon/Issue 1/)
# Input:  output/cache/panel.rds  (built by 02_clean.R)
# Output: output/tab_second_look.csv, output/tab_loo.csv,
#         key numbers prefixed "sl_" appended to key_numbers.csv
#
# The main specification (03_analyse.R) was fixed before seeing results and stays the headline.
# Everything here is exploratory and labelled as such in the post. Four checks, chosen on
# economic grounds before running them (2026-11-18):
#  S1  Reference group: El Niño and La Niña each vs NEUTRAL years (La Niña floods may
#      contaminate the "other years" comparison).
#  S2  Margins: El Niño effect on yield, harvested area and production, in the onset year (t)
#      and the following year (t+1). Drought can cut planted rainfed area while the remaining
#      (irrigated) harvest has high yields: yield may rise while production falls.
#  S3  Region: maritime (IDN, PHL, MYS) vs mainland (THA, VNM, KHM).
#  S4  Disruptions: drop major-conflict country-years (UCDP intensity 2); drop 1997-98.
# Inference: moving-block bootstrap over years (3-year blocks, 999 draws), as in 03_analyse.R.
# Added after the first red team: Holm-adjusted p-values across the 14 margin contrasts (S2, S3),
# and a leave-one-event-out check of the maritime production effect in t+1.
# Added after the second red team (2026-11-20), outside the Holm family:
#  D3  maritime minus mainland difference in the t+1 effect (production, yield): the direct test
#  W3  maritime production t+1 weighted by each country's mean production (Indonesia dominates)
#  T3  the maritime t+1 effect in tonnes, at each country's 2024 trend production
#  C24 2024, the harvest year after the 2023 onset: observed maritime production vs trend

stopifnot("Run this script from the issue folder (AgEcon/Issue 1)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
set.seed(20261119)
B <- 999
block_len <- 3
kn_file <- "output/key_numbers.csv"
pct <- function(x) 100 * (exp(x) - 1)
panel <- readRDS("output/cache/panel.rds")
stopifnot(all(c("shock_area", "shock_prod", "enso3", "major_conflict") %in% names(panel)))
# Production weights (country mean over the sample), and trend production in each year
panel <- panel |>
  group_by(iso3) |>
  mutate(w_prod = mean(prod)) |>
  ungroup() |>
  mutate(prod_trend = prod / exp(shock_prod))

coef_of <- function(fit, term) {
  cf <- coef(fit)
  if (term %in% names(cf)) unname(cf[[term]]) else NA_real_
}
# LPM for a >= 5% yield shortfall, El Niño and La Niña vs neutral (country FE)
s1 <- function(d) {
  f <- lm(as.numeric(loss5) ~ elnino + lanina + factor(iso3), data = d)
  c(el = coef_of(f, "elninoTRUE"), la = coef_of(f, "laninaTRUE"))
}
# Distributed-lag shock regression: ENSO in t and t-1, neutral reference, country FE
s2 <- function(d, y, w = NULL) {
  d$w_ <- if (is.null(w)) 1 else d[[w]]
  f <- lm(stats::as.formula(paste(y, "~ elnino + elnino_lag + lanina + lanina_lag + factor(iso3)")),
          data = d, weights = w_)
  c(t = coef_of(f, "elninoTRUE"), t1 = coef_of(f, "elnino_lagTRUE"))
}
main_diff5 <- function(d) {   # same estimand as the headline (El Niño vs all other years)
  coef_of(lm(as.numeric(loss5) ~ elnino + factor(iso3), data = d), "elninoTRUE")
}
ratio10 <- function(d) {
  # normal benchmark re-estimated on the data at hand, as in 03_analyse.R
  s <- stats::ave(d$shock, d$iso3, FUN = stats::mad)
  mean(d$loss10) / mean(pnorm(d$thr10 / s))
}

stats_fun <- function(d) {
  mar <- d[d$region == "Maritime", ]
  mai <- d[d$region == "Mainland", ]
  noc <- d[!d$major_conflict, ]
  nocr <- d[!d$crisis9798, ]
  a <- s1(d)
  y <- s2(d, "shock")
  ar <- s2(d, "shock_area")
  pr <- s2(d, "shock_prod")
  pm <- s2(mar, "shock_prod")
  pn <- s2(mai, "shock_prod")
  ym <- s2(mar, "shock")
  yn <- s2(mai, "shock")
  pw <- s2(mar, "shock_prod", w = "w_prod")
  c(
    s1_el_vs_neutral = a[["el"]], s1_la_vs_neutral = a[["la"]],
    s2_yield_t = y[["t"]], s2_yield_t1 = y[["t1"]],
    s2_area_t = ar[["t"]], s2_area_t1 = ar[["t1"]],
    s2_prod_t = pr[["t"]], s2_prod_t1 = pr[["t1"]],
    s3_mar_yield_t = ym[["t"]], s3_mar_yield_t1 = ym[["t1"]],
    s3_mai_yield_t = yn[["t"]], s3_mai_yield_t1 = yn[["t1"]],
    s3_mar_prod_t = pm[["t"]],  s3_mar_prod_t1 = pm[["t1"]],
    s3_mai_prod_t = pn[["t"]],  s3_mai_prod_t1 = pn[["t1"]],
    s4_noconflict_diff5 = main_diff5(noc), s4_noconflict_ratio10 = ratio10(noc),
    s4_nocrisis_diff5 = main_diff5(nocr),  s4_nocrisis_ratio10 = ratio10(nocr),
    d3_prod_t1  = pct(pm[["t1"]]) - pct(pn[["t1"]]),
    d3_yield_t1 = pct(ym[["t1"]]) - pct(yn[["t1"]]),
    w3_mar_prod_t1 = pw[["t1"]]
  )
}

# --- Descriptives --------------------------------------------------------------------------
cat("ENSO phase counts:\n")
print(table(panel$enso3, panel$region))
cat("\nMajor-conflict country-years dropped in S4:", sum(panel$major_conflict), "\n")
print(panel |> filter(major_conflict) |> count(iso3))
cat("\nShare of >=5% shortfalls by ENSO phase:\n")
print(panel |> group_by(enso3) |> summarise(n = n(), p5 = mean(loss5), p10 = mean(loss10),
                                            .groups = "drop"))
cat("\nCheck: production shock ~ area shock + yield shock (correlation):",
    round(cor(panel$shock_prod, panel$shock_area + panel$shock), 3), "\n")

# --- Estimates + moving-block bootstrap over years ---------------------------------------------
est <- stats_fun(panel)
yrs <- as.character(sort(unique(panel$year)))
by_year <- split(panel, panel$year)[yrs]
draw_years <- function(yrs, l) {
  n <- length(yrs)
  starts <- sample(seq_len(n - l + 1), ceiling(n / l), replace = TRUE)
  idx <- unlist(lapply(starts, function(s) s:(s + l - 1)))
  yrs[idx[seq_len(n)]]
}
boot <- t(vapply(seq_len(B), function(b) {
  d <- bind_rows(by_year[draw_years(yrs, block_len)])
  tryCatch(stats_fun(d), error = function(e) rep(NA_real_, length(est)))
}, numeric(length(est))))
colnames(boot) <- names(est)
ci <- apply(boot, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)

is_ratio <- grepl("ratio", names(est))
is_prob  <- grepl("^s1_|diff5", names(est))
is_diff  <- grepl("^d3_", names(est))           # already a difference of % effects
to_unit  <- function(x, nm) {
  ifelse(grepl("ratio", nm) | grepl("^d3_", nm), x,
         ifelse(grepl("^s1_|diff5", nm), 100 * x, pct(x)))
}
tab <- tibble(
  stat = names(est),
  estimate = to_unit(est, names(est)),
  lo = to_unit(ci[1, ], names(est)),
  hi = to_unit(ci[2, ], names(est)),
  unit = ifelse(is_ratio, "x", ifelse(is_prob | is_diff, "pp", "% vs trend")),
  description = c(
    "S1: P(>=5% yield shortfall), El Niño vs neutral years",
    "S1: P(>=5% yield shortfall), La Niña vs neutral years",
    "S2: El Niño effect on yield, onset year t", "S2: El Niño effect on yield, year t+1",
    "S2: El Niño effect on harvested area, t", "S2: El Niño effect on harvested area, t+1",
    "S2: El Niño effect on production, t", "S2: El Niño effect on production, t+1",
    "S3: maritime, yield, t", "S3: maritime, yield, t+1",
    "S3: mainland, yield, t", "S3: mainland, yield, t+1",
    "S3: maritime, production, t", "S3: maritime, production, t+1",
    "S3: mainland, production, t", "S3: mainland, production, t+1",
    "S4: headline estimate without major-conflict country-years",
    "S4: >=10% shortfalls vs normal, without major-conflict years",
    "S4: headline estimate without 1997-98",
    "S4: >=10% shortfalls vs normal, without 1997-98",
    "D3: maritime minus mainland, production effect t+1 (pp)",
    "D3: maritime minus mainland, yield effect t+1 (pp)",
    "W3: maritime production t+1, weighted by mean production"
  )
)
# Many contrasts were looked at, so adjust the S2/S3 p-values for multiple testing (Holm).
# p-values use the bootstrap standard error and a normal approximation.
se_boot <- apply(boot, 2, stats::sd, na.rm = TRUE)
tab$p <- 2 * stats::pnorm(-abs(est / se_boot))
tab$p[is_diff] <- 2 * stats::pnorm(-abs(est[is_diff] / se_boot[is_diff]))
margins <- grepl("^s2_|^s3_", tab$stat)
tab$p_holm <- NA_real_
tab$p_holm[margins] <- stats::p.adjust(tab$p[margins], method = "holm")
print(tab, n = Inf, width = Inf)
readr::write_csv(tab, "output/tab_second_look.csv")

# Leave one El Niño event out: does the maritime t+1 production effect rest on one episode?
# Each event removes its onset year and the following year; for back-to-back events
# (e.g. 1986-87) that also removes the next onset.
mar_all <- panel[panel$region == "Maritime", ]
events <- sort(unique(panel$year[panel$elnino]))
loo <- tibble(
  event = events,
  estimate = vapply(events, function(e) {
    d <- mar_all[!mar_all$year %in% c(e, e + 1), ]
    pct(s2(d, "shock_prod")[["t1"]])
  }, numeric(1))
)
cat("\nLeave-one-event-out, maritime production t+1 (% vs trend):\n")
print(loo, n = Inf)
readr::write_csv(loo, "output/tab_loo.csv")

# --- Key numbers (appended; 03_analyse.R resets the file first) ---------------------------------
src <- "FAOSTAT QCL rice; NOAA CPC ONI; UCDP/PRIO ACD v26.1; author's calculations (exploratory)"
for (i in seq_len(nrow(tab))) {
  u <- ifelse(tab$unit[i] == "% vs trend", "%", tab$unit[i])
  for (part in c("estimate", "lo", "hi")) {
    key <- paste0("sl_", tab$stat[i], if (part == "estimate") "" else paste0("_", part))
    kn_add(key, tab[[part]][i], unit = u, digits = 1,
           description = paste0(tab$description[i], if (part == "estimate") "" else paste0(" (95% CI ", part, ")")),
           source = src, file = kn_file)
  }
}
holm_of <- function(st) tab$p_holm[tab$stat == st]
kn_add("sl_holm_mar_prod_t1", holm_of("s3_mar_prod_t1"), unit = "", digits = 3,
       description = "Holm-adjusted p, maritime production t+1 (14 margin contrasts)", source = src, file = kn_file)
kn_add("sl_holm_mar_yield_t1", holm_of("s3_mar_yield_t1"), unit = "", digits = 3,
       description = "Holm-adjusted p, maritime yield t+1 (14 margin contrasts)", source = src, file = kn_file)
kn_add("sl_holm_area_t", holm_of("s2_area_t"), unit = "", digits = 3,
       description = "Holm-adjusted p, harvested area in the onset year (14 margin contrasts)", source = src, file = kn_file)
kn_add("sl_n_holm_sig", sum(tab$p_holm < 0.05, na.rm = TRUE), unit = "", digits = 0,
       description = "Margin contrasts with Holm-adjusted p < 0.05", source = src, file = kn_file)
kn_add("sl_n_margin_tests", sum(margins), unit = "", digits = 0,
       description = "Margin contrasts tested (S2, S3)", source = src, file = kn_file)
kn_add("sl_loo_min", min(loo$estimate), unit = "%", digits = 1,
       description = "Maritime production t+1, leave-one-event-out: most negative", source = src, file = kn_file)
kn_add("sl_loo_max", max(loo$estimate), unit = "%", digits = 1,
       description = "Maritime production t+1, leave-one-event-out: closest to zero", source = src, file = kn_file)
kn_add("sl_loo_max_event", loo$event[which.max(loo$estimate)], unit = "", digits = 0, big_mark = "",
       description = "El Niño event whose removal moves the maritime t+1 effect closest to zero",
       source = src, file = kn_file)
kn_add("sl_n_conflict_years", sum(panel$major_conflict), unit = "country-years", digits = 0,
       description = "Country-years with major armed conflict (UCDP intensity 2), dropped in S4",
       source = src, file = kn_file)
for (ph in c("Neutral", "El Nino", "La Nina")) {
  key <- paste0("sl_p5_", tolower(gsub(" ", "", ph)), "_pct")
  kn_add(key, 100 * mean(panel$loss5[panel$enso3 == ph]), unit = "%", digits = 0,
         description = paste("Share of >=5% yield shortfalls,", ph, "years"), source = src, file = kn_file)
}
# --- Tonnes and the 2024 check (T3, C24) ------------------------------------------------------------
last_year <- max(panel$year)
mar_last <- panel |> filter(region == "Maritime", year == last_year)
eff <- exp(c(est = est[["s3_mar_prod_t1"]], lo = ci[1, "s3_mar_prod_t1"], hi = ci[2, "s3_mar_prod_t1"])) - 1
tonnes <- mar_last |>
  transmute(iso3, prod_trend_kt = prod_trend / 1e3,
            gap_kt = prod_trend_kt * eff[["est"]],
            gap_lo_kt = prod_trend_kt * eff[["lo"]],
            gap_hi_kt = prod_trend_kt * eff[["hi"]],
            obs_vs_trend_pct = 100 * (prod / prod_trend - 1))
cat("\nMaritime t+1 effect in tonnes at", last_year, "trend production (kt):\n")
print(tonnes, width = Inf)
readr::write_csv(tonnes, "output/tab_tonnes.csv")
onset_prev <- any(panel$year == last_year - 1 & panel$elnino)
mar_obs_last <- 100 * (sum(mar_last$prod) / sum(mar_last$prod_trend) - 1)
cat("Previous year an El Niño onset:", onset_prev, "; maritime production", last_year,
    "vs trend:", round(mar_obs_last, 1), "%\n")

add_t <- function(key, value, unit, digits, description, big_mark = ",")
  kn_add(key, value, unit = unit, digits = digits, description = description, source = src,
         big_mark = big_mark, file = kn_file)
add_t("sl_t3_mar_kt", sum(tonnes$gap_kt), "thousand t", 0,
      paste("T3: maritime t+1 effect in tonnes at", last_year, "trend production (IDN+PHL+MYS)"))
add_t("sl_t3_mar_lo_kt", sum(tonnes$gap_lo_kt), "thousand t", 0, "T3: 95% CI lower")
add_t("sl_t3_mar_hi_kt", sum(tonnes$gap_hi_kt), "thousand t", 0, "T3: 95% CI upper")
add_t("sl_t3_idn_kt", tonnes$gap_kt[tonnes$iso3 == "IDN"], "thousand t", 0,
      "T3: of which Indonesia")
add_t("sl_t3_year", last_year, "", 0, "Year of trend production used for tonnes", big_mark = "")
add_t("sl_c24_onset_prev", as.numeric(onset_prev), "", 0, "1 if the year before the last year is an El Niño onset")
add_t("sl_c24_mar_prod_pct", mar_obs_last, "%", 1,
      paste("C24: observed maritime production vs trend in", last_year, "(production-weighted)"))
add_t("sl_c24_idn_prod_pct", tonnes$obs_vs_trend_pct[tonnes$iso3 == "IDN"], "%", 1,
      paste("C24: observed Indonesian production vs trend in", last_year))
add_t("sl_p_d3_prod_t1", tab$p[tab$stat == "d3_prod_t1"], "", 3,
      "D3: p-value, maritime minus mainland production t+1 (bootstrap SE)")
message("03b_second_look.R done.")
