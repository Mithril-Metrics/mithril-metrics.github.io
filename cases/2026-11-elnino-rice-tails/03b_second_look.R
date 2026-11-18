# 03b_second_look.R — why doesn't El Niño show up? (ADDED AFTER THE MAIN RESULTS) ----
# Case: 2026-11-elnino-rice-tails (local folder: AgEcon/Issue 1/)
# Input:  output/cache/panel.rds  (built by 02_clean.R)
# Output: output/tab_second_look.csv, key numbers prefixed "sl_" appended to key_numbers.csv
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
# Inference: year-block bootstrap (999 draws), as in 03_analyse.R.

stopifnot("Run this script from the issue folder (AgEcon/Issue 1)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
set.seed(20261119)
B <- 999
kn_file <- "output/key_numbers.csv"
pct <- function(x) 100 * (exp(x) - 1)
panel <- readRDS("output/cache/panel.rds")
stopifnot(all(c("shock_area", "shock_prod", "enso3", "major_conflict") %in% names(panel)))

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
s2 <- function(d, y) {
  f <- lm(stats::as.formula(paste(y, "~ elnino + elnino_lag + lanina + lanina_lag + factor(iso3)")),
          data = d)
  c(t = coef_of(f, "elninoTRUE"), t1 = coef_of(f, "elnino_lagTRUE"))
}
main_diff5 <- function(d) {   # same estimand as the headline (El Niño vs all other years)
  coef_of(lm(as.numeric(loss5) ~ elnino + factor(iso3), data = d), "elninoTRUE")
}
ratio10 <- function(d) mean(d$loss10) / mean(d$pnorm10)

stats_fun <- function(d) {
  mar <- d[d$region == "Maritime", ]; mai <- d[d$region == "Mainland", ]
  noc <- d[!d$major_conflict, ];      nocr <- d[!d$crisis9798, ]
  a <- s1(d)
  y <- s2(d, "shock"); ar <- s2(d, "shock_area"); pr <- s2(d, "shock_prod")
  pm <- s2(mar, "shock_prod"); pn <- s2(mai, "shock_prod")
  ym <- s2(mar, "shock");      yn <- s2(mai, "shock")
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
    s4_nocrisis_diff5 = main_diff5(nocr),  s4_nocrisis_ratio10 = ratio10(nocr)
  )
}

# --- Descriptives --------------------------------------------------------------------------
cat("ENSO phase counts:\n"); print(table(panel$enso3, panel$region))
cat("\nMajor-conflict country-years dropped in S4:", sum(panel$major_conflict), "\n")
print(panel |> filter(major_conflict) |> count(iso3))
cat("\nShare of >=5% shortfalls by ENSO phase:\n")
print(panel |> group_by(enso3) |> summarise(n = n(), p5 = mean(loss5), p10 = mean(loss10),
                                            .groups = "drop"))
cat("\nCheck: production shock ~ area shock + yield shock (correlation):",
    round(cor(panel$shock_prod, panel$shock_area + panel$shock), 3), "\n")

# --- Estimates + year-block bootstrap ----------------------------------------------------------
est <- stats_fun(panel)
by_year <- split(panel, panel$year)
yrs <- names(by_year)
boot <- t(vapply(seq_len(B), function(b) {
  d <- bind_rows(by_year[sample(yrs, length(yrs), replace = TRUE)])
  tryCatch(stats_fun(d), error = function(e) rep(NA_real_, length(est)))
}, numeric(length(est))))
colnames(boot) <- names(est)
ci <- apply(boot, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)

is_ratio <- grepl("ratio", names(est))
is_prob  <- grepl("^s1_|diff5", names(est))
to_unit  <- function(x, nm) ifelse(grepl("ratio", nm), x, ifelse(grepl("^s1_|diff5", nm), 100 * x, pct(x)))
tab <- tibble(
  stat = names(est),
  estimate = to_unit(est, names(est)),
  lo = to_unit(ci[1, ], names(est)),
  hi = to_unit(ci[2, ], names(est)),
  unit = ifelse(is_ratio, "x", ifelse(is_prob, "pp", "% vs trend")),
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
    "S4: >=10% shortfalls vs normal, without 1997-98"
  )
)
print(tab, n = Inf, width = Inf)
readr::write_csv(tab, "output/tab_second_look.csv")

# --- Key numbers (appended; 03_analyse.R resets the file first) ---------------------------------
src <- "FAOSTAT QCL rice; NOAA CPC ONI; UCDP/PRIO ACD v26.1; author's calculations (exploratory)"
for (i in seq_len(nrow(tab))) {
  u <- tab$unit[i]; u <- ifelse(u == "% vs trend", "%", u)
  for (part in c("estimate", "lo", "hi")) {
    key <- paste0("sl_", tab$stat[i], if (part == "estimate") "" else paste0("_", part))
    kn_add(key, tab[[part]][i], unit = u, digits = 1,
           description = paste0(tab$description[i], if (part == "estimate") "" else paste0(" (95% CI ", part, ")")),
           source = src, file = kn_file)
  }
}
kn_add("sl_n_conflict_years", sum(panel$major_conflict), unit = "country-years", digits = 0,
       description = "Country-years with major armed conflict (UCDP intensity 2), dropped in S4",
       source = src, file = kn_file)
for (ph in c("Neutral", "El Nino", "La Nina")) {
  key <- paste0("sl_p5_", tolower(gsub(" ", "", ph)), "_pct")
  kn_add(key, 100 * mean(panel$loss5[panel$enso3 == ph]), unit = "%", digits = 0,
         description = paste("Share of >=5% yield shortfalls,", ph, "years"), source = src, file = kn_file)
}
message("03b_second_look.R done.")
