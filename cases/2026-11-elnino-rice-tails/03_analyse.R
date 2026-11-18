# 03_analyse.R — shortfall probabilities, normal benchmark, tails, key numbers ----
# Case: 2026-11-elnino-rice-tails (local folder: AgEcon/Issue 1/)
# Input:  output/cache/panel.rds, output/cache/raw.rds
# Output: output/key_numbers.csv, output/tab_results.csv, output/tab_country.csv,
#         output/tab_fig1.csv, output/tab_fig3.csv, output/sessionInfo.txt
#
# Estimands (fixed in PLAN):
#  E1  Delta P(yield >= 5% below trend | El Niño onset year) - P(... | other years), pp
#  E2  observed frequency of >= 5% / >= 10% shortfalls  /  frequency implied by a normal
#      distribution with each country's typical (robust) variability
# Inference: year-block bootstrap. El Niño hits all six countries in the same year, so shocks
# are correlated across countries within a year; resampling whole years keeps that structure.

stopifnot("Run this script from the issue folder (AgEcon/Issue 1)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})
set.seed(20261118)
B <- 999
kn_file <- "output/key_numbers.csv"
pct <- function(x) 100 * (exp(x) - 1)          # log points -> % change

panel <- readRDS("output/cache/panel.rds")
raw   <- readRDS("output/cache/raw.rds")
src   <- "FAOSTAT QCL (rice, 1961-2024) and NOAA CPC ONI; author's calculations"

# --- Estimators --------------------------------------------------------------------------
lpm <- function(d, y, treat) {
  # linear probability model with country fixed effects; coefficient = change in probability
  f <- stats::as.formula(paste0("as.numeric(", y, ") ~ ", treat, " + factor(iso3)"))
  unname(coef(stats::lm(f, data = d))[paste0(treat, "TRUE")])
}
qr_eff <- function(d, taus = c(0.1, 0.5)) {
  # quantile regression of the yield shock on El Niño with country FE:
  # does El Niño move the bad tail (10th percentile) more than the middle (median)?
  fit <- suppressWarnings(quantreg::rq(shock ~ elnino + factor(iso3), tau = taus, data = d))
  cf <- coef(fit)
  unname(cf["elninoTRUE", ])
}
stats_fun <- function(d) {
  q <- qr_eff(d)
  c(
    p5_other  = mean(d$loss5[!d$elnino]),
    p5_elnino = mean(d$loss5[d$elnino]),
    p5_strong = if (any(d$strong)) mean(d$loss5[d$strong]) else NA_real_,
    diff5     = lpm(d, "loss5", "elnino"),
    diff10    = lpm(d, "loss10", "elnino"),
    p5_obs    = mean(d$loss5),  p5_norm  = mean(d$pnorm5),
    p10_obs   = mean(d$loss10), p10_norm = mean(d$pnorm10),
    ratio5    = mean(d$loss5) / mean(d$pnorm5),
    ratio10   = mean(d$loss10) / mean(d$pnorm10),
    qr10      = q[1], qr50 = q[2],
    r1_diff5  = lpm(d, "loss5", "elnino_lag"),        # R1: exposure in harvest year t+1
    r2_diff5  = lpm(d, "loss5_loess", "elnino")       # R2: LOESS trend
  )
}

# --- Point estimates -----------------------------------------------------------------------
est <- stats_fun(panel)
print(round(est, 4))

# --- Year-block bootstrap --------------------------------------------------------------------
by_year <- split(panel, panel$year)
yrs <- names(by_year)
boot <- t(vapply(seq_len(B), function(b) {
  d <- bind_rows(by_year[sample(yrs, length(yrs), replace = TRUE)])
  tryCatch(stats_fun(d), error = function(e) rep(NA_real_, length(est)))
}, numeric(length(est))))
colnames(boot) <- names(est)
ci <- apply(boot, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)
cat("\nBootstrap draws with errors:", sum(!stats::complete.cases(boot[, -3])), "of", B, "\n")

res <- tibble(stat = names(est), estimate = unname(est), lo = ci[1, ], hi = ci[2, ])
print(res, n = Inf)

# --- Tail shape: Generalised Pareto fit to pooled standardised losses ---------------------
# z = loss / country sigma. For normal shocks the GPD shape xi is ~0 (thin, Gumbel-type tail);
# xi > 0 indicates a heavy tail. Simple MLE, standard errors from the Hessian.
gpd_nll <- function(par, y) {
  s <- exp(par[1]); xi <- par[2]
  if (abs(xi) < 1e-8) return(length(y) * log(s) + sum(y) / s)
  t <- 1 + xi * y / s
  if (any(t <= 0)) return(1e10)
  length(y) * log(s) + (1 + 1 / xi) * sum(log(t))
}
fit_gpd <- function(z, q) {
  u <- stats::quantile(z, q, names = FALSE)
  y <- z[z > u] - u
  o <- stats::optim(c(log(stats::sd(y)), 0.1), gpd_nll, y = y, hessian = TRUE)
  se <- tryCatch(sqrt(diag(solve(o$hessian))), error = function(e) c(NA, NA))
  tibble(threshold_q = q, u = u, n_exc = length(y), xi = o$par[2], se_xi = se[2],
         xi_lo = o$par[2] - 1.96 * se[2], xi_hi = o$par[2] + 1.96 * se[2],
         converged = o$convergence == 0)
}
gpd <- bind_rows(lapply(c(0.80, 0.85, 0.90), function(q) fit_gpd(panel$z_loss, q)))
print(gpd)
gpd_main <- gpd[gpd$threshold_q == 0.85, ]

# --- Where do the worst harvests fall? ---------------------------------------------------------
worst <- panel$shock <= stats::quantile(panel$shock, 0.10)
share_worst_elnino <- mean(panel$elnino[worst])
share_obs_elnino   <- mean(panel$elnino)

# --- Sanity line: drop FAO-estimated ("E") yields -------------------------------------------
noE <- panel |> filter(is.na(flag) | flag != "E")
diff5_noE <- lpm(noE, "loss5", "elnino")

# --- Country table ------------------------------------------------------------------------------
tab_country <- panel |>
  group_by(iso3, country) |>
  summarise(
    years = paste(range(year), collapse = "-"),
    sigma_pct = 100 * first(sigma),
    n_elnino = sum(elnino),
    mean_shock_elnino_pct = pct(mean(shock[elnino])),
    mean_shock_other_pct  = pct(mean(shock[!elnino])),
    p5_elnino_pct = 100 * mean(loss5[elnino]),
    p5_other_pct  = 100 * mean(loss5[!elnino]),
    p5_normal_pct = 100 * mean(pnorm5),
    worst_year = year[which.min(shock)],
    worst_shock_pct = pct(min(shock)),
    .groups = "drop"
  )
print(tab_country, width = Inf)
readr::write_csv(tab_country, "output/tab_country.csv")

tab_results <- bind_rows(
  res |> mutate(type = "bootstrap (year blocks)"),
  gpd |> transmute(stat = paste0("gpd_xi_q", threshold_q), estimate = xi, lo = xi_lo, hi = xi_hi,
                   type = paste0("GPD MLE, n_exc = ", n_exc)),
  tibble(stat = "diff5_noE", estimate = diff5_noE, lo = NA, hi = NA, type = "sanity: drop E flags")
)
readr::write_csv(tab_results, "output/tab_results.csv")

# --- Figure data ----------------------------------------------------------------------------------
tab_fig1 <- tibble(
  group = c("Other years", "El Niño onset years", "Strong El Niño years"),
  p   = 100 * est[c("p5_other", "p5_elnino", "p5_strong")],
  lo  = 100 * ci[1, c("p5_other", "p5_elnino", "p5_strong")],
  hi  = 100 * ci[2, c("p5_other", "p5_elnino", "p5_strong")],
  n   = c(sum(!panel$elnino), sum(panel$elnino), sum(panel$strong)),
  p_normal = 100 * est[["p5_norm"]]
)
readr::write_csv(tab_fig1, "output/tab_fig1.csv")

grid <- seq(0, 0.25, by = 0.005)
tab_fig3 <- bind_rows(lapply(grid, function(x) {
  thr <- log(1 - x)
  tibble(loss_pct = 100 * x,
         observed_all    = mean(panel$shock <= thr),
         observed_elnino = mean(panel$shock[panel$elnino] <= thr),
         normal          = mean(pnorm(thr / panel$sigma)))
}))
readr::write_csv(tab_fig3, "output/tab_fig3.csv")

# --- Key numbers (the only source for text, titles and the LinkedIn post) ---------------------
kn_reset(kn_file)
add <- function(key, value, unit, digits, description, big_mark = ",")
  kn_add(key, value, unit = unit, digits = digits, description = description, source = src,
         big_mark = big_mark, file = kn_file)

add("n_countries", n_distinct(panel$iso3), "countries", 0, "Countries in the pooled panel")
add("n_obs", nrow(panel), "country-years", 0, "Country-year observations")
add("year_start", min(panel$year), "", 0, "First year", big_mark = "")
add("year_end", max(panel$year), "", 0, "Last year", big_mark = "")
add("n_elnino_years", n_distinct(panel$year[panel$elnino]), "years", 0, "El Niño onset years (Jul-Dec ONI >= 0.5)")
add("n_strong_years", n_distinct(panel$year[panel$strong]), "years", 0, "Strong El Niño years (Jul-Dec ONI >= 1.5)")
add("median_sigma_pct", 100 * median(unique(panel$sigma)), "%", 1, "Median country SD of yield shocks (robust)")

add("p5_other_pct", 100 * est[["p5_other"]], "%", 0, "P(yield >= 5% below trend), other years")
add("p5_elnino_pct", 100 * est[["p5_elnino"]], "%", 0, "P(yield >= 5% below trend), El Niño onset years")
add("p5_strong_pct", 100 * est[["p5_strong"]], "%", 0, "P(yield >= 5% below trend), strong El Niño years")
add("diff5_pp", 100 * est[["diff5"]], "pp", 1, "Change in P(>=5% shortfall), El Niño vs other (LPM, country FE)")
add("diff5_lo_pp", 100 * ci[1, "diff5"], "pp", 1, "95% CI lower, year-block bootstrap")
add("diff5_hi_pp", 100 * ci[2, "diff5"], "pp", 1, "95% CI upper, year-block bootstrap")
add("diff10_pp", 100 * est[["diff10"]], "pp", 1, "Change in P(>=10% shortfall), El Niño vs other")
add("diff10_lo_pp", 100 * ci[1, "diff10"], "pp", 1, "95% CI lower")
add("diff10_hi_pp", 100 * ci[2, "diff10"], "pp", 1, "95% CI upper")

add("p5_obs_pct", 100 * est[["p5_obs"]], "%", 1, "Observed share of >=5% shortfalls, all years")
add("p5_normal_pct", 100 * est[["p5_norm"]], "%", 1, "Share implied by normal shocks with each country's sigma")
add("ratio5", est[["ratio5"]], "x", 1, "Observed / normal-implied frequency, >=5% shortfalls")
add("p10_obs_pct", 100 * est[["p10_obs"]], "%", 1, "Observed share of >=10% shortfalls, all years")
add("p10_normal_pct", 100 * est[["p10_norm"]], "%", 2, "Share implied by normal shocks, >=10%")
add("ratio10", est[["ratio10"]], "x", 1, "Observed / normal-implied frequency, >=10% shortfalls")
add("ratio10_lo", ci[1, "ratio10"], "x", 1, "95% CI lower")
add("ratio10_hi", ci[2, "ratio10"], "x", 1, "95% CI upper")

add("qr10_elnino_pct", pct(est[["qr10"]]), "%", 1, "El Niño shift of the 10th percentile of yield shocks")
add("qr10_lo_pct", pct(ci[1, "qr10"]), "%", 1, "95% CI lower")
add("qr10_hi_pct", pct(ci[2, "qr10"]), "%", 1, "95% CI upper")
add("qr50_elnino_pct", pct(est[["qr50"]]), "%", 1, "El Niño shift of the median yield shock")

add("gpd_xi", gpd_main$xi, "", 2, "GPD shape of standardised losses (85th pct threshold); >0 = heavy tail")
add("gpd_xi_lo", gpd_main$xi_lo, "", 2, "95% CI lower (Hessian)")
add("gpd_xi_hi", gpd_main$xi_hi, "", 2, "95% CI upper (Hessian)")
add("gpd_n_exc", gpd_main$n_exc, "exceedances", 0, "Exceedances above the threshold")

add("share_worst_elnino_pct", 100 * share_worst_elnino, "%", 0, "Share of the worst 10% of harvests that fell in El Niño onset years")
add("share_obs_elnino_pct", 100 * share_obs_elnino, "%", 0, "Share of all country-years that are El Niño onset years")

add("r1_diff5_pp", 100 * est[["r1_diff5"]], "pp", 1, "R1: change in P(>=5% shortfall), exposure in t+1")
add("r1_lo_pp", 100 * ci[1, "r1_diff5"], "pp", 1, "R1 95% CI lower")
add("r1_hi_pp", 100 * ci[2, "r1_diff5"], "pp", 1, "R1 95% CI upper")
add("r2_diff5_pp", 100 * est[["r2_diff5"]], "pp", 1, "R2: change in P(>=5% shortfall), LOESS trend")
add("r2_lo_pp", 100 * ci[1, "r2_diff5"], "pp", 1, "R2 95% CI lower")
add("r2_hi_pp", 100 * ci[2, "r2_diff5"], "pp", 1, "R2 95% CI upper")
add("noe_diff5_pp", 100 * diff5_noE, "pp", 1, "Sanity: main estimate without FAO-estimated (E) values")

oni_last <- raw$oni[nrow(raw$oni), ]
add("oni_latest", oni_last$ANOM, "°C", 2,
    paste0("Latest ONI anomaly at access: ", oni_last$SEAS, " ", oni_last$YR))
add("oni_latest_year", oni_last$YR, "", 0, paste("Year of latest ONI season", oni_last$SEAS), big_mark = "")

cat("\nKey numbers:\n"); print(kn_read(kn_file), n = Inf, width = Inf)
write_session_info("output")
message("03_analyse.R done.")
