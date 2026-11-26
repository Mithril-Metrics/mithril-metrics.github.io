# 03_analyse.R — retail cooking oil vs world palm oil, by policy phase
# Case: 2026-11-indonesia-palm-oil-exports
# Input: output/cache/analysis_data.rds
# Output: output/key_numbers.csv, output/tab_phase_effects.csv, output/tab_models.csv,
#         output/tab_monthly_gaps.csv, output/cache/analysis_data_fitted.rds, output/sessionInfo.txt

stopifnot("Run this script from the issue folder" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(sandwich)
  library(lmtest)
})
set.seed(20261126)
kn_file <- "output/key_numbers.csv"
df <- readRDS("output/cache/analysis_data.rds")

# Estimand: shift of the log retail price, per phase, conditional on the world price in rupiah
# (lags 0-2). Retail cooking oil follows CPO with a lag and less than one-for-one (refining,
# packaging and distribution margins), which is why the price is not simply differenced.

# ---- Helpers ----
ar_fit <- function(y, x, d = df) {
  arima(d[[y]], order = c(1, 0, 0), xreg = as.matrix(d[, x]), method = "ML")
}

tidy_ar <- function(m) {
  b <- coef(m)
  se <- sqrt(diag(m$var.coef))
  tibble(term = names(b), est = unname(b), se = unname(se)) |>
    mutate(p = 2 * pnorm(-abs(est / se)))
}

# OLS with Newey-West SE. Its estimate differs from the AR(1) one, so it is kept as est_ols
fit_nw <- function(y, x, d = df) {
  m <- lm(reformulate(x, response = y), data = d)
  ct <- coeftest(m, vcov = NeweyWest(m, lag = 3, prewhite = TRUE))
  tibble(term = rownames(ct), est_ols = ct[, 1], se_nw = ct[, 2], p_nw = ct[, 4])
}

run_model <- function(label, y, x) {
  tidy_ar(ar_fit(y, x)) |>
    left_join(fit_nw(y, x), by = "term") |>
    mutate(model = label, .before = 1)
}

pt_sum <- function(m, terms) {
  c(sum = sum(coef(m)[terms]), se = sqrt(sum(m$var.coef[terms, terms])))
}

x_lags <- c("ln_cpo", "ln_cpo_l1", "ln_cpo_l2")
x_oil <- c(x_lags, "window", "post", "bps2023", "eid")
x_flour <- c("ln_wheat", "ln_wheat_l1", "ln_wheat_l2", "window", "post", "bps2023", "eid")
x_r1 <- c(x_lags, "window_a", "window_b", "post", "bps2023", "eid")

res <- bind_rows(
  run_model("main", "ln_vegoil", x_oil),
  run_model("placebo_flour", "ln_flour", x_flour),
  run_model("r1_split", "ln_vegoil", x_r1)
)

# Holm over the three phase terms of the main model; gamma_window is the primary test
main_phase <- res$model == "main" & res$term %in% c("window", "post", "bps2023")
res$p_holm <- NA_real_
res$p_holm[main_phase] <- p.adjust(res$p[main_phase], method = "holm")

pre_mean <- mean(df$vegoil[df$date >= as.Date("2021-01-01") & df$date <= as.Date("2021-12-01")])

res <- res |>
  mutate(
    pct = 100 * (exp(est) - 1),
    pct_lo = 100 * (exp(est - 1.96 * se) - 1),
    pct_hi = 100 * (exp(est + 1.96 * se) - 1),
    pct_ols = 100 * (exp(est_ols) - 1),
    idr_l = if_else(model %in% c("main", "r1_split"), pct / 100 * pre_mean, NA_real_)
  )

phase_terms <- c("window", "window_a", "window_b", "post", "bps2023")
tab_phase <- res |>
  filter(term %in% phase_terms) |>
  select(model, term, est, se, p, p_holm, pct, pct_lo, pct_hi, idr_l, pct_ols, se_nw, p_nw)
write.csv(res, "output/tab_models.csv", row.names = FALSE)
write.csv(tab_phase, "output/tab_phase_effects.csv", row.names = FALSE)
print(as.data.frame(tab_phase), digits = 3)

# ---- Pass-through and persistence ----
main_ar <- ar_fit("ln_vegoil", x_oil)
flour_ar <- ar_fit("ln_flour", x_flour)
lr <- pt_sum(main_ar, x_lags)
lr_flour <- pt_sum(flour_ar, c("ln_wheat", "ln_wheat_l1", "ln_wheat_l2"))
ar1 <- coef(main_ar)["ar1"]
ar1_flour <- coef(flour_ar)["ar1"]
cat(sprintf("Cooking oil: pass-through %.3f (SE %.3f), AR(1) %.2f\n", lr["sum"], lr["se"], ar1))
cat(sprintf("Wheat flour: pass-through %.3f (SE %.3f), AR(1) %.2f\n", lr_flour["sum"], lr_flour["se"], ar1_flour))
if (lr["sum"] <= 0 || lr["sum"] >= 1.2) message("Check: pass-through outside the expected range (0, 1).")
if (ar1 > 0.95) message("Check: AR(1) coefficient above 0.95; level effects are hard to separate from drift.")

# Level implied by the world price alone (phase dummies and AR term switched off)
b <- coef(main_ar)
x_cf <- c(x_lags, "eid", "bps2023")
df$cf_vegoil <- exp(b["intercept"] + drop(as.matrix(df[, x_cf]) %*% b[x_cf]))
df$gap_main <- df$ln_vegoil - log(df$cf_vegoil)

# Same, with the link estimated without 2022 (S2; 2022 is skipped in the AR(1) likelihood)
df$y_ex2022 <- if_else(format(df$date, "%Y") == "2022", NA_real_, df$ln_vegoil)
ex_ar <- ar_fit("y_ex2022", x_cf)
b_ex <- coef(ex_ar)
lr_ex <- pt_sum(ex_ar, x_lags)
df$cf_vegoil_ex <- exp(b_ex["intercept"] + drop(as.matrix(df[, x_cf]) %*% b_ex[x_cf]))
df$gap_ex <- df$ln_vegoil - log(df$cf_vegoil_ex)
cat(sprintf("Link without 2022: pass-through %.3f (SE %.3f)\n", lr_ex["sum"], lr_ex["se"]))
saveRDS(df, "output/cache/analysis_data_fitted.rds")

gaps <- df |>
  transmute(date, retail = vegoil, implied = cf_vegoil, implied_ex2022 = cf_vegoil_ex,
            gap_pct = 100 * (exp(gap_main) - 1), gap_ex2022_pct = 100 * (exp(gap_ex) - 1))
write.csv(gaps, "output/tab_monthly_gaps.csv", row.names = FALSE)

# Mean gap over a period, in %
period_gap <- function(g, from, to) {
  sel <- df$date >= as.Date(from) & df$date <= as.Date(to)
  100 * (exp(mean(g[sel])) - 1)
}
periods <- list(
  early = c("2020-11-01", "2021-06-01", "Nov 2020 - Jun 2021"),
  q4_2021 = c("2021-10-01", "2021-12-01", "Oct-Dec 2021"),
  janmar = c("2022-01-01", "2022-03-01", "Jan-Mar 2022"),
  febmar = c("2022-02-01", "2022-03-01", "Feb-Mar 2022 (retail caps in force)"),
  aprmay = c("2022-04-01", "2022-05-01", "Apr-May 2022"),
  after = c("2022-06-01", "2026-07-01", "Jun 2022 - Jul 2026")
)
gap_tab <- bind_rows(lapply(names(periods), function(n) {
  p <- periods[[n]]
  tibble(period = n, label = p[3], gap_main = period_gap(df$gap_main, p[1], p[2]),
         gap_ex2022 = period_gap(df$gap_ex, p[1], p[2]))
}))
print(as.data.frame(gap_tab), digits = 3)

# ---- Descriptives ----
in_win <- df$window == 1
in_2021 <- df$date >= as.Date("2021-01-01") & df$date <= as.Date("2021-12-01")
peak <- df[which.max(df$vegoil), ]
chg_oil <- 100 * (mean(df$vegoil[in_win]) / pre_mean - 1)
chg_cpo <- 100 * (mean(df$cpo_idr[in_win]) / mean(df$cpo_idr[in_2021]) - 1)

# ---- Key numbers ----
g <- function(m, t, col) res[[col]][res$model == m & res$term == t]
src_fpma <- "FAO GIEWS FPMA (BPS retail prices; Oil World CPO c.i.f. Rotterdam; FPMA IDR/USD)"
src_model <- paste(src_fpma, "- AR(1) regression, 03_analyse.R")
add <- function(key, value, unit = "", digits = 1, description = "", source = src_model) {
  kn_add(key, value, unit = unit, digits = digits, description = description, source = source, file = kn_file)
}

kn_reset(kn_file)
add("n_months", nrow(df), "months", 0, "Months in the estimation sample (Nov 2020 - Jul 2026)", src_fpma)
add("vegoil_mean_2021", pre_mean, "IDR/litre", 0, "Mean national retail cooking-oil price, 2021", src_fpma)
add("vegoil_peak", peak$vegoil, "IDR/litre", 0,
    paste("Highest monthly retail cooking-oil price in the sample,", format(peak$date, "%b %Y")), src_fpma)
add("vegoil_window_vs_2021_pct", chg_oil, "%", 1, "Mean retail cooking-oil price in Jan-May 2022 vs 2021 mean", src_fpma)
add("cpo_window_vs_2021_pct", chg_cpo, "%", 1, "Mean world CPO price in rupiah in Jan-May 2022 vs 2021 mean", src_fpma)

for (t in c("window", "post", "bps2023")) {
  add(paste0("main_", t, "_pct"), g("main", t, "pct"), "%", 1, paste("Main model: retail shift vs world-price level,", t))
  add(paste0("main_", t, "_lo"), g("main", t, "pct_lo"), "%", 1, paste("Main model: lower 95% bound,", t))
  add(paste0("main_", t, "_hi"), g("main", t, "pct_hi"), "%", 1, paste("Main model: upper 95% bound,", t))
  add(paste0("main_", t, "_p_holm"), g("main", t, "p_holm"), "", 3, paste("Main model: Holm-adjusted p-value (AR(1) SE),", t))
}
add("main_window_ols_pct", g("main", "window", "pct_ols"), "%", 1, "Check: OLS estimate of the window shift (Newey-West SE)")
add("main_window_ols_p_nw", g("main", "window", "p_nw"), "", 3, "Check: Newey-West p-value of the OLS window estimate")
add("main_window_idr", g("main", "window", "idr_l"), "IDR/litre", 0, "Main model: window shift in IDR/litre at the 2021 mean price")
add("main_post_idr", g("main", "post", "idr_l"), "IDR/litre", 0, "Main model: shift from Jun 2022 in IDR/litre at the 2021 mean price")
add("passthrough_sum", lr["sum"], "", 2, "Sum of CPO elasticities, lags 0-2 (main model)")
add("passthrough_se", lr["se"], "", 2, "SE of the sum of CPO elasticities (main model)")
add("ar1_main", ar1, "", 2, "AR(1) coefficient of the main model errors")

for (t in c("window_a", "window_b", "post")) {
  lab <- c(window_a = "Jan-Mar 2022", window_b = "Apr-May 2022", post = "from Jun 2022")[t]
  add(paste0("r1_", t, "_pct"), g("r1_split", t, "pct"), "%", 1, paste("R1 split: shift,", lab))
  add(paste0("r1_", t, "_lo"), g("r1_split", t, "pct_lo"), "%", 1, paste("R1 split: lower 95% bound,", lab))
  add(paste0("r1_", t, "_hi"), g("r1_split", t, "pct_hi"), "%", 1, paste("R1 split: upper 95% bound,", lab))
  add(paste0("r1_", t, "_p"), g("r1_split", t, "p"), "", 3, paste("R1 split: p-value (AR(1) SE),", lab))
}

add("placebo_window_pct", g("placebo_flour", "window", "pct"), "%", 1, "Placebo (wheat flour): window shift")
add("placebo_window_p", g("placebo_flour", "window", "p"), "", 3, "Placebo (wheat flour): window p-value (AR(1) SE)")
add("flour_passthrough_sum", lr_flour["sum"], "", 2, "Placebo: sum of world-wheat elasticities, lags 0-2")
add("flour_ar1", ar1_flour, "", 2, "Placebo: AR(1) coefficient of the errors")

src_gap <- paste(src_fpma, "- gap to the AR(1) world-price link, 03_analyse.R")
for (i in seq_len(nrow(gap_tab))) {
  add(paste0("gap_", gap_tab$period[i], "_pct"), gap_tab$gap_main[i], "%", 1,
      paste("Mean gap, retail vs world-price level (main link),", gap_tab$label[i]), src_gap)
  add(paste0("gap_", gap_tab$period[i], "_ex2022_pct"), gap_tab$gap_ex2022[i], "%", 1,
      paste("Mean gap, retail vs world-price level (link estimated without 2022),", gap_tab$label[i]), src_gap)
}
add("passthrough_ex2022", lr_ex["sum"], "", 2, "S2: sum of CPO elasticities, link estimated without 2022")
add("passthrough_ex2022_se", lr_ex["se"], "", 2, "S2: SE of that sum")

print(kn_read(kn_file)[, c("key", "value", "description")], n = Inf, width = Inf)
write_session_info("output")
