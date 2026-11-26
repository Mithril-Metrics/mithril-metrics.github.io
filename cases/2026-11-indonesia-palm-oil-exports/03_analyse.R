# 03_analyse.R — retail cooking oil vs world palm oil parity, by policy phase
# Case: 2026-11-indonesia-palm-oil-exports
# Input: output/cache/analysis_data.rds
# Output: output/key_numbers.csv, output/tab_phase_effects.csv, output/tab_models.csv, output/sessionInfo.txt

stopifnot("Run this script from the issue folder (AgEcon/Issue 6)" = file.exists("00_meta.yml"))
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
fit_ar1 <- function(y, x) {
  m <- arima(df[[y]], order = c(1, 0, 0), xreg = as.matrix(df[, x]), method = "ML")
  b <- coef(m)
  se <- sqrt(diag(m$var.coef))
  tibble(term = names(b), est = unname(b), se = unname(se)) |>
    mutate(p = 2 * pnorm(-abs(est / se)))
}

fit_nw <- function(y, x) {
  f <- reformulate(x, response = y)
  m <- lm(f, data = df)
  ct <- coeftest(m, vcov = NeweyWest(m, lag = 3, prewhite = TRUE))
  tibble(term = rownames(ct), est_nw = ct[, 1], se_nw = ct[, 2], p_nw = ct[, 4])
}

run_model <- function(label, y, x) {
  fit_ar1(y, x) |>
    left_join(fit_nw(y, x), by = "term") |>
    mutate(model = label, .before = 1)
}

x_oil <- c("ln_cpo", "ln_cpo_l1", "ln_cpo_l2", "window", "post", "bps2023", "eid")
x_flour <- c("ln_wheat", "ln_wheat_l1", "ln_wheat_l2", "window", "post", "bps2023", "eid")
x_r1 <- c("ln_cpo", "ln_cpo_l1", "ln_cpo_l2", "window_a", "window_b", "post", "bps2023", "eid")
x_r2 <- c("window", "post", "bps2023", "eid")

res <- bind_rows(
  run_model("main", "ln_vegoil", x_oil),
  run_model("placebo_flour", "ln_flour", x_flour),
  run_model("r1_split", "ln_vegoil", x_r1),
  run_model("r2_gap_diff", "gap_diff", x_r2)
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
    idr_l = if_else(model %in% c("main", "r1_split"), pct / 100 * pre_mean, NA_real_)
  )

phase_terms <- c("window", "window_a", "window_b", "post", "bps2023")
tab_phase <- res |>
  filter(term %in% phase_terms) |>
  select(model, term, est, se, p, p_holm, se_nw, p_nw, pct, pct_lo, pct_hi, idr_l)
write.csv(res, "output/tab_models.csv", row.names = FALSE)
write.csv(tab_phase, "output/tab_phase_effects.csv", row.names = FALSE)
print(as.data.frame(tab_phase), digits = 3)

# ---- Pass-through check (should be positive and below one) ----
main_ar <- arima(df$ln_vegoil, order = c(1, 0, 0), xreg = as.matrix(df[, x_oil]), method = "ML")
lr_terms <- c("ln_cpo", "ln_cpo_l1", "ln_cpo_l2")
lr <- sum(coef(main_ar)[lr_terms])
lr_se <- sqrt(sum(main_ar$var.coef[lr_terms, lr_terms]))
ar1 <- coef(main_ar)["ar1"]
cat(sprintf("Sum of CPO elasticities (lags 0-2): %.3f (SE %.3f); AR(1) coefficient %.2f\n", lr, lr_se, ar1))
if (lr <= 0 || lr >= 1.2) message("Check: pass-through outside the expected range (0, 1).")
# A near-unit-root error makes level effects hard to separate from drift; flag for P6
if (ar1 > 0.95) message("Check: AR(1) coefficient above 0.95; discuss at P6 before reading the phase effects.")

# Path implied by the world price alone (phase dummies and AR term switched off), for Fig 1
b <- coef(main_ar)
df$cf_vegoil <- exp(b["intercept"] + b["ln_cpo"] * df$ln_cpo + b["ln_cpo_l1"] * df$ln_cpo_l1 +
                      b["ln_cpo_l2"] * df$ln_cpo_l2 + b["eid"] * df$eid + b["bps2023"] * df$bps2023)
saveRDS(df, "output/cache/analysis_data_fitted.rds")

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

kn_reset(kn_file)
kn_add("n_months", nrow(df), unit = "months", digits = 0,
       description = "Months in the estimation sample (Nov 2020 - Jul 2026)", source = src_fpma, file = kn_file)
kn_add("vegoil_mean_2021", pre_mean, unit = "IDR/litre", digits = 0,
       description = "Mean national retail cooking-oil price, 2021", source = src_fpma, file = kn_file)
kn_add("vegoil_peak", peak$vegoil, unit = "IDR/litre", digits = 0,
       description = paste("Highest monthly retail cooking-oil price in the sample,", format(peak$date, "%b %Y")),
       source = src_fpma, file = kn_file)
kn_add("vegoil_window_vs_2021_pct", chg_oil, unit = "%", digits = 1,
       description = "Mean retail cooking-oil price in Jan-May 2022 vs 2021 mean", source = src_fpma, file = kn_file)
kn_add("cpo_window_vs_2021_pct", chg_cpo, unit = "%", digits = 1,
       description = "Mean world CPO price in rupiah in Jan-May 2022 vs 2021 mean", source = src_fpma, file = kn_file)

for (t in c("window", "post", "bps2023")) {
  kn_add(paste0("main_", t, "_pct"), g("main", t, "pct"), unit = "%", digits = 1,
         description = paste("Main model: retail price shift vs world-parity path,", t), source = src_model, file = kn_file)
  kn_add(paste0("main_", t, "_lo"), g("main", t, "pct_lo"), unit = "%", digits = 1,
         description = paste("Main model: lower 95% bound,", t), source = src_model, file = kn_file)
  kn_add(paste0("main_", t, "_hi"), g("main", t, "pct_hi"), unit = "%", digits = 1,
         description = paste("Main model: upper 95% bound,", t), source = src_model, file = kn_file)
  kn_add(paste0("main_", t, "_p_holm"), g("main", t, "p_holm"), unit = "", digits = 3,
         description = paste("Main model: Holm-adjusted p-value (AR(1) SE),", t), source = src_model, file = kn_file)
  kn_add(paste0("main_", t, "_p_nw"), g("main", t, "p_nw"), unit = "", digits = 3,
         description = paste("Main model: Newey-West p-value (check),", t), source = src_model, file = kn_file)
}
kn_add("main_window_idr", g("main", "window", "idr_l"), unit = "IDR/litre", digits = 0,
       description = "Main model: window shift in IDR/litre at the 2021 mean price", source = src_model, file = kn_file)
kn_add("main_post_idr", g("main", "post", "idr_l"), unit = "IDR/litre", digits = 0,
       description = "Main model: Jun-Dec 2022 shift in IDR/litre at the 2021 mean price", source = src_model, file = kn_file)
kn_add("passthrough_sum", lr, unit = "", digits = 2,
       description = "Sum of CPO elasticities, lags 0-2 (main model)", source = src_model, file = kn_file)
kn_add("placebo_window_pct", g("placebo_flour", "window", "pct"), unit = "%", digits = 1,
       description = "Placebo (wheat flour): window shift", source = src_model, file = kn_file)
kn_add("placebo_window_p", g("placebo_flour", "window", "p"), unit = "", digits = 3,
       description = "Placebo (wheat flour): window p-value (AR(1) SE)", source = src_model, file = kn_file)
kn_add("r1_window_a_pct", g("r1_split", "window_a", "pct"), unit = "%", digits = 1,
       description = "R1: shift in Jan-Mar 2022 (DMO + full caps)", source = src_model, file = kn_file)
kn_add("r1_window_b_pct", g("r1_split", "window_b", "pct"), unit = "%", digits = 1,
       description = "R1: shift in Apr-May 2022 (bulk cap + export ban)", source = src_model, file = kn_file)
kn_add("r2_window_pct", g("r2_gap_diff", "window", "pct"), unit = "%", digits = 1,
       description = "R2: window shift of the cooking-oil gap net of the wheat-flour gap", source = src_model, file = kn_file)
kn_add("r2_window_p", g("r2_gap_diff", "window", "p"), unit = "", digits = 3,
       description = "R2: window p-value (AR(1) SE)", source = src_model, file = kn_file)

print(kn_read(kn_file), n = Inf, width = Inf)
write_session_info("output")
