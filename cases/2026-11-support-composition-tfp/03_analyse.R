# 03_analyse.R — long difference, FE check, robustness, rebalancing arithmetic ------
# Case: 2026-11-support-composition-tfp (local folder: AgEcon/Issue 5/)
# Input:  output/cache/clean.rds
# Output: output/key_numbers.csv, tab_models.csv, tab_loo.csv, tab_rebalancing.csv, sessionInfo.txt

stopifnot("Run this script from the issue folder (AgEcon/Issue 5)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
cl <- readRDS("output/cache/clean.rds")
ld <- cl$ld
fe <- cl$fe
set.seed(20261123)
n_draws <- 9999
src_oecd <- "OECD PSE/CSE database, 2026 edition"
src_both <- "OECD PSE/CSE database, 2026 edition; USDA ERS International Agricultural Productivity, 1961-2023 release"
kn_reset()

# HC3 standard error of one coefficient, for many outcome vectors at once.
# X is fixed, so beta_k = w'y with w = X (X'X)^-1 e_k.
hc3_tools <- function(X, k) {
  B <- solve(crossprod(X))
  w <- drop(X %*% B[, k])
  H <- X %*% B %*% t(X)
  list(w = w, H = H, h = diag(H))
}
hc3_t <- function(tools, Y) {
  U <- Y - tools$H %*% Y
  b <- drop(crossprod(tools$w, Y))
  v <- colSums((tools$w^2 / (1 - tools$h)^2) * U^2)
  b / sqrt(v)
}

# ---- Long difference ----
f_ld <- g_post ~ akis + mps + infra + ln_gdppc + g_pre
m_ld <- lm(f_ld, data = ld)
X_ld <- model.matrix(m_ld)
k_ld <- which(colnames(X_ld) == "akis")
tools_ld <- hc3_tools(X_ld, k_ld)
b_ld <- unname(coef(m_ld)["akis"])
se_ld <- sqrt(sandwich::vcovHC(m_ld, type = "HC3")["akis", "akis"])
t_ld <- b_ld / se_ld
stopifnot(abs(t_ld - hc3_t(tools_ld, matrix(ld$g_post))) < 1e-8)
print(summary(m_ld))

# Freedman-Lane: permute residuals of the model without AKIS, keep the regressors fixed
m_red <- lm(g_post ~ mps + infra + ln_gdppc + g_pre, data = ld)
Y_perm <- replicate(n_draws, fitted(m_red) + sample(resid(m_red)))
t_perm <- hc3_t(tools_ld, Y_perm)
p_perm <- (1 + sum(abs(t_perm) >= abs(t_ld))) / (n_draws + 1)
sd_akis <- sd(ld$akis)
cat(sprintf("\nLong difference: beta1 %.3f (HC3 SE %.3f), permutation p %.3f, n %d\n",
            b_ld, se_ld, p_perm, nrow(ld)))

# ---- R1: leave one country out ----
loo <- bind_rows(lapply(ld$iso3, function(i) {
  m <- lm(f_ld, data = filter(ld, iso3 != i))
  tibble::tibble(dropped = i, beta1 = unname(coef(m)["akis"]),
                 se_hc3 = sqrt(sandwich::vcovHC(m, type = "HC3")["akis", "akis"]))
}))
readr::write_csv(loo, "output/tab_loo.csv")
cat("Leave-one-out beta1 range:", paste(round(range(loo$beta1), 3), collapse = " to "),
    "| most influential:", loo$dropped[which.max(abs(loo$beta1 - b_ld))], "\n")

# ---- R2: without the four economies with large public stockholding ----
ld_r2 <- filter(ld, !iso3 %in% cl$stock_heavy)
m_r2 <- lm(f_ld, data = ld_r2)
b_r2 <- unname(coef(m_r2)["akis"])
se_r2 <- sqrt(sandwich::vcovHC(m_r2, type = "HC3")["akis", "akis"])

# ---- FE short-run check ----
m_fe <- lm(g ~ akis_l + mps_l + infra_l + factor(iso3) + factor(period), data = fe)
b_fe <- unname(coef(m_fe)["akis_l"])
ct <- clubSandwich::coef_test(m_fe, vcov = "CR2", cluster = fe$iso3, test = "Satterthwaite",
                              coefs = "akis_l")
print(ct)
se_fe <- ct$SE[1]
p_fe_cr2 <- ct$p_Satt[1]

# Wild cluster bootstrap-t, restricted (beta1 = 0), Webb six-point weights, CR1 t-statistics
X_fe <- model.matrix(m_fe)
X_fe <- X_fe[, !is.na(coef(m_fe)), drop = FALSE]
k_fe <- which(colnames(X_fe) == "akis_l")
B_fe <- solve(crossprod(X_fe))
w_fe <- drop(X_fe %*% B_fe[, k_fe])
H_fe <- X_fe %*% B_fe %*% t(X_fe)
clus <- fe$iso3
G <- length(unique(clus))
N <- nrow(X_fe)
c_cr1 <- G / (G - 1) * (N - 1) / (N - ncol(X_fe))
cr1_t <- function(Y) {
  U <- Y - H_fe %*% Y
  b <- drop(crossprod(w_fe, Y))
  v <- c_cr1 * colSums(rowsum(w_fe * U, clus)^2)
  b / sqrt(v)
}
t_fe <- cr1_t(matrix(fe$g))
m_fe_r <- lm(g ~ mps_l + infra_l + factor(iso3) + factor(period), data = fe)
webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
ids <- match(clus, sort(unique(clus)))
V <- matrix(sample(webb, G * n_draws, replace = TRUE), nrow = G)
Y_wb <- fitted(m_fe_r) + resid(m_fe_r) * V[ids, ]
t_wb <- cr1_t(Y_wb)
p_fe_wb <- mean(abs(t_wb) >= abs(t_fe))
cat(sprintf("FE: beta1 %.3f (CR2 SE %.3f), Satterthwaite p %.3f, wild bootstrap p %.3f, %d obs, %d countries\n",
            b_fe, se_fe, p_fe_cr2, p_fe_wb, N, G))

models <- tibble::tribble(
  ~model, ~beta1, ~se, ~p, ~p_type, ~n,
  "Long difference (main)", b_ld, se_ld, p_perm, "permutation (Freedman-Lane), HC3 t", nrow(ld),
  "R2: without IND, CHN, IDN, PHL", b_r2, se_r2, NA_real_, "HC3 SE only", nrow(ld_r2),
  "FE short-run check", b_fe, se_fe, p_fe_cr2, "CR2, Satterthwaite df", N,
  "FE short-run check", b_fe, NA_real_, p_fe_wb, "wild cluster bootstrap-t, Webb", N
)
readr::write_csv(models, "output/tab_models.csv")
print(models)

# ---- Rebalancing arithmetic (EU included, 2023-25 vs own 2000-04 intensity) ----
rebal <- cl$rebal
dumb <- cl$dumb
tab_rebal <- rebal |>
  transmute(iso3,
            akis_pct_vp_2000_04 = akis_base,
            akis_pct_vp_2023_25 = akis_now,
            akis_usd_bn_2023_25 = akis_usd / 1e9,
            gap_usd_bn = gap_usd / 1e9,
            gap_pct_of_mps = gap_pct_mps,
            gap_pct_of_gsse = gap_pct_gsse,
            gap_pct_of_budget_support = gap_pct_budget,
            mps_pct_of_tse = mps_pct_tse,
            stock_heavy) |>
  arrange(desc(gap_usd_bn))
tab_rebal <- left_join(tab_rebal, select(dumb, iso3, real_change_pct_2000_04_to_2021_25 = real_change_pct, step),
                       by = "iso3")
readr::write_csv(tab_rebal, "output/tab_rebalancing.csv")
print(tab_rebal, n = 40)
pos_mps <- filter(rebal, mps_usd > 0)
# Country ratios, not only the pooled one: China holds most of the pooled MPS
gap_pos <- filter(rebal, gap_usd > 0)
pos_mps_gap <- filter(pos_mps, gap_usd > 0)
pos_mps_xchn <- filter(pos_mps, iso3 != "CHN")
cat("\nGap as % of own MPS (economies with a gap and MPS > 0): median",
    round(median(pos_mps_gap$gap_pct_mps), 1), "| range",
    paste(round(range(pos_mps_gap$gap_pct_mps), 1), collapse = " to "),
    "| pooled without China", round(100 * sum(pos_mps_xchn$gap_usd) / sum(pos_mps_xchn$mps_usd), 1), "\n")

# ---- Step changes (diagnostic) ----
step_units <- cl$step_units
cat("Units with a one-year jump in the AKIS share of GSSE above 50%:", paste(step_units, collapse = ", "), "\n")
dumb_ns <- filter(dumb, !step)
real_missing <- dumb$iso3[is.na(dumb$real_change_pct)]
cat("Real AKIS fell in:", paste(sort(dumb$iso3[which(dumb$real_change_pct < 0)]), collapse = ", "),
    "| no real comparison:", paste(real_missing, collapse = ", "), "\n")
can_sens <- cl$can_sens

# ---- FE check: where does the within-country variation come from? ----
w_res <- resid(lm(akis_l ~ factor(iso3) + factor(period), data = fe))
fe_infl <- tibble::tibble(iso3 = fe$iso3, w2 = w_res^2) |>
  group_by(iso3) |>
  summarise(share_within_var = 100 * sum(w2), .groups = "drop") |>
  mutate(share_within_var = share_within_var / sum(share_within_var)) |>
  arrange(desc(share_within_var))
readr::write_csv(fe_infl, "output/tab_fe_influence.csv")
print(head(fe_infl, 6))
diag_out <- c("ISL", "CHE", "KAZ", "CAN")
fe_d <- filter(fe, !iso3 %in% diag_out)
m_fe_d <- lm(g ~ akis_l + mps_l + infra_l + factor(iso3) + factor(period), data = fe_d)
ct_d <- clubSandwich::coef_test(m_fe_d, vcov = "CR2", cluster = fe_d$iso3, test = "Satterthwaite",
                                coefs = "akis_l")
print(ct_d)

# ---- Benchmarks against the APME report (printed, not enforced) ----
bench <- cl$ann |>
  filter(iso3 != "OECD", year %in% 2023:2025) |>
  summarise(gsse_pct_vp = 100 * sum(gsse_usd, na.rm = TRUE) / sum(vp[!is.na(gsse_usd)], na.rm = TRUE))
bench_oecd <- cl$ann |>
  filter(iso3 == "OECD", year %in% 2022:2024) |>
  summarise(akis_usd_bn = mean(akis, na.rm = TRUE) / 1e9, akis_pct_gsse = mean(akis_sh, na.rm = TRUE))
cat("\nBenchmark: GSSE % of VP 2023-25, all units", round(bench$gsse_pct_vp, 2),
    "(APME 2026: 2.4%)\n")
bench_all <- cl$ann |>
  filter(iso3 != "OECD", year %in% 2022:2024) |>
  summarise(akis_pct_gsse = 100 * sum(akis, na.rm = TRUE) / sum(gsse_usd[!is.na(akis)], na.rm = TRUE),
            akis_pct_vp = 100 * sum(akis, na.rm = TRUE) / sum(vp[!is.na(akis)], na.rm = TRUE))
cat("Benchmark: OECD AKIS 2022-24", round(bench_oecd$akis_usd_bn, 1), "USD bn,",
    round(bench_oecd$akis_pct_gsse, 1), "% of OECD GSSE (APME 2025: 30% of OECD GSSE)\n")
cat("Benchmark: all units 2022-24, AKIS", round(bench_all$akis_pct_gsse, 1), "% of GSSE and",
    round(bench_all$akis_pct_vp, 2), "% of VP (APME 2025: 22% of GSSE, about 0.5% of VP)\n")

# ---- Key numbers ----
kn_add("n_ld", nrow(ld), "countries", 0, "Countries in the long-difference regression", src_both)
kn_add("n_ld_candidates", nrow(cl$ld_all), "countries", 0, "PSE countries outside the EU block", src_oecd)
kn_add("beta1_ld", b_ld, "pp per pp", 2, "TFP growth (pp a year, 2009-23) per 1 pp of AKIS/VP 2000-09", src_both)
kn_add("se_ld", se_ld, "", 2, "HC3 standard error of beta1, long difference", src_both)
kn_add("p_perm_ld", p_perm, "", 3, "Permutation p-value of beta1 (Freedman-Lane, 9,999 draws)", src_both)
kn_add("sd_akis_ld", sd_akis, "pp", 2, "SD of AKIS/VP 2000-09 across countries", src_oecd)
kn_add("effect_sd_ld", b_ld * sd_akis, "pp", 2, "TFP growth difference per 1 SD of AKIS/VP 2000-09", src_both)
kn_add("effect_01_ld", b_ld * 0.1, "pp", 2, "TFP growth difference per 0.1 pp of AKIS/VP 2000-09", src_both)
kn_add("beta1_loo_min", min(loo$beta1), "", 2, "Smallest beta1, leave one country out", src_both)
kn_add("beta1_loo_max", max(loo$beta1), "", 2, "Largest beta1, leave one country out", src_both)
kn_add("beta1_r2", b_r2, "", 2, "beta1 without IND, CHN, IDN, PHL", src_both)
kn_add("beta1_fe", b_fe, "", 2, "FE short-run check: beta1 (5-year lag)", src_both)
kn_add("p_fe_cr2", p_fe_cr2, "", 3, "FE check: CR2 Satterthwaite p-value", src_both)
kn_add("p_fe_wb", p_fe_wb, "", 3, "FE check: wild cluster bootstrap p-value", src_both)
kn_add("n_fe", N, "country-periods", 0, "Observations in the FE check", src_both)
kn_add("g_post_median", median(ld$g_post), "%", 2, "Median TFP growth 2009-23, % a year", src_both)
kn_add("akis_ld_median", median(ld$akis), "%", 2, "Median AKIS/VP 2000-09", src_oecd)
kn_add("n_dumb", nrow(dumb), "economies", 0, "Economies in the dumbbell (EU included)", src_oecd)
kn_add("n_lower", sum(dumb$akis_now < dumb$akis_base), "economies", 0,
       "Economies with lower AKIS/VP in 2021-25 than in 2000-04", src_oecd)
kn_add("n_higher", sum(dumb$akis_now > dumb$akis_base), "economies", 0,
       "Economies with higher AKIS/VP in 2021-25 than in 2000-04", src_oecd)
kn_add("akis_median_base", median(dumb$akis_base), "%", 2, "Median AKIS/VP 2000-04", src_oecd)
kn_add("akis_median_now", median(dumb$akis_now), "%", 2, "Median AKIS/VP 2021-25", src_oecd)
kn_add("akis_usd_bn_now", sum(rebal$akis_usd) / 1e9, "USD bn", 1, "AKIS spending a year, 2023-25, all units", src_oecd)
kn_add("gap_usd_bn", sum(rebal$gap_usd) / 1e9, "USD bn", 1,
       "Extra AKIS spending a year to return every unit to its 2000-04 intensity", src_oecd)
kn_add("n_gap", sum(rebal$gap_usd > 0), "economies", 0, "Units with a positive gap", src_oecd)
kn_add("gap_pct_mps", 100 * sum(pos_mps$gap_usd) / sum(pos_mps$mps_usd), "%", 1,
       "Gap as % of market price support, units with MPS > 0, 2023-25", src_oecd)
kn_add("mps_usd_bn_pos", sum(pos_mps$mps_usd) / 1e9, "USD bn", 1, "MPS a year, 2023-25, units with MPS > 0", src_oecd)
kn_add("gap_usd_bn_xchn", sum(rebal$gap_usd[rebal$iso3 != "CHN"]) / 1e9, "USD bn", 1,
       "Gap a year without China", src_oecd)
kn_add("gap_usd_bn_chn", sum(rebal$gap_usd[rebal$iso3 == "CHN"]) / 1e9, "USD bn", 1, "Gap a year, China", src_oecd)
kn_add("gap_pct_mps_median", median(pos_mps_gap$gap_pct_mps), "%", 0,
       "Median of gap as % of own MPS, economies with a gap and MPS > 0", src_oecd)
kn_add("gap_pct_mps_min", min(pos_mps_gap$gap_pct_mps), "%", 1, "Smallest gap as % of own MPS", src_oecd)
kn_add("gap_pct_mps_max", max(pos_mps_gap$gap_pct_mps), "%", 0, "Largest gap as % of own MPS", src_oecd)
kn_add("n_gap_mps_half", sum(pos_mps_gap$gap_pct_mps >= 50), "economies", 0,
       "Economies where the gap is at least half of own MPS", src_oecd)
kn_add("gap_pct_mps_xchn", 100 * sum(pos_mps_xchn$gap_usd) / sum(pos_mps_xchn$mps_usd), "%", 1,
       "Gap as % of MPS, pooled, MPS > 0, without China", src_oecd)
kn_add("gap_pct_gsse_median", median(gap_pos$gap_pct_gsse, na.rm = TRUE), "%", 0,
       "Median of gap as % of own GSSE, economies with a gap", src_oecd)
kn_add("gap_pct_budget_median", median(gap_pos$gap_pct_budget, na.rm = TRUE), "%", 0,
       "Median of gap as % of own budgetary support (TSE - MPS), economies with a gap", src_oecd)
kn_add("n_real_fall", sum(dumb$real_change_pct < 0, na.rm = TRUE), "economies", 0,
       "Economies where real AKIS spending (national currency, GDP deflator) fell, 2000-04 to 2021-25",
       paste(src_oecd, "; World Bank WDI GDP deflator"))
kn_add("n_real_avail", sum(!is.na(dumb$real_change_pct)), "economies", 0,
       "Economies with a real AKIS comparison", paste(src_oecd, "; World Bank WDI GDP deflator"))
kn_add("n_step", length(step_units), "economies", 0,
       "Economies with a one-year jump in the AKIS share of GSSE above 50%, 2000-2025", src_oecd)
kn_add("n_lower_nostep", sum(dumb_ns$akis_now < dumb_ns$akis_base), "economies", 0,
       "Economies with lower AKIS/VP in 2021-25 than 2000-04, without step-change economies", src_oecd)
kn_add("n_dumb_nostep", nrow(dumb_ns), "economies", 0, "Economies in the dumbbell without step-change economies", src_oecd)
kn_add("n_real_fall_xeu", sum(dumb$real_change_pct[dumb$iso3 != "EU"] < 0, na.rm = TRUE), "economies", 0,
       "Economies where real AKIS fell, EU excluded", paste(src_oecd, "; World Bank WDI GDP deflator"))
kn_add("n_real_avail_xeu", sum(!is.na(dumb$real_change_pct[dumb$iso3 != "EU"])), "economies", 0,
       "Economies with a real AKIS comparison, EU excluded", paste(src_oecd, "; World Bank WDI GDP deflator"))
kn_add("n_real_rise_ratio_fall", sum(dumb$real_change_pct > 0 & dumb$akis_now < dumb$akis_base, na.rm = TRUE),
       "economies", 0, "Economies where real AKIS rose but its share of output value fell", paste(src_oecd, "; World Bank WDI GDP deflator"))
kn_add("can_gap_usd_bn", rebal$gap_usd[rebal$iso3 == "CAN"] / 1e9, "USD bn", 2, "Canada gap a year", src_oecd)
kn_add("can_gap_pct_mps", rebal$gap_pct_mps[rebal$iso3 == "CAN"], "%", 0, "Canada gap as % of own MPS", src_oecd)
kn_add("can_gap_usd_bn_x03", can_sens$gap_usd / 1e9, "USD bn", 2, "Canada gap a year, base without 2003", src_oecd)
kn_add("can_gap_pct_mps_x03", can_sens$gap_pct_mps, "%", 0, "Canada gap as % of own MPS, base without 2003", src_oecd)
kn_add("n_gap_gsse_over100", sum(rebal$gap_pct_gsse > 100, na.rm = TRUE), "economies", 0,
       "Economies where the gap exceeds their whole current GSSE", src_oecd)
kn_add("fe_top4_within_share", 100 * sum(head(fe_infl$share_within_var, 4)), "%", 0,
       "Share of within-country AKIS variation in the FE check held by the top 4 economies", src_both)
kn_add("beta1_fe_diag", unname(coef(m_fe_d)["akis_l"]), "", 2, "FE check without ISL, CHE, KAZ, CAN (diagnostic)", src_both)
kn_add("p_fe_diag", ct_d$p_Satt[1], "", 3, "FE check without ISL, CHE, KAZ, CAN: CR2 Satterthwaite p", src_both)
kn_add("df_fe_diag", ct_d$df_Satt[1], "", 1, "FE check without ISL, CHE, KAZ, CAN: Satterthwaite df", src_both)
kn_add("df_fe", ct$df_Satt[1], "", 1, "FE check: Satterthwaite df", src_both)
kn_add("beta1_ld_lo95", b_ld - qt(0.975, df = m_ld$df.residual) * se_ld, "", 2,
       "Lower end of the 95% interval of beta1 (HC3, t)", src_both)
kn_add("beta1_ld_hi95", b_ld + qt(0.975, df = m_ld$df.residual) * se_ld, "", 2,
       "Upper end of the 95% interval of beta1 (HC3, t)", src_both)
if ("ISL" %in% loo$dropped) {
  kn_add("beta1_no_isl", loo$beta1[loo$dropped == "ISL"], "", 2, "beta1 without Iceland", src_both)
}
kn_add("n_fe_countries", sum(table(fe$iso3) >= 2), "countries", 0,
       "Countries with two or more periods in the FE check", src_both)
kn_add("bench_akis_pct_vp", bench_all$akis_pct_vp, "%", 2, "AKIS as % of VP 2022-24, all units (check vs APME 2025 about 0.5%)", src_oecd)
kn_add("bench_gsse_pct_vp", bench$gsse_pct_vp, "%", 2, "GSSE as % of VP 2023-25, all units (check vs APME 2.4%)", src_oecd)

# Added-variable data for figure 2: both variables net of the controls, sample means added back
ctrl <- g_post ~ mps + infra + ln_gdppc + g_pre
av <- ld |>
  transmute(iso3,
            akis_adj = resid(lm(update(ctrl, akis ~ .), data = ld)) + mean(ld$akis),
            g_adj = resid(lm(ctrl, data = ld)) + mean(ld$g_post))
stopifnot("FWL slope must equal beta1" = abs(coef(lm(g_adj ~ akis_adj, data = av))[2] - b_ld) < 1e-8)
saveRDS(av, "output/cache/av.rds")

write_session_info()
cat("\nKey numbers written:", nrow(kn_read()), "\n")
