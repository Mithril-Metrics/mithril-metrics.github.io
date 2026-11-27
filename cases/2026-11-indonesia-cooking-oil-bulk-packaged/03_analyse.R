# 03_analyse.R - weekly event study of the packaged-bulk price gap, heterogeneity by producer
# provinces, 2017-2021 time placebo, bulk prices against the cap
# Input: output/cache/*.rds, 00_meta.yml   Output: output/tab_*.csv, output/key_numbers.csv

stopifnot("Run this script from the issue folder (AgEcon/Issue 7)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
  library(clubSandwich)
})
meta <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")
dec <- meta$decisions
panel <- readRDS("output/cache/panel.rds")
national <- readRDS("output/cache/national_weekly.rds")
monthly <- readRDS("output/cache/monthly.rds")
no_bulk <- readRDS("output/cache/no_bulk.rds")
kn_reset()
src <- "Bank Indonesia PIHPS, traditional markets"
pct <- function(x) 100 * (exp(x) - 1)
`%||%` <- function(a, b) if (is.null(a)) b else a
krange <- function(r) seq(r[[1]], r[[2]])

# ---- Event-study design ----
# One dummy per event week except the reference week (k = -1, week of 7 Mar 2022).
# The producer interaction gets its own set of week dummies, so each group keeps its own path
# and gamma_k compares producer with other provinces relative to the same reference week.
ks <- sort(setdiff(unique(panel$k), dec$ref_k))
kname <- function(k, pre = "e") paste0(pre, ifelse(k < 0, paste0("m", abs(k)), k))
for (k in ks) {
  panel[[kname(k)]] <- as.integer(panel$k == k)
  panel[[kname(k, "p")]] <- panel[[kname(k)]] * panel$producer
}
ev <- kname(ks)
evp <- kname(ks, "p")

# Weeks with no price in any province (holiday weeks) have all-zero dummies and are left out.
# In the producer model a week reported only by producer provinces gives two identical dummies:
# lm flags one as NA, and it is dropped before the clustered errors are computed.
fit_es <- function(y, terms, data = panel) {
  d <- data[!is.na(data[[y]]), ]
  terms <- terms[vapply(terms, function(t) any(d[[t]] != 0), logical(1))]
  m <- lm(reformulate(c("factor(province)", terms), response = y), data = d)
  aliased <- intersect(names(coef(m))[is.na(coef(m))], terms)
  if (length(aliased)) {
    message("Dropped collinear dummies: ", paste(aliased, collapse = ", "))
    terms <- setdiff(terms, aliased)
    m <- lm(reformulate(c("factor(province)", terms), response = y), data = d)
  }
  v <- vcovCR(m, cluster = d$province, type = "CR2")
  list(model = m, vcov = v, data = d, terms = terms)
}

# Average of event-week coefficients over a window, CR2 + Satterthwaite
window_avg <- function(fit, k_win, pre = "e", minus = NULL) {
  b <- coef(fit$model)
  w <- setNames(numeric(length(b)), names(b))
  on <- intersect(kname(k_win, pre), names(b))
  w[on] <- 1 / length(on)
  if (!is.null(minus)) {
    off <- intersect(kname(minus, pre), names(b))
    w[off] <- w[off] - 1 / length(off)
  }
  lc <- linear_contrast(fit$model, vcov = fit$vcov, contrasts = matrix(w, nrow = 1),
                        test = "Satterthwaite", p_values = TRUE)
  tibble(est = lc$Est, se = lc$SE, df = lc$df, lo = lc$CI_L, hi = lc$CI_U, p = lc$p_val)
}

# P7: window average minus a straight line fitted through the caps-period weeks and the reference
# week (beta = 0), carried forward. The line is linear in the coefficients, so the result is one
# linear contrast with CR2 errors. It treats the February drift as continuing: a lower bound.
detrended_avg <- function(fit, k_win, k_trend) {
  b <- coef(fit$model)
  kt <- c(k_trend[kname(k_trend) %in% names(b)], dec$ref_k)
  kbar <- mean(kt)
  sxx <- sum((kt - kbar)^2)
  fitted_w <- function(x) {
    # weight of each beta_j in the fitted line at week x (the reference beta is 0 and drops out)
    w <- 1 / length(kt) + (x - kbar) * (kt - kbar) / sxx
    setNames(w, kname(kt))[kname(kt) != kname(dec$ref_k)]
  }
  on <- intersect(kname(k_win), names(b))
  w <- setNames(numeric(length(b)), names(b))
  w[on] <- 1 / length(on)
  k_on <- k_win[kname(k_win) %in% on]
  ft <- Reduce(`+`, lapply(k_on, fitted_w)) / length(k_on)
  w[names(ft)] <- w[names(ft)] - ft
  lc <- linear_contrast(fit$model, vcov = fit$vcov, contrasts = matrix(w, nrow = 1),
                        test = "Satterthwaite", p_values = TRUE)
  tibble(est = lc$Est, se = lc$SE, df = lc$df, lo = lc$CI_L, hi = lc$CI_U, p = lc$p_val,
         slope_pp_week = 100 * sum(((kt - kbar) / sxx) * c(b[kname(kt[kt != dec$ref_k])], 0)))
}

event_table <- function(fit, label, pre = "e") {
  cf <- intersect(kname(ks, pre), fit$terms)
  ct <- conf_int(fit$model, vcov = fit$vcov, coefs = cf, test = "Satterthwaite")
  tibble(k = ks[kname(ks, pre) %in% cf], outcome = label, beta = ct$beta, lo = ct$CI_L,
         hi = ct$CI_U) |>
    bind_rows(tibble(k = dec$ref_k, outcome = label, beta = 0, lo = 0, hi = 0)) |>
    arrange(k)
}

# ---- Main model: log gap, packaged 1 minus bulk ----
es_gap <- fit_es("gap", ev)
es_pkg <- fit_es("ln_pkg1", ev, data = filter(panel, !is.na(gap)))
es_bulk <- fit_es("ln_bulk", ev, data = filter(panel, !is.na(gap)))
es_gap2 <- fit_es("gap2", ev)
es_jump <- fit_es("gap_jump", ev)
es_drop <- fit_es("gap", ev, data = filter(panel, province != dec$drop_province_check))

wk_dates <- tibble(k = c(dec$ref_k, ks)) |>
  mutate(week = as.Date(dec$event_week) + 7 * k)
tab_es <- bind_rows(
  event_table(es_gap, "gap"),
  event_table(es_pkg, "packaged"),
  event_table(es_bulk, "bulk")
) |>
  left_join(wk_dates, by = "k") |>
  mutate(across(c(beta, lo, hi), pct, .names = "{.col}_pct"))
readr::write_csv(tab_es, "output/tab_event_study.csv")

pre_k <- krange(dec$precap_k)
cap_k <- krange(dec$cap_k)
win <- list(post_preban = krange(dec$post_preban_k), post_ban = krange(dec$post_ban_k))

conf <- bind_rows(
  window_avg(es_gap, win$post_preban) |> mutate(test = "beta_post_preban"),
  window_avg(es_gap, win$post_ban) |> mutate(test = "beta_post_ban")
)
extra <- bind_rows(
  window_avg(es_gap, win$post_preban, minus = pre_k) |> mutate(test = "post_preban_vs_precap"),
  window_avg(es_gap, win$post_ban, minus = pre_k) |> mutate(test = "post_ban_vs_precap"),
  window_avg(es_pkg, win$post_preban) |> mutate(test = "pkg_post_preban"),
  window_avg(es_bulk, win$post_preban) |> mutate(test = "bulk_post_preban"),
  window_avg(es_pkg, win$post_ban) |> mutate(test = "pkg_post_ban"),
  window_avg(es_bulk, win$post_ban) |> mutate(test = "bulk_post_ban"),
  window_avg(es_gap2, win$post_preban) |> mutate(test = "pkg2_post_preban"),
  window_avg(es_gap2, win$post_ban) |> mutate(test = "pkg2_post_ban"),
  window_avg(es_jump, win$post_preban) |> mutate(test = "jumprule_post_preban"),
  window_avg(es_jump, win$post_ban) |> mutate(test = "jumprule_post_ban"),
  window_avg(es_drop, win$post_preban) |> mutate(test = "nosultra_post_preban"),
  window_avg(es_drop, win$post_ban) |> mutate(test = "nosultra_post_ban"),
  detrended_avg(es_gap, win$post_preban, cap_k) |> mutate(test = "drift_post_preban"),
  detrended_avg(es_gap, win$post_ban, cap_k) |> mutate(test = "drift_post_ban")
)

# Within the caps period the gap should be flat if the reference week is representative
cap_terms <- intersect(kname(cap_k), es_gap$terms)
cap_test <- Wald_test(es_gap$model, constraints = constrain_zero(cap_terms),
                      vcov = es_gap$vcov, test = "HTZ")

# ---- Heterogeneity: producer provinces ----
es_het <- fit_es("gap", c(ev, evp))
gam <- bind_rows(
  window_avg(es_het, win$post_preban, pre = "p") |> mutate(test = "gamma_post_preban"),
  window_avg(es_het, win$post_ban, pre = "p") |> mutate(test = "gamma_post_ban")
)
het_terms <- intersect(kname(cap_k, "p"), es_het$terms)
pre_het <- Wald_test(es_het$model, constraints = constrain_zero(het_terms),
                     vcov = es_het$vcov, test = "HTZ")
conf <- bind_rows(conf, gam[1, ]) |>
  mutate(p_holm = p.adjust(p, method = "holm"))

summary_tab <- bind_rows(conf |> mutate(role = "confirmatory"),
                         gam[2, ] |> mutate(role = "secondary"),
                         extra |> mutate(role = "descriptive")) |>
  mutate(est_pct = pct(est), lo_pct = pct(lo), hi_pct = pct(hi))
readr::write_csv(summary_tab, "output/tab_summary.csv")
print(summary_tab |> select(test, role, est_pct, lo_pct, hi_pct, p, df), n = 30)
cat(sprintf("Caps-period joint test (gap flat 31 Jan - 28 Feb): F = %.2f, p = %.3f\n",
            cap_test$Fstat, cap_test$p_val))
cat(sprintf("Producer pre-switch difference, caps period: F = %.2f, p = %.3f\n",
            pre_het$Fstat, pre_het$p_val))

# ---- Time placebo, monthly national gap ----
# How unusual is the 2022 change compared with every change of the same length in 2017-2021?
pl <- monthly |>
  filter(month >= as.Date(paste0(dec$placebo_months[[1]], "-01")),
         month <= as.Date(paste0(dec$placebo_months[[2]], "-01")),
         !is.na(gap))
ram <- as.Date(paste0(unlist(dec$ramadan_months), "-01"))
add_months <- function(d, h) {
  out <- vapply(seq_along(d), function(i) {
    as.numeric(seq(d[i], by = "month", length.out = h + 1)[h + 1])
  }, numeric(1))
  as.Date(out, origin = "1970-01-01")
}
base_m <- as.Date(paste0(dec$placebo_base, "-01"))
base <- monthly$gap[monthly$month == base_m]

placebo <- bind_rows(lapply(unlist(dec$placebo_targets), function(tg) {
  m_tg <- as.Date(paste0(tg, "-01"))
  h <- length(seq(base_m, m_tg, by = "month")) - 1
  actual <- monthly$gap[monthly$month == m_tg] - base
  ch <- tibble(start = pl$month, end = add_months(pl$month, h)) |>
    inner_join(select(pl, start = month, g0 = gap), by = "start") |>
    inner_join(select(pl, end = month, g1 = gap), by = "end") |>
    mutate(change = g1 - g0)
  # a change "spans Ramadan" if any month after its start, up to its end, is a Ramadan month
  ch$ramadan <- vapply(seq_len(nrow(ch)), function(i) {
    any(seq(ch$start[i], by = "month", length.out = h + 1)[-1] %in% ram)
  }, logical(1))
  tibble(
    target = tg, horizon = h, actual_pp = 100 * actual, n = nrow(ch),
    max_pp = 100 * max(ch$change), sd_pp = 100 * sd(ch$change),
    pctile = 100 * mean(ch$change < actual),
    p_upper = mean(ch$change >= actual),
    p_abs = mean(abs(ch$change) >= abs(actual)),
    n_ramadan = sum(ch$ramadan),
    p_upper_ramadan = mean(ch$change[ch$ramadan] >= actual)
  )
}))
readr::write_csv(placebo, "output/tab_time_placebo.csv")
print(placebo)

# ---- Bulk and packaged prices against the caps (Rp per litre) ----
caps <- bind_rows(lapply(meta$price_caps, function(x) as_tibble(lapply(x, function(v) v %||% NA)))) |>
  mutate(from = as.Date(from), to = as.Date(to))
period_of <- function(k) {
  case_when(k %in% cap_k | k == dec$ref_k ~ "caps",
            k %in% win$post_preban ~ "post_preban",
            k %in% win$post_ban ~ "post_ban",
            TRUE ~ NA_character_)
}
cap_level <- c(caps = caps$bulk[1], post_preban = caps$bulk[2], post_ban = caps$bulk[2])

bulk_cap <- panel |>
  mutate(period = period_of(k)) |>
  filter(!is.na(period), !is.na(bulk)) |>
  group_by(province, period, producer, eastern) |>
  summarise(bulk = mean(bulk), pkg1 = mean(pkg1, na.rm = TRUE), weeks = n(), .groups = "drop") |>
  mutate(cap = cap_level[period], excess_pct = 100 * (bulk / cap - 1))
readr::write_csv(bulk_cap, "output/tab_bulk_cap.csv")

nat_period <- national |>
  mutate(period = period_of(k)) |>
  filter(!is.na(period)) |>
  group_by(period) |>
  summarise(bulk = mean(bulk, na.rm = TRUE), pkg1 = mean(pkg1, na.rm = TRUE), .groups = "drop") |>
  mutate(cap = cap_level[period], excess_pct = 100 * (bulk / cap - 1))
print(nat_period)

bulk_caps_median <- median(bulk_cap$excess_pct[bulk_cap$period == "caps"])

# ---- Province changes for the heterogeneity figure ----
prov_change <- panel |>
  group_by(province, producer, prod_share, eastern) |>
  summarise(
    ref = if (any(k == dec$ref_k & !is.na(gap))) gap[k == dec$ref_k & !is.na(gap)][1]
    else mean(gap[k %in% c(-2, -1)], na.rm = TRUE),
    post = mean(gap[k %in% win$post_preban], na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(ref), !is.na(post)) |>
  mutate(change_pp = 100 * (post - ref))
readr::write_csv(prov_change, "output/tab_province_change.csv")

# ---- Sanity checks ----
cat("Event study: n =", nobs(es_gap$model), "province-weeks,",
    n_distinct(es_gap$data$province), "provinces,", length(es_gap$terms) + 1, "weeks with data\n")
cat("Weeks without any gap observation:",
    paste(format(as.Date(dec$event_week) + 7 * ks[!kname(ks) %in% es_gap$terms]), collapse = ", "), "\n")
cat("Producer provinces in the model:",
    n_distinct(es_gap$data$province[es_gap$data$producer == 1]), "\n")

# ---- Key numbers ----
nat_at <- function(kk) mean(national$gap[national$k %in% kk], na.rm = TRUE)
kn_add("n_provinces", n_distinct(es_gap$data$province), "", 0, "Provinces in the event study", src)
kn_add("n_producer", n_distinct(es_gap$data$province[es_gap$data$producer == 1]), "", 0,
       "Producer provinces (>= 2% of 2021 palm oil output) in the event study", "Ditjenbun 2021")
kn_add("n_obs", nobs(es_gap$model), "", 0, "Province-weeks in the event study", src)
kn_add("n_weeks", length(es_gap$terms) + 1, "", 0, "Weeks with data in the event study", src)
kn_add("n_outliers_jump", sum(panel$jump_bulk | panel$jump_pkg1), "", 0,
       "Province-weeks the P4 jump rule drops (check only)", src)
kn_add("n_no_bulk", length(no_bulk), "", 0, "Provinces without a bulk series", src)

# Holiday weeks can be missing: take the latest recorded week up to two weeks earlier
nat_last <- function(kk) {
  x <- national |>
    filter(k <= kk, k >= kk - 2, !is.na(gap)) |>
    arrange(desc(k))
  x$gap[1]
}
kn_add("nat_gap_ref_pct", pct(nat_last(dec$ref_k)), "%", 1,
       "National packaged-bulk gap, week of 7 Mar 2022 (latest recorded week up to then)", src)
kn_add("nat_gap_precap_pct", pct(nat_at(pre_k)), "%", 1,
       "National gap, mean of weeks 4 Oct 2021 - 10 Jan 2022", src)
kn_add("nat_gap_post_preban_pct", pct(nat_at(win$post_preban)), "%", 1,
       "National gap, mean of weeks 21 Mar - 25 Apr 2022", src)
kn_add("nat_gap_k6_pct", pct(nat_last(6)), "%", 1,
       "National gap, week of 25 Apr 2022 (latest recorded week up to then)", src)
kn_add("nat_gap_post_ban_pct", pct(nat_at(win$post_ban)), "%", 1,
       "National gap, mean of weeks 30 May - 27 Jun 2022", src)
kn_add("nat_gap_end_pct", pct(nat_at(max(panel$k))), "%", 1,
       "National gap, last week in the window", src)

for (i in seq_len(nrow(summary_tab))) {
  r <- summary_tab[i, ]
  key <- gsub("^beta_", "es_", r$test)
  kn_add(paste0(key, "_pct"), r$est_pct, "%", 1, paste("Window average,", r$test), src)
  kn_add(paste0(key, "_lo"), r$lo_pct, "%", 1, paste("95% CI low (CR2),", r$test), src)
  kn_add(paste0(key, "_hi"), r$hi_pct, "%", 1, paste("95% CI high (CR2),", r$test), src)
  kn_add(paste0(key, "_p"), r$p, "", 3, paste("p-value (CR2, Satterthwaite),", r$test), src)
}
kn_add("cap_joint_p", cap_test$p_val, "", 3, "Joint test: gap flat across caps-period weeks", src)
kn_add("het_cap_joint_p", pre_het$p_val, "", 3,
       "Joint test: producer and other provinces move alike in caps-period weeks", src)

for (i in seq_len(nrow(conf))) {
  key <- gsub("^beta_", "es_", conf$test[i])
  kn_add(paste0(key, "_p_holm"), conf$p_holm[i], "", 3,
         paste("Holm-adjusted p across the three confirmatory tests,", conf$test[i]), src)
}
slope <- extra$slope_pp_week[extra$test == "drift_post_preban"]
kn_add("cap_drift_pp_week", slope, "log points/week", 2,
       "Slope of the gap across caps-period weeks (31 Jan - 7 Mar)", src)
kn_add("n_prov_fig3", nrow(prov_change), "", 0,
       "Provinces with a gap in the reference week and the first window", src)

for (i in seq_len(nrow(placebo))) {
  r <- placebo[i, ]
  tag <- paste0("plac_h", r$horizon)
  kn_add(paste0(tag, "_actual_pp"), r$actual_pp, "log points", 1,
         paste("Change in national monthly gap, Feb 2022 to", r$target), src)
  kn_add(paste0(tag, "_max_pp"), r$max_pp, "log points", 1,
         paste0("Largest ", r$horizon, "-month change, 2017-2021"), src)
  kn_add(paste0(tag, "_pctile"), r$pctile, "%", 1,
         paste0("Percentile of the 2022 change among ", r$horizon, "-month changes, 2017-2021"), src)
  kn_add(paste0(tag, "_p"), r$p_upper, "", 3, "Share of 2017-2021 changes at least as large", src)
  kn_add(paste0(tag, "_p_ramadan"), r$p_upper_ramadan, "", 3,
         "Same, Ramadan-spanning changes only", src)
  kn_add(paste0(tag, "_n"), r$n, "", 0, "Number of 2017-2021 changes compared", src)
}

for (pr in nat_period$period) {
  r <- nat_period[nat_period$period == pr, ]
  kn_add(paste0("nat_bulk_", pr), r$bulk, "Rp/l", 0, paste("National bulk price,", pr), src)
  kn_add(paste0("nat_bulk_excess_", pr, "_pct"), r$excess_pct, "%", 1,
         paste("National bulk price above its cap,", pr), src)
  kn_add(paste0("nat_pkg1_", pr), r$pkg1, "Rp/l", 0, paste("National packaged 1 price,", pr), src)
}
above <- bulk_cap |>
  filter(period == "post_preban")
kn_add("n_prov_bulk_above_cap_post", sum(above$excess_pct > 0), "", 0,
       "Provinces with bulk above Rp14,000/l, 21 Mar - 25 Apr 2022", src)
kn_add("n_prov_bulk_post", nrow(above), "", 0, "Provinces with bulk prices, 21 Mar - 25 Apr", src)
kn_add("bulk_excess_post_median_pct", median(above$excess_pct), "%", 1,
       "Median province: bulk price above its cap, 21 Mar - 25 Apr 2022", src)
kn_add("bulk_excess_post_max_pct", max(above$excess_pct), "%", 1,
       "Highest province: bulk price above its cap, 21 Mar - 25 Apr 2022", src)
kn_add("bulk_excess_caps_median_pct", bulk_caps_median, "%", 1,
       "Median province: bulk price above its cap, 1 Feb - 13 Mar 2022", src)
ct <- meta$cash_transfer$rp_month
kn_add("transfer_litres_cap", ct / caps$bulk[2], "litres", 1,
       "Litres of bulk oil the monthly cash transfer bought at the Rp14,000 cap", meta$cash_transfer$source)
kn_add("transfer_litres_actual", ct / nat_period$bulk[nat_period$period == "post_preban"], "litres", 1,
       "Litres it bought at the national bulk price, 21 Mar - 25 Apr 2022", src)
kn_add("pkg1_vs_premium_cap_caps_pct",
       100 * (nat_period$pkg1[nat_period$period == "caps"] / caps$premium_pkg[1] - 1), "%", 1,
       "National packaged 1 price vs Rp14,000 premium cap, caps period", src)

write_session_info()
