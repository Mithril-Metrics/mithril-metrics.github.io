# 03b_sensitivity.R — checks added at P6 (see p6_checks in 00_meta.yml)
# Case: 2026-11-indonesia-palm-oil-exports
# Input: output/cache/analysis_data.rds
# Output: output/tab_sensitivity.csv, output/tab_slope_grid.csv, output/tab_time_placebo.csv,
#         more rows in output/key_numbers.csv
# The primary test stays in 03_analyse.R; nothing here replaces it.

stopifnot("Run this script from the issue folder" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
})
kn_file <- "output/key_numbers.csv"
df <- readRDS("output/cache/analysis_data.rds")

ar_fit <- function(y, x, d = df) {
  arima(d[[y]], order = c(1, 0, 0), xreg = as.matrix(d[, x]), method = "ML")
}

# Shift of one dummy in %, with 95% interval and p-value
shift <- function(m, term) {
  est <- coef(m)[term]
  se <- sqrt(m$var.coef[term, term])
  tibble(pct = 100 * (exp(est) - 1), pct_lo = 100 * (exp(est - 1.96 * se) - 1),
         pct_hi = 100 * (exp(est + 1.96 * se) - 1), p = 2 * pnorm(-abs(est / se)))
}

x_lags <- c("ln_cpo", "ln_cpo_l1", "ln_cpo_l2")
x_lags5 <- c(x_lags, "ln_cpo_l3", "ln_cpo_l4", "ln_cpo_l5")
x_rest <- c("post", "bps2023", "eid")

# ---- S1: world-price lags 0-5 ----
s1 <- ar_fit("ln_vegoil", c(x_lags5, "window", x_rest))
s1_r1 <- ar_fit("ln_vegoil", c(x_lags5, "window_a", "window_b", x_rest))
s1_pt <- sum(coef(s1)[x_lags5])

# ---- S4: does the gap move in the three months before the window? ----
s4 <- ar_fit("ln_vegoil", c(x_lags, "pre3", "window", x_rest))

sens <- bind_rows(
  shift(s1, "window") |> mutate(check = "S1 lags 0-5", term = "window"),
  shift(s1_r1, "window_b") |> mutate(check = "S1 lags 0-5", term = "window_b"),
  shift(s4, "pre3") |> mutate(check = "S4 pre-window dummy", term = "pre3"),
  shift(s4, "window") |> mutate(check = "S4 pre-window dummy", term = "window")
) |>
  relocate(check, term)
write.csv(sens, "output/tab_sensitivity.csv", row.names = FALSE)
print(as.data.frame(sens), digits = 3)
cat(sprintf("S1 pass-through (lags 0-5): %.3f\n", s1_pt))

# ---- S3: pass-through imposed on a grid ----
# Retail price net of beta times the 3-month mean of ln CPO; equal weights on lags 0-2
df$ma_cpo <- rowMeans(df[, x_lags])
grid <- seq(0.1, 0.9, by = 0.1)
s3 <- bind_rows(lapply(grid, function(bt) {
  d <- df
  d$y_b <- d$ln_vegoil - bt * d$ma_cpo
  m_w <- ar_fit("y_b", c("window", x_rest), d)
  m_b <- ar_fit("y_b", c("window_a", "window_b", x_rest), d)
  bind_rows(
    shift(m_w, "window") |> mutate(term = "window"),
    shift(m_b, "window_b") |> mutate(term = "window_b")
  ) |>
    mutate(beta = bt, ar1 = c(coef(m_w)["ar1"], coef(m_b)["ar1"]))
}))
write.csv(s3, "output/tab_slope_grid.csv", row.names = FALSE)
print(as.data.frame(s3), digits = 3)

# ---- S5: placebo in time ----
# The main model plus a fake window of the same length at every position that does not touch
# Jan-May 2022. How often does a fake window give a shift as large as the real one?
fake_shift <- function(i, len, base_x) {
  idx <- i:(i + len - 1)
  if (max(idx) > nrow(df) || any(df$window[idx] == 1)) return(NULL)
  d <- df
  d$fake <- 0L
  d$fake[idx] <- 1L
  m <- tryCatch(ar_fit("ln_vegoil", c(base_x, "fake"), d), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tibble(start = df$date[i], len = len, pct = 100 * (exp(coef(m)["fake"]) - 1))
}
x_main <- c(x_lags, "window", x_rest)
x_r1 <- c(x_lags, "window_a", "window_b", x_rest)
s5 <- bind_rows(
  bind_rows(lapply(seq_len(nrow(df)), fake_shift, len = 5, base_x = x_main)),
  bind_rows(lapply(seq_len(nrow(df)), fake_shift, len = 2, base_x = x_r1))
)
write.csv(s5, "output/tab_time_placebo.csv", row.names = FALSE)

real_w <- 100 * (exp(coef(ar_fit("ln_vegoil", x_main))["window"]) - 1)
real_b <- 100 * (exp(coef(ar_fit("ln_vegoil", x_r1))["window_b"]) - 1)
f5 <- s5$pct[s5$len == 5]
f2 <- s5$pct[s5$len == 2]
share5 <- mean(abs(f5) >= abs(real_w))
share2 <- mean(abs(f2) >= abs(real_b))
cat(sprintf("S5: %d fake 5-month windows, share with |shift| >= %.1f%%: %.2f (largest %.1f%%)\n",
            length(f5), real_w, share5, max(abs(f5))))
cat(sprintf("S5: %d fake 2-month windows, share with |shift| >= %.1f%%: %.2f (largest %.1f%%)\n",
            length(f2), real_b, share2, max(abs(f2))))

# ---- Key numbers ----
src <- "FAO GIEWS FPMA (BPS retail prices; Oil World CPO c.i.f. Rotterdam; FPMA IDR/USD) - AR(1) regression, 03b_sensitivity.R"
add <- function(key, value, unit = "", digits = 1, description = "") {
  kn_add(key, value, unit = unit, digits = digits, description = description, source = src, file = kn_file)
}
sv <- function(ch, t, col) sens[[col]][sens$check == ch & sens$term == t]

add("s1_window_pct", sv("S1 lags 0-5", "window", "pct"), "%", 1, "S1 (lags 0-5): window shift")
add("s1_window_lo", sv("S1 lags 0-5", "window", "pct_lo"), "%", 1, "S1 (lags 0-5): lower 95% bound, window")
add("s1_window_hi", sv("S1 lags 0-5", "window", "pct_hi"), "%", 1, "S1 (lags 0-5): upper 95% bound, window")
add("s1_window_b_pct", sv("S1 lags 0-5", "window_b", "pct"), "%", 1, "S1 (lags 0-5): Apr-May 2022 shift")
add("s1_passthrough", s1_pt, "", 2, "S1: sum of CPO elasticities, lags 0-5")
add("s4_pre3_pct", sv("S4 pre-window dummy", "pre3", "pct"), "%", 1, "S4: shift in Oct-Dec 2021, before any measure")
add("s4_pre3_lo", sv("S4 pre-window dummy", "pre3", "pct_lo"), "%", 1, "S4: lower 95% bound, Oct-Dec 2021")
add("s4_pre3_hi", sv("S4 pre-window dummy", "pre3", "pct_hi"), "%", 1, "S4: upper 95% bound, Oct-Dec 2021")
add("s4_pre3_p", sv("S4 pre-window dummy", "pre3", "p"), "", 3, "S4: p-value, Oct-Dec 2021")
add("s4_window_pct", sv("S4 pre-window dummy", "window", "pct"), "%", 1, "S4: window shift with the pre-window dummy in")
add("s3_window_min", min(s3$pct[s3$term == "window"]), "%", 1, "S3: smallest window shift over pass-through 0.1-0.9")
add("s3_window_max", max(s3$pct[s3$term == "window"]), "%", 1, "S3: largest window shift over pass-through 0.1-0.9")
add("s3_window_b_min", min(s3$pct[s3$term == "window_b"]), "%", 1, "S3: smallest Apr-May shift over pass-through 0.1-0.9")
add("s3_window_b_max", max(s3$pct[s3$term == "window_b"]), "%", 1, "S3: largest Apr-May shift over pass-through 0.1-0.9")
add("s5_n5", length(f5), "", 0, "S5: number of fake 5-month windows")
add("s5_share5", 100 * share5, "%", 0, "S5: share of fake 5-month windows with a shift at least as large as the real one")
add("s5_n2", length(f2), "", 0, "S5: number of fake 2-month windows")
add("s5_share2", 100 * share2, "%", 0, "S5: share of fake 2-month windows with a shift at least as large as Apr-May 2022")
add("s5_max2", max(abs(f2)), "%", 1, "S5: largest absolute shift among fake 2-month windows")

# ---- S6: one dummy per month, Oct 2021 - Dec 2022 (no assumption on when effects start) ----
# Each coefficient is that month's gap to the world-price level, allowing for the usual
# persistence of the gap. Oct-Dec 2021 are before any measure and show the pre-trend.
ev_months <- seq(as.Date("2021-10-01"), as.Date("2022-12-01"), by = "month")
ev_names <- paste0("m", format(ev_months, "%Y_%m"))
for (i in seq_along(ev_months)) {
  df[[ev_names[i]]] <- as.integer(df$date == ev_months[i])
}
s6_fit <- ar_fit("ln_vegoil", c(x_lags, ev_names, "bps2023", "eid"))
s6 <- bind_rows(lapply(seq_along(ev_names), function(i) {
  shift(s6_fit, ev_names[i]) |> mutate(month = ev_months[i], term = ev_names[i])
})) |>
  relocate(month, term)
write.csv(s6, "output/tab_event_months.csv", row.names = FALSE)
print(as.data.frame(s6), digits = 3)

for (i in seq_len(nrow(s6))) {
  lab <- format(s6$month[i], "%b %Y")
  key <- paste0("s6_", format(s6$month[i], "%Y_%m"))
  add(paste0(key, "_pct"), s6$pct[i], "%", 1, paste("S6 month dummies: gap,", lab))
  add(paste0(key, "_lo"), s6$pct_lo[i], "%", 1, paste("S6 month dummies: lower 95% bound,", lab))
  add(paste0(key, "_hi"), s6$pct_hi[i], "%", 1, paste("S6 month dummies: upper 95% bound,", lab))
}

# ---- S7: pre-registered phases moved by 0, 1 and 2 months (all reported) ----
shift_month <- function(x, k) seq(as.Date(x), by = paste(k, "months"), length.out = 2)[2]
ph <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")$decisions$phases
in_range <- function(r, k) as.integer(df$date >= shift_month(r[1], k) & df$date <= shift_month(r[2], k))
s7 <- bind_rows(lapply(0:2, function(k) {
  d <- df
  d$window <- in_range(ph$window, k)
  d$window_a <- in_range(ph$window_a, k)
  d$window_b <- in_range(ph$window_b, k)
  d$post <- as.integer(d$date >= shift_month(ph$post_start, k))
  m_w <- ar_fit("ln_vegoil", c(x_lags, "window", x_rest), d)
  m_b <- ar_fit("ln_vegoil", c(x_lags, "window_a", "window_b", x_rest), d)
  bind_rows(
    shift(m_w, "window") |> mutate(term = "window"),
    shift(m_w, "post") |> mutate(term = "post"),
    shift(m_b, "window_a") |> mutate(term = "window_a"),
    shift(m_b, "window_b") |> mutate(term = "window_b")
  ) |>
    mutate(lag_months = k, .before = 1)
}))
write.csv(s7, "output/tab_shifted_phases.csv", row.names = FALSE)
print(as.data.frame(s7), digits = 3)

for (k in 1:2) {
  for (t in c("window", "window_a", "window_b", "post")) {
    r <- s7[s7$lag_months == k & s7$term == t, ]
    lab <- paste0("S7 phases moved ", k, " month(s): ", t)
    add(paste0("s7_lag", k, "_", t, "_pct"), r$pct, "%", 1, paste(lab, "shift"))
    add(paste0("s7_lag", k, "_", t, "_lo"), r$pct_lo, "%", 1, paste(lab, "lower 95% bound"))
    add(paste0("s7_lag", k, "_", t, "_hi"), r$pct_hi, "%", 1, paste(lab, "upper 95% bound"))
    add(paste0("s7_lag", k, "_", t, "_p"), r$p, "", 3, paste(lab, "p-value"))
  }
}
