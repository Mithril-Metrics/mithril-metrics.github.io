# 04_figures.R — hero dumbbell and long-difference scatter --------------------------
# Case: 2026-11-support-composition-tfp (local folder: AgEcon/Issue 5/)
# Input:  output/cache/clean.rds, output/key_numbers.csv
# Output: output/fig1_hero.png/.svg, output/fig2_long_difference.png/.svg

stopifnot("Run this script from the issue folder (AgEcon/Issue 5)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(ggplot2)
})
cl <- readRDS("output/cache/clean.rds")
num <- function(key) kn_read()$value[kn_read()$key == key]

names_short <- c(
  ARG = "Argentina", AUS = "Australia", BRA = "Brazil", CAN = "Canada", CHE = "Switzerland",
  CHL = "Chile", CHN = "China", COL = "Colombia", CRI = "Costa Rica", EGY = "Egypt", EU = "EU",
  GBR = "United Kingdom", IDN = "Indonesia", IND = "India", ISL = "Iceland", ISR = "Israel",
  JPN = "Japan", KAZ = "Kazakhstan", KOR = "Korea", MEX = "Mexico", NOR = "Norway",
  NZL = "New Zealand", PER = "Peru", PHL = "Philippines", RUS = "Russia", TUR = "T\u00fcrkiye",
  UKR = "Ukraine", USA = "United States", VNM = "Viet Nam", ZAF = "South Africa"
)
wrap <- function(x, width = 62) {
  paste(strwrap(x, width), collapse = "\n")
}
label_of <- function(iso3) {
  ifelse(iso3 %in% names(names_short), names_short[iso3], iso3)
}
cap1 <- source_caption("OECD PSE/CSE database (2026 edition)", accessed = cl$accessed)
cap <- source_caption(
  c("OECD PSE/CSE database (2026 ed.)", "USDA ERS International Agricultural Productivity (2026 release)"),
  accessed = cl$accessed
)

# ---- Figure 1 (hero): AKIS spending as % of value of production, 2000-04 vs 2021-25 ----
dumb <- cl$dumb |>
  mutate(name = paste0(label_of(iso3), ifelse(stock_heavy, "*", ""), ifelse(step, " \u2020", "")),
         name = reorder(name, akis_now))
long <- bind_rows(
  transmute(dumb, name, window = "2000-04", value = akis_base),
  transmute(dumb, name, window = "2021-25", value = akis_now)
)
n_lower <- num("n_lower")
n_higher <- num("n_higher")
n_dumb <- num("n_dumb")
p1 <- ggplot(dumb, aes(y = name)) +
  geom_segment(aes(x = akis_base, xend = akis_now, yend = name), colour = "#b8b7b1", linewidth = 0.8) +
  geom_point(data = long, aes(x = value, colour = window), size = 2.6) +
  scale_colour_manual(values = ac_palette(2)) +
  labs(
    title = wrap(sprintf("Research and extension spending grew more slowly than farm output: its share fell in %d of %d economies",
                         n_lower, n_dumb)),
    subtitle = wrap(sprintf("Public spending on agricultural knowledge and innovation (AKIS), %% of farm output value. Real spending (GDP deflator) fell in %d of %d; in %d more it rose, but less than farm output value.",
                            num("n_real_fall"), num("n_real_avail"), num("n_real_rise_ratio_fall")), 85),
    x = "AKIS spending, % of value of agricultural production",
    y = "Economy",
    caption = paste0(cap1, "\n* Public stockholding is a large part of general services support here; it is not counted as AKIS.",
                     "\n\u2020 One-year jump of more than half in the AKIS share of general services, 2000-2025: a reclassification or a real change.")
  ) +
  theme_ac(base_size = 12, grid = "x") +
  theme(axis.text.y = element_text(size = 9),
        legend.position = "bottom")
# 30 economies need more height than the usual 1000 px to stay readable on a phone
save_fig(p1, "fig1_hero", height_px = 1400)

# ---- Figure 2: added-variable plot, AKIS 2000-09 vs TFP growth 2009-23 ----
# Both axes are net of price support, infrastructure, income and past growth (sample means
# added back), so the fitted slope is exactly the regression coefficient
ld <- readRDS("output/cache/av.rds") |>
  mutate(name = label_of(iso3), akis = akis_adj, g_post = g_adj)
b1 <- num("beta1_ld")
p_perm <- num("p_perm_ld")
assoc <- if (p_perm < 0.10 && b1 > 0) {
  "a positive association"
} else if (p_perm < 0.10 && b1 < 0) {
  "a negative association"
} else {
  "no clear association"
}
line <- tibble::tibble(x = range(ld$akis)) |>
  mutate(y = mean(ld$g_post) + b1 * (x - mean(ld$akis)))
txt <- if (requireNamespace("ggrepel", quietly = TRUE)) {
  ggrepel::geom_text_repel(aes(label = name), size = 3, colour = "#4a4a46", seed = 1, max.overlaps = 30)
} else {
  geom_text(aes(label = name), size = 3, colour = "#4a4a46", vjust = -0.8, check_overlap = TRUE)
}
p2 <- ggplot(ld, aes(akis, g_post)) +
  geom_hline(yintercept = 0, colour = "#b8b7b1") +
  geom_line(data = line, aes(x, y), colour = ac_palette(2)[2], linewidth = 0.9, linetype = "dashed") +
  geom_point(colour = ac_palette(1), size = 2.6) +
  txt +
  labs(
    title = wrap(sprintf("Research and extension spending in 2000-09 and productivity growth in 2009-23: %s across %d economies",
                         assoc, nrow(ld))),
    subtitle = wrap(sprintf("Both axes adjusted for price support, infrastructure spending, income and 1990-2000 growth (residuals plus the sample mean, not actual values); dashed line = fitted slope (permutation p = %.2f)",
                            p_perm), 85),
    x = "AKIS spending 2000-09, % of farm output value (adjusted)",
    y = "TFP growth 2009-23, % a year (adjusted)",
    caption = cap
  ) +
  theme_ac(base_size = 12, grid = "xy")
save_fig(p2, "fig2_long_difference")
