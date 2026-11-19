# 04_figures.R — hero price-ratio chart, elasticity chart, back-of-envelope table --
# Case: 2026-11-ssa-fertiliser-response (local folder: AgEcon/Issue 2/)
# Titles read key_numbers.csv and switch wording with the sign and the interval,
# so the figures cannot contradict the text.

stopifnot("Run this script from the issue folder (AgEcon/Issue 2)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(ggplot2)
})
kn_file <- "output/key_numbers.csv"
k  <- function(key, ...) kn(key, file = kn_file, ...)
kv <- function(key) {
  x <- kn_read(kn_file)
  x$value[x$key == key]
}
raw <- readRDS("output/cache/raw.rds")
ratio_month <- readRDS("output/cache/ratio_month.rds")
wrap <- function(x, width) paste(strwrap(x, width), collapse = "\n")
wrap_cap <- function(x) paste(vapply(strsplit(x, "\n")[[1]], wrap, "", width = 105), collapse = "\n")
col_line <- ac_palette(1)
col_band <- "#e4e3df"
col_hi <- ac_palette(2)[2]

# ---- Figure 1 (hero): urea-to-maize price ratio since 1990 ----
d1 <- filter(ratio_month, year >= 1990)
band <- quantile(d1$ratio[d1$year <= 2019], c(0.25, 0.75))
last <- tail(d1, 1)
peaks <- d1 |>
  filter(year %in% c(2008, 2022)) |>
  group_by(year) |>
  slice_max(ratio, n = 1) |>
  ungroup()
month_lab <- paste(month.name[kv("latest_month")], kv("latest_year"))
title1 <- if (kv("ratio_latest_multiple") >= 1) {
  paste0("In ", month_lab, ", a tonne of urea cost ", k("ratio_latest"), " tonnes of maize, ",
         k("ratio_latest_multiple"), " times the 1990-2019 median")
} else {
  paste0("In ", month_lab, ", a tonne of urea cost ", k("ratio_latest"),
         " tonnes of maize, below the 1990-2019 median of ", k("ratio_base"))
}
cap1 <- wrap_cap(source_caption(
  paste0("World Bank Pink Sheet (", raw$versions$pink, "): urea and maize (US Gulf), nominal $/t"),
  accessed = raw$accessed,
  note = "Band: middle half of monthly values 1990-2019. World prices, not farm-gate prices."
))
p1 <- ggplot(d1, aes(date, ratio)) +
  annotate("rect", xmin = min(d1$date), xmax = max(d1$date), ymin = band[1], ymax = band[2],
           fill = col_band, alpha = 0.8) +
  geom_hline(yintercept = kv("ratio_base"), linetype = "dashed", colour = "grey45", linewidth = 0.5) +
  geom_line(colour = col_line, linewidth = 0.7) +
  geom_point(data = bind_rows(peaks, last), colour = col_hi, size = 2.8) +
  geom_text(data = peaks, aes(label = paste0(format(date, "%b %Y"), ": ", round(ratio, 1))),
            vjust = -0.9, size = 3.8, colour = "grey15") +
  geom_text(data = last, aes(label = paste0(format(date, "%b %Y"), ": ", round(ratio, 1))),
            vjust = 2, hjust = 0.9, size = 3.8, colour = "grey15") +
  annotate("text", x = min(d1$date), y = kv("ratio_base"), hjust = 0, vjust = -0.5, size = 3.5,
           colour = "grey35", label = paste0("1990-2019 median: ", k("ratio_base"))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(title = wrap(title1, 62),
       subtitle = wrap(paste0("Tonnes of maize needed to buy one tonne of urea, monthly. Average for the ",
                              "Oct 2025-Sep 2026 crop year: ", k("ratio_now"), " (", k("ratio_now_multiple"),
                              " times the median)."), 72),
       x = "Month", y = "Tonnes of maize per tonne of urea", caption = cap1) +
  theme_ac()
save_fig(p1, "fig1_hero")

# ---- Figure 2: response of yields and N use, by subsidy group ----
pct10 <- function(b) 100 * (1.1^b - 1)
d2 <- readr::read_csv("output/tab_models.csv", show_col_types = FALSE) |>
  filter(model == "A") |>
  mutate(across(c(est, lo, hi), pct10),
         sample = factor(sample, levels = c("High input subsidies", "Low input subsidies", "All 14 countries")),
         outcome = factor(outcome, levels = c("Maize yield", "N use per ha")))
loo <- readr::read_csv("output/tab_loo.csv", show_col_types = FALSE) |>
  filter(model == "A") |>
  summarise(lo = pct10(min(est)), hi = pct10(max(est))) |>
  mutate(sample = factor("All 14 countries", levels = levels(d2$sample)),
         outcome = factor("Maize yield", levels = levels(d2$outcome)))

clear_drop <- kv("b_yield_hi") < 0
clear_rise <- kv("b_yield_lo") > 0
part1 <- if (clear_drop) {
  paste0("A 10% higher fertiliser-to-maize price ratio goes with ", k("b_yield_10pct", abs = TRUE, unit = TRUE),
         " lower maize yields")
} else if (clear_rise) {
  paste0("Maize yields did not fall when fertiliser got dearer relative to maize (",
         k("b_yield_10pct", unit = TRUE), " for a 10% higher ratio)")
} else {
  paste0("No clear link between the fertiliser-to-maize price ratio and maize yields (",
         k("b_yield_10pct", unit = TRUE), " for a 10% higher ratio, interval includes zero)")
}
theta_lo <- kv("theta_yield_lo")
theta_hi <- kv("theta_yield_hi")
part2 <- if (theta_lo > 0) {
  "; yields hold up better where input subsidies are large"
} else if (theta_hi < 0) {
  "; yields fall more where input subsidies are large"
} else {
  "; no clear difference by input-subsidy level"
}
cap2 <- wrap_cap(source_caption(
  c(paste0("FAOSTAT QCL (", substr(raw$versions$qcl, 1, 10), ") and RFN (", substr(raw$versions$rfn, 1, 10), ")"),
    "World Bank Pink Sheet", "FAO MAFAP public expenditure (March 2026)", "NOAA ONI"),
  accessed = raw$accessed,
  note = paste0("Country fixed effects and trends, ENSO control, Driscoll-Kraay 90% intervals. ",
                "Grey bar: range when one country at a time is left out. Associations, not effects.")
))
p2 <- ggplot(d2, aes(y = sample, x = est)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.5) +
  geom_errorbar(data = loo, aes(y = sample, xmin = lo, xmax = hi), inherit.aes = FALSE,
                width = 0, linewidth = 4, colour = col_band, orientation = "y") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.15, orientation = "y", colour = "grey35", linewidth = 0.6) +
  geom_point(aes(colour = sample == "All 14 countries"), size = 4) +
  geom_text(aes(label = sprintf("%+.1f%%", est)), vjust = -1.1, size = 3.9, colour = "grey15") +
  scale_colour_manual(values = c(`TRUE` = col_line, `FALSE` = col_hi), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(~outcome, scales = "free_x") +
  labs(title = wrap(paste0(part1, part2), 62),
       subtitle = wrap(paste0(k("n_countries"), " Sub-Saharan African countries, ", k("year_start"), "-",
                              k("year_end"), ". Change in the next season's outcome when the urea-to-maize ",
                              "price ratio is 10% higher."), 72),
       x = "% change for a 10% higher urea-to-maize price ratio", y = "Countries", caption = cap2) +
  theme_ac(grid = "x") +
  theme(panel.spacing.x = unit(2, "lines"))
save_fig(p2, "fig2_response")

# ---- Table 1: back-of-envelope for the 2026 crop year ----
boe <- readr::read_csv("output/tab_back_of_envelope.csv", show_col_types = FALSE)
fmt_pct <- function(x) sprintf("%+.1f%%", x)
fmt_kt <- function(x) format(round(x / 1e3), big.mark = ",")
rows <- paste0(
  "<tr><td>", boe$sample, "</td><td>", format(round(boe$prod_2024_t / 1e6, 1), nsmall = 1), "</td><td>",
  fmt_pct(boe$yield_pct), " (", fmt_pct(boe$yield_pct_lo), " to ", fmt_pct(boe$yield_pct_hi), ")</td><td>",
  fmt_kt(boe$tonnes), " (", fmt_kt(boe$tonnes_lo), " to ", fmt_kt(boe$tonnes_hi), ")</td></tr>"
)
html <- c(
  "<table class=\"table\">",
  paste0("<caption>If past responses hold: maize output implied by the ", k("crop_year_now"),
         " crop-year urea-to-maize ratio (", k("ratio_now"), ") against the 1990-2019 median (",
         k("ratio_base"), "). 90% intervals.</caption>"),
  "<thead><tr><th>Countries</th><th>Maize output 2024 (million t)</th><th>Implied yield change</th><th>Implied output change (thousand t)</th></tr></thead>",
  "<tbody>", rows, "</tbody></table>",
  "<p class=\"small\">Group rows use group-specific elasticities. World prices, national averages; not a forecast.</p>"
)
writeLines(html, "output/tab_1_back_of_envelope.html")
cat("Figures and table written to output/.\n")
