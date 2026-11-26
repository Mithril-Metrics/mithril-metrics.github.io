# 04_figures.R — figures for the report
# Case: 2026-11-indonesia-palm-oil-exports
# Input: output/cache/analysis_data_fitted.rds, output/tab_*.csv, output/key_numbers.csv
# Output: output/fig1_hero, fig2_event_months, fig3_passthrough_grid, figA_levels, figB_phase_effects (.png/.svg)

stopifnot("Run this script from the issue folder" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})
kn_file <- "output/key_numbers.csv"
k <- function(key, ...) kn(key, file = kn_file, ...)
kv <- kn_read(kn_file)
v <- function(key) kv$value[kv$key == key]
meta <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")
df <- readRDS("output/cache/analysis_data_fitted.rds")
gaps <- read.csv("output/tab_monthly_gaps.csv") |> mutate(date = as.Date(date))
tab <- read.csv("output/tab_phase_effects.csv")
grid <- read.csv("output/tab_slope_grid.csv")
accessed <- max(as.Date(access_log()$accessed))
cols <- ac_palette(3)
win <- as.Date(meta$decisions$phases$window)
events <- bind_rows(lapply(meta$events_2022, as_tibble)) |>
  mutate(date = as.Date(date), y = c(1.54, 1.43, 1.32, 1.21, 1.10))
src <- "FAO GIEWS FPMA"

gap_words <- function(key) paste0(k(key, abs = TRUE), "% ", if (v(key) < 0) "below" else "above")

# ---- Fig 1 (hero): monthly gap between retail and the world-price level ----
yr <- range(c(gaps$gap_pct, gaps$gap_ex2022_pct))
events$y_pos <- yr[1] + events$y * diff(yr)
p1 <- ggplot(gaps, aes(date)) +
  annotate("rect", xmin = win[1] - 15, xmax = win[2] + 15, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_col(aes(y = gap_pct, fill = "Gap, main model"), width = 25) +
  geom_line(aes(y = gap_ex2022_pct, colour = "Gap, link estimated without 2022"), linewidth = 0.8) +
  geom_segment(data = events, aes(x = date, xend = date, y = 0, yend = y_pos),
               linetype = "dotted", colour = "grey25", linewidth = 0.4) +
  geom_text(data = events, aes(x = date + 20, y = y_pos, label = label),
            hjust = 0, vjust = 0.5, size = 3.2, colour = "grey20") +
  scale_fill_manual(values = cols[1], name = NULL) +
  scale_colour_manual(values = "grey15", name = NULL) +
  labs(
    title = str_wrap(paste0("The gap was ", k("gap_aprmay_minus_febmar_pp", abs = TRUE), " points ",
                            if (v("gap_aprmay_minus_febmar_pp") >= 0) "wider" else "narrower",
                            " in Apr-May 2022 than in Feb-Mar, the first full months of retail caps"), 55),
    subtitle = str_wrap("Indonesia, national retail price vs the price predicted from world palm oil prices (fitted relationship, not a parity price), %", 85),
    x = NULL, y = "Gap, %",
    caption = source_caption(paste0(src, "; policy dates: FAO FAPDA, press reports"), accessed = accessed,
                             note = "\nGrey band: Jan-May 2022. Implied level: AR(1) link to world CPO, lags 0-2.")
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(p1, "fig1_hero")

# ---- Fig B (supporting): pre-registered phase effects ----
rows <- tribble(
  ~model, ~term, ~label,
  "main", "window", "Jan-May 2022 (primary test)",
  "r1_split", "window_a", "Jan-Mar 2022 (split)",
  "r1_split", "window_b", "Apr-May 2022 (split)",
  "main", "post", "From Jun 2022",
  "placebo_flour", "window", "Wheat flour, Jan-May 2022 (placebo)"
)
eff <- rows |>
  left_join(tab, by = c("model", "term")) |>
  mutate(label = factor(label, levels = rev(rows$label)),
         kind = if_else(model == "placebo_flour", "Wheat flour (placebo)", "Cooking oil"))

primary_zero <- v("main_window_lo") < 0 & v("main_window_hi") > 0
lead <- if (primary_zero) "Over Jan-May 2022 as a whole, the shift is not clearly different from zero; " else
  paste0("Over Jan-May 2022, retail cooking oil shifted ", k("main_window_pct", sign = TRUE), "%; ")
p2 <- ggplot(eff, aes(pct, label, colour = kind)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_pointrange(aes(xmin = pct_lo, xmax = pct_hi), size = 0.6, linewidth = 0.9) +
  scale_colour_manual(values = cols[c(1, 3)], name = NULL) +
  labs(
    title = str_wrap(paste0(lead, "Apr-May alone was ", k("r1_window_b_pct", sign = TRUE), "%"), 55),
    subtitle = "Retail price relative to the level implied by the world price, % with 95% intervals",
    x = "Shift in retail price, %", y = NULL,
    caption = source_caption(src, accessed = accessed,
                             note = "\nAR(1) regression on world prices (lags 0-2), phase and Eid dummies.")
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(p2, "figB_phase_effects")

# ---- Fig 2: gap month by month (S6), no assumption on when effects start ----
ev <- read.csv("output/tab_event_months.csv") |>
  mutate(month = as.Date(month), mid = month + 14)
ev22 <- ev[format(ev$month, "%Y") == "2022", ]
chg <- function(key) paste0(if (v(key) < 0) "fell " else "rose ", k(key, abs = TRUE), " points")
bands <- tibble(
  from = as.Date(c("2022-01-19", "2022-04-28")),
  to = as.Date(c("2022-03-16", "2022-05-23")),
  label = c("One price, then caps", "Export ban")
)
p2 <- ggplot(ev, aes(mid, pct)) +
  geom_rect(data = bands, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
            fill = "grey85", alpha = 0.7) +
  geom_text(data = bands, aes(x = from + 3, y = -Inf, label = label), inherit.aes = FALSE,
            hjust = 0, vjust = -0.6, size = 3.2, colour = "grey25") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_pointrange(aes(ymin = pct_lo, ymax = pct_hi), colour = cols[1], size = 0.5, linewidth = 0.8) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(
    title = str_wrap(paste0("Month by month, the gap ", chg("s6_feb_minus_jan_pp"), " in February 2022 and ",
                            chg("s6_apr_minus_mar_pp"), " in April"), 55),
    subtitle = str_wrap("Shift by month relative to Nov 2020 - Sep 2021, allowing for persistence, % with 95% intervals; compare months with each other, not with Fig 1", 90),
    x = NULL, y = "Gap, %",
    caption = source_caption(paste0(src, "; policy dates: FAO FAPDA, press reports"), accessed = accessed,
                             note = "\nAR(1) regression with one dummy per month, Oct 2021 - Dec 2022; Oct-Dec 2021 precede any measure.\nCheck added after first results.")
  ) +
  theme_ac()
save_fig(p2, "fig2_event_months")

# ---- Fig 3: how the estimates depend on the assumed pass-through ----
grid_lab <- c(window = "Jan-May 2022", window_b = "Apr-May 2022")
grid <- grid |> mutate(series = grid_lab[term])
est_pt <- tibble(x = c(v("passthrough_sum"), v("passthrough_ex2022")),
                 lab = c("Estimated, all months", "Estimated without 2022"))
p3 <- ggplot(grid, aes(beta, pct, colour = series, fill = series)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pct_lo, ymax = pct_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(data = est_pt, aes(xintercept = x), linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = cols[1:2], name = NULL) +
  scale_fill_manual(values = cols[1:2], name = NULL) +
  labs(
    title = str_wrap(paste0("As assumed pass-through goes from 0.1 to 0.9, the Apr-May 2022 shift ranges from ",
                            k("s3_window_b_min"), "% to ", k("s3_window_b_max"), "%"), 55),
    subtitle = "Shift in retail price for an assumed pass-through of world palm oil, % with 95% intervals",
    x = "Assumed pass-through (elasticity, lags 0-2)", y = "Shift in retail price, %",
    caption = source_caption(src, accessed = accessed,
                             note = paste0("\nDashed lines: estimated pass-through, all months (", k("passthrough_sum"),
                                           ") and without 2022 (", k("passthrough_ex2022"), ").",
                                           "\nAbove 0.7 the AR(1) term nears 1 and the intervals are less reliable. Check added after first results."))
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(p3, "fig3_passthrough_grid")

# ---- Fig A (supporting): price levels with the price caps ----
caps <- bind_rows(lapply(meta$price_caps, as_tibble)) |>
  mutate(from = as.Date(from), to = as.Date(to))
lines <- df |>
  select(date, `Retail price (BPS national average)` = vegoil,
         `Price predicted from world palm oil prices` = cf_vegoil) |>
  pivot_longer(-date, names_to = "series", values_to = "idr")
pA <- ggplot() +
  annotate("rect", xmin = win[1], xmax = win[2] + 30, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  annotate("text", x = win[1] - 20, y = Inf, vjust = 1.5, hjust = 1, size = 3.3, colour = "grey30",
           label = "Jan-May 2022") +
  geom_segment(data = caps, aes(x = from, xend = to, y = idr_per_litre, yend = idr_per_litre),
               linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  annotate("text", x = as.Date("2024-09-01"), y = max(caps$idr_per_litre), vjust = 1.5, hjust = 0,
           size = 3.2, colour = "grey30", label = "Price caps (one price, packaged, bulk / MinyaKita)") +
  geom_line(data = lines, aes(date, idr, colour = series), linewidth = 1) +
  scale_colour_manual(values = cols[1:2], name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Retail cooking oil and the price predicted from world palm oil prices",
    subtitle = "Indonesia, national retail price, IDR per litre",
    x = NULL, y = "IDR per litre",
    caption = source_caption(paste0(src, "; price caps: Trade Ministry rules (press reports)"),
                             accessed = accessed,
                             note = "\nCaps apply to specific oil types, not to the whole BPS series.")
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(pA, "figA_levels")
