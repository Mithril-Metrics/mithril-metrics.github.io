# 04_figures.R — hero figure (retail price vs world-parity path) and phase effects
# Case: 2026-11-indonesia-palm-oil-exports
# Input: output/cache/analysis_data_fitted.rds, output/tab_phase_effects.csv, key_numbers.csv
# Output: output/fig1_hero.png/.svg, output/fig2_phase_effects.png/.svg

stopifnot("Run this script from the issue folder (AgEcon/Issue 6)" = file.exists("00_meta.yml"))
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
meta <- yaml::read_yaml("00_meta.yml", fileEncoding = "UTF-8")
df <- readRDS("output/cache/analysis_data_fitted.rds")
tab <- read.csv("output/tab_phase_effects.csv")
accessed <- max(as.Date(access_log()$accessed))
cols <- ac_palette(3)

# ---- Fig 1: retail price vs the path implied by world palm oil ----
caps <- bind_rows(lapply(meta$price_caps, as_tibble)) |>
  mutate(from = as.Date(from), to = as.Date(to))
win <- as.Date(meta$decisions$phases$window)

lines <- df |>
  select(date, `Retail price (BPS national average)` = vegoil,
         `Level implied by world palm oil price` = cf_vegoil) |>
  pivot_longer(-date, names_to = "series", values_to = "idr")

kv <- kn_read(kn_file)
dir_word <- if (kv$value[kv$key == "main_window_pct"] < 0) "below" else "above"
p1 <- ggplot() +
  annotate("rect", xmin = win[1], xmax = win[2] + 30, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  annotate("text", x = win[1] + 75, y = -Inf, vjust = -0.4, size = 3.6, colour = "grey30",
           label = "Policy window\nJan-May 2022") +
  geom_segment(data = caps, aes(x = from, xend = to, y = idr_per_litre, yend = idr_per_litre),
               linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  annotate("text", x = as.Date("2024-09-01"), y = max(caps$idr_per_litre), vjust = 1.3, hjust = 0,
           size = 3.4, colour = "grey30", label = "Price cap on bulk /\nMinyaKita oil") +
  geom_line(data = lines, aes(date, idr, colour = series), linewidth = 1) +
  scale_colour_manual(values = cols[1:2], name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = str_wrap(paste0("In Jan-May 2022, retail cooking oil was ", k("main_window_pct", abs = TRUE),
                            "% ", dir_word, " the level implied by world palm oil prices"), 52),
    subtitle = paste0("Indonesia, national retail price, IDR per litre; 95% interval ",
                      k("main_window_lo"), "% to ", k("main_window_hi"), "%"),
    x = "Month", y = "Cooking oil, IDR per litre",
    caption = source_caption(
      "FAO GIEWS FPMA; price caps: Trade Ministry rules (press reports)",
      accessed = accessed,
      note = "\nImplied level: estimated link to world CPO in rupiah (lags 0-2)."
    )
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(p1, "fig1_hero")

# ---- Fig 2: phase effects, cooking oil vs wheat-flour placebo ----
labs_term <- c(window = "Policy window\n(Jan-May 2022)", post = "After\n(Jun 2022 on)",
               bps2023 = "From 2023\n(incl. BPS sample change)")
eff <- tab |>
  filter(model %in% c("main", "placebo_flour"), term %in% names(labs_term)) |>
  mutate(
    term = factor(labs_term[term], levels = labs_term),
    product = if_else(model == "main", "Cooking oil vs world palm oil", "Wheat flour vs world wheat (placebo)")
  )

p2 <- ggplot(eff, aes(term, pct, colour = product)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_pointrange(aes(ymin = pct_lo, ymax = pct_hi), position = position_dodge(width = 0.5),
                  size = 0.6, linewidth = 0.9) +
  scale_colour_manual(values = cols[c(1, 3)], name = NULL) +
  labs(
    title = str_wrap(paste0("Cooking oil shifted ", k("main_window_pct", sign = TRUE),
                            "% in the policy window; wheat flour shifted ",
                            k("placebo_window_pct", sign = TRUE), "%"), 52),
    subtitle = "Retail price relative to the level implied by the world price, % with 95% intervals",
    x = "Phase", y = "Shift in retail price, %",
    caption = source_caption("FAO GIEWS FPMA", accessed = accessed,
                             note = "\nAR(1) regression on world prices (lags 0-2), phase and Eid dummies.")
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(p2, "fig2_phase_effects")
