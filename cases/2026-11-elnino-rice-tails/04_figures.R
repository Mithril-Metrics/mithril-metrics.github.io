# 04_figures.R — 3 figures, hero readable on a phone -------------------------------
# Case: 2026-11-elnino-rice-tails (local folder: AgEcon/Issue 1/)
# Titles pull numbers from key_numbers.csv and switch wording if the sign flips,
# so the figures can never contradict the text.

stopifnot("Run this script from the issue folder (AgEcon/Issue 1)" = file.exists("00_meta.yml"))
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
panel <- readRDS("output/cache/panel.rds")
wrap <- function(x, width) paste(strwrap(x, width), collapse = "\n")   # keep titles inside the frame

col_en    <- ac_palette(2)[2]   # orange = El Niño
col_other <- "#8f8e88"          # neutral grey = other years
col_norm  <- ac_palette(1)      # blue = normal benchmark
cap <- source_caption(
  c(paste0("FAOSTAT QCL rice yields (release ", substr(raw$fao_version, 1, 10), ")"), "NOAA CPC ONI"),
  accessed = raw$fao_accessed,
  note = "Shortfall: national yield at least 5% below its long-run trend. Cambodia from 1980."
)
cap <- paste(vapply(strsplit(cap, "\n")[[1]], wrap, "", width = 115), collapse = "\n")

# --- Figure 1 (hero): probability of a >= 5% shortfall, by year type -------------------------
f1 <- readr::read_csv("output/tab_fig1.csv", show_col_types = FALSE) |>
  filter(!is.na(p)) |>
  mutate(group = factor(group, levels = rev(group)))
up <- kv("p5_elnino_pct") >= kv("p5_other_pct")
title1 <- if (up) {
  paste0("In El Niño years, the chance of a rice harvest at least 5% below trend rises from ",
         k("p5_other_pct", unit = TRUE), " to ", k("p5_elnino_pct", unit = TRUE))
} else {
  paste0("No detectable rise in the chance of a rice harvest at least 5% below trend in the year El Niño begins (",
         k("p5_elnino_pct", unit = TRUE), " vs ", k("p5_other_pct", unit = TRUE), ")")
}
p1 <- ggplot(f1, aes(y = group, x = p)) +
  geom_vline(xintercept = f1$p_normal[1], linetype = "dashed", colour = col_norm, linewidth = 0.6) +
  annotate("text", x = f1$p_normal[1], y = 0.55, hjust = -0.05, size = 3.8, colour = col_norm,
           label = paste0("If shocks were normal: ", round(f1$p_normal[1]), "%")) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, orientation = "y", colour = "grey55", linewidth = 0.6) +
  geom_point(aes(colour = group == "Other years"), size = 4.5) +
  geom_text(aes(label = paste0(round(p), "%")), vjust = -1.1, size = 4.4, colour = "grey15") +
  scale_colour_manual(values = c(`TRUE` = col_other, `FALSE` = col_en), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_y_discrete(labels = function(x) paste0(x, "\n(n = ", f1$n[match(x, f1$group)], ")")) +
  labs(title = wrap(title1, 62),
       subtitle = wrap(paste0("Share of country-years with yields at least 5% below trend; ",
                         k("n_countries"), " Southeast Asian countries, ", k("year_start"), "-",
                         k("year_end"), ". Bars: 95% intervals (year-block bootstrap)."), 88),
       x = NULL, y = NULL, caption = cap) +
  theme_ac(grid = "x")
save_fig(p1, "fig1_hero")

# --- Figure 2: yield shocks by country, El Niño years highlighted -------------------------------
title2 <- paste0(k("share_worst_elnino_pct", unit = TRUE), " of the worst 10% of rice harvests came in El Niño onset years, which make up ",
                 k("share_obs_elnino_pct", unit = TRUE), " of all years")
p2 <- ggplot(panel, aes(x = year, y = 100 * (exp(shock) - 1), fill = elnino)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = -5, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
  geom_col(width = 0.8) +
  facet_wrap(~ country, ncol = 2) +
  scale_fill_manual(values = c(`TRUE` = col_en, `FALSE` = "#c9c8c2"),
                    labels = c(`TRUE` = "El Niño onset year", `FALSE` = "Other year")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = wrap(title2, 70),
       subtitle = "National rice yield, % deviation from each country's long-run trend",
       x = NULL, y = NULL, caption = cap) +
  theme_ac() +
  theme(plot.title = element_text(size = rel(1.05)))
save_fig(p2, "fig2_country_shocks", height_px = 1300)

# --- Figure 3: tail frequencies vs the normal benchmark ---------------------------------------------
f3 <- readr::read_csv("output/tab_fig3.csv", show_col_types = FALSE) |>
  tidyr::pivot_longer(-loss_pct, names_to = "series", values_to = "share") |>
  filter(share >= 1e-4, loss_pct <= 20) |>
  mutate(series = recode(series, observed_all = "Observed, all years",
                         observed_elnino = "Observed, El Niño onset years",
                         normal = "If shocks were normal (typical years)",
                         normal_sd = "If shocks were normal (all years)"),
         series = factor(series, levels = c("Observed, all years", "Observed, El Niño onset years",
                                            "If shocks were normal (typical years)",
                                            "If shocks were normal (all years)")))
r10 <- kv("ratio10")
r10_sd <- kv("ratio10_sd")
title3 <- if (r10 >= 1.05 && r10_sd >= 1.05) {
  paste0("Harvests 10% or more below trend are ", k("ratio10_sd"), " to ", k("ratio10"),
         " times as frequent as a normal distribution predicts")
} else if (r10 >= 1.05) {
  paste0("Harvests 10% or more below trend are ", k("ratio10"), " times as frequent as a normal distribution predicts")
} else if (r10 <= 0.95) {
  paste0("Harvests 10% or more below trend are rarer than a normal distribution predicts (",
         k("ratio10"), "x)")
} else {
  "Harvests 10% or more below trend occur about as often as a normal distribution predicts"
}
p3 <- ggplot(f3, aes(x = loss_pct, y = 100 * share, colour = series, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 10, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
  scale_y_log10(labels = function(x) paste0(format(x, scientific = FALSE, drop0trailing = TRUE), "%")) +
  scale_colour_manual(values = c("Observed, all years" = "grey25",
                                 "Observed, El Niño onset years" = col_en,
                                 "If shocks were normal (typical years)" = col_norm,
                                 "If shocks were normal (all years)" = col_norm)) +
  scale_linetype_manual(values = c("Observed, all years" = "solid",
                                   "Observed, El Niño onset years" = "solid",
                                   "If shocks were normal (typical years)" = "dashed",
                                   "If shocks were normal (all years)" = "dotted")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  labs(title = wrap(title3, 62),
       subtitle = wrap(paste0("Share of country-years with a shortfall at least this large (log scale). ",
                              "Normal curves use each country's spread in typical years (robust) or in all years (SD); ",
                              k("n_loss10"), " of ", k("n_obs"), " country-years fell 10% or more."), 88),
       x = "Shortfall below trend", y = NULL, caption = cap) +
  theme_ac(grid = "xy")
save_fig(p3, "fig3_tail_vs_normal")

message("04_figures.R done: fig1_hero, fig2_country_shocks, fig3_tail_vs_normal in output/.")

# --- Figure 4 (candidate, exploratory "second look"): El Niño footprint by margin ----------
# Shown only if 03b_second_look.R has run. At most 3 figures go into the post; this one may
# replace Figure 2 if the margins tell the clearer story.
if (file.exists("output/tab_second_look.csv")) {
  sl <- readr::read_csv("output/tab_second_look.csv", show_col_types = FALSE) |>
    filter(grepl("^s2_|^s3_", stat)) |>
    mutate(
      group  = case_when(grepl("^s3_mar", stat) ~ "Maritime (IDN, PHL, MYS)",
                         grepl("^s3_mai", stat) ~ "Mainland (THA, VNM, KHM)",
                         TRUE ~ "All six countries"),
      margin = case_when(grepl("yield", stat) ~ "Yield",
                         grepl("area", stat)  ~ "Harvested area",
                         TRUE ~ "Production"),
      timing = ifelse(grepl("_t1$", stat), "Year after onset (t+1)", "Onset year (t)"),
      group  = factor(group, levels = c("All six countries", "Maritime (IDN, PHL, MYS)", "Mainland (THA, VNM, KHM)")),
      margin = factor(margin, levels = rev(c("Yield", "Harvested area", "Production")))
    )
  # Title: the production effect (pooled or by region, t or t+1) that is clearly different
  # from zero and largest in size; otherwise say plainly that the footprint is small.
  pr <- sl |> filter(margin == "Production", hi < 0 | lo > 0) |> arrange(desc(abs(estimate)))
  title4 <- if (nrow(pr) >= 1) {
    where <- c("All six countries" = "Across the six countries",
               "Maritime (IDN, PHL, MYS)" = "In maritime Southeast Asia",
               "Mainland (THA, VNM, KHM)" = "In mainland Southeast Asia")[[as.character(pr$group[1])]]
    when <- ifelse(grepl("_t1$", pr$stat[1]), " the year after an El Niño onset", " in El Niño onset years")
    paste0(where, ", rice production runs ", round(abs(pr$estimate[1]), 1), "% ",
           ifelse(pr$estimate[1] < 0, "below", "above"), " trend", when)
  } else {
    "El Niño's footprint on rice yields, area and production is small and imprecise"
  }
  n_sig <- sum(sl$p_holm < 0.05, na.rm = TRUE)
  holm_note <- if (n_sig == 1) {
    "Only the effect named in the title survives a correction for multiple testing (Holm)."
  } else {
    paste0(n_sig, " of ", nrow(sl), " effects survive a correction for multiple testing (Holm).")
  }
  p4 <- ggplot(sl, aes(x = estimate, y = margin, colour = timing)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25, orientation = "y", linewidth = 0.6,
                  position = position_dodge(width = 0.55)) +
    geom_point(size = 3, position = position_dodge(width = 0.55)) +
    facet_wrap(~ group, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c("Onset year (t)" = col_en, "Year after onset (t+1)" = col_norm)) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = wrap(title4, 62),
         subtitle = wrap(paste0("Effect of an El Niño year on the deviation from trend, vs ENSO-neutral years; ",
                                "country fixed effects, 95% intervals (year-block bootstrap). ",
                                "Exploratory: added after the main results. ", holm_note), 88),
         x = NULL, y = NULL,
         caption = paste(vapply(strsplit(sub("NOAA CPC ONI", "NOAA CPC ONI; UCDP/PRIO ACD v26.1",
                                              gsub("\n(?!Analysis)", " ", cap, perl = TRUE), fixed = TRUE), "\n")[[1]],
                                 wrap, "", width = 115), collapse = "\n")) +
    theme_ac(grid = "x")
  save_fig(p4, "fig4_second_look_margins", height_px = 1300)
}
