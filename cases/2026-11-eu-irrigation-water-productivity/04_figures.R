# 04_figures.R — water per irrigated hectare; frontier savings under two specs; table ---
# Case: 2026-11-eu-irrigation-water-productivity (local folder: AgEcon/Issue 3/)
# Titles read key_numbers.csv, so the figures cannot contradict the text.

stopifnot("Run this script from the issue folder (AgEcon/Issue 3)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(ggplot2)
})
kn_file <- "output/key_numbers.csv"
k <- function(key, ...) kn(key, file = kn_file, ...)
raw <- readRDS("output/cache/raw.rds")
res <- readRDS("output/cache/results.rds")$res
wrap <- function(x, width) paste(strwrap(x, width), collapse = "\n")
wrap_cap <- function(x) paste(vapply(strsplit(x, "\n")[[1]], wrap, "", width = 110), collapse = "\n")
pal <- ac_palette(2)
grp_lab <- c("Mediterranean climate", "Other")
res <- mutate(res, group = factor(group, levels = grp_lab),
              lab_name = if_else(filled, paste0(name, "*"), name))
crop_src <- if (raw$crop$source[1] == "qv") "FAOSTAT QV (constant 2014-2016 int. $)" else "Eurostat aact_eaa01"
cap_base <- c("Eurostat env_wat_abs, ef_fsi_irri, sdg_06_60", crop_src, "FAOSTAT RL",
              "FAO AQUASTAT via World Bank")
fill_note <- paste0("* Water filled from AQUASTAT agricultural withdrawals, scaled by ",
                    k("fill_ratio", unit = TRUE), " (Eurostat irrigation / AQUASTAT agriculture).")

# ---- Figure 1 (hero): water per irrigated hectare ----
cap1 <- wrap_cap(source_caption(cap_base, accessed = raw$accessed, note = paste0(
  "Irrigation abstraction, mean 2020-2022, divided by irrigated area interpolated to 2021 from ",
  "the 2016 and 2023 farm surveys. Countries with irrigated area >= ", k("irr_threshold"),
  "% of farmland. ", fill_note)))
title1 <- paste0("Mediterranean member states apply ", k("m3ha_ratio"), " times more irrigation ",
                 "water per hectare than the rest of the sample (median ", k("m3ha_med"),
                 " vs ", k("m3ha_other"), " m3/ha)")
p1 <- ggplot(res, aes(m3_per_ha, reorder(lab_name, m3_per_ha), fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = formatC(round(m3_per_ha, -1), format = "d", big.mark = ",")),
            hjust = -0.1, size = 3.6, colour = "grey20") +
  scale_fill_manual(values = pal, name = NULL) +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(title = wrap(title1, 62), x = "Irrigation water per irrigated hectare, m3/ha",
       y = NULL, caption = cap1) +
  theme_ac(grid = "x") +
  theme(legend.position = "top", legend.justification = "left")
save_fig(p1, "fig1_hero", height_px = 1300)

# ---- Figure 2: frontier "saving potential" under two specifications ----
cap2 <- wrap_cap(source_caption(cap_base, accessed = raw$accessed, note = paste0(
  "Water-directional DEA, means 2020-2022, output crop production value. A: variable returns ",
  "to scale, inputs cropland and water. B: constant returns to scale, inputs rainfed cropland, ",
  "irrigated area and water. ", fill_note)))
title2 <- paste0("Two reasonable frontier models put the 'saving potential' at ",
                 k("saving_share_a"), "% or ", k("saving_share_b"), "% of irrigation water; ",
                 "model B mostly ranks water per hectare (rank correlation ", k("rho_b_m3ha"), ")")
d2 <- res |>
  mutate(lab_name = reorder(lab_name, saving_b)) |>
  tidyr::pivot_longer(c(saving_a, saving_b), names_to = "spec", values_to = "saving") |>
  mutate(spec = recode(spec, saving_a = "A: VRS, cropland + water",
                       saving_b = "B: CRS, rainfed + irrigated land + water"))
p2 <- ggplot(d2, aes(saving, lab_name)) +
  geom_line(aes(group = lab_name), colour = "grey70", linewidth = 0.8) +
  geom_point(aes(colour = spec), size = 3) +
  scale_colour_manual(values = pal, name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = wrap(title2, 62), x = "Water-saving potential, % of the country's irrigation water",
       y = NULL, caption = cap2) +
  theme_ac(grid = "x") +
  theme(legend.position = "top", legend.justification = "left", legend.direction = "vertical")
save_fig(p2, "fig2_two_models", height_px = 1400)

# ---- Table 1: inputs and both models ----
tab <- res |>
  arrange(desc(m3_per_ha)) |>
  transmute(Country = lab_name,
            Climate = as.character(group),
            `Water per irrigated ha (m3)` = round(m3_per_ha, -1),
            `Crop value per m3 (int. $)` = round(out_per_m3, 2),
            `WEI+ (%)` = round(wei, 1),
            `Saving, model A (%)` = round(saving_a),
            `Saving, model B (%)` = round(saving_b),
            `Model B, leave-one-out (%)` = if_else(is.na(loo_min), "",
                                                   paste0(round(loo_min), "-", round(loo_max))))
readr::write_csv(tab, "output/tab_main.csv")
title3 <- paste0("Water per hectare, crop value per m3 and frontier savings, ", k("n_countries"),
                 " member states, means ", k("year_start"), "-", k("year_end"))
html <- knitr::kable(tab, format = "html", caption = title3, align = c("l", "l", rep("r", 6)))
writeLines(c(as.character(html), paste0("<p>", gsub("\n", "<br>", wrap_cap(source_caption(
  cap_base, accessed = raw$accessed, note = fill_note))), "</p>")), "output/tab_main.html")
cat(title1, "\n", title2, "\n", title3, "\n", sep = "")
