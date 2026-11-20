# 04_figures.R — abstraction per irrigated hectare; two frontier models; tables ---------
# Case: 2026-11-eu-irrigation-water-productivity (local folder: AgEcon/Issue 3/)
# Titles read key_numbers.csv, so the figures cannot contradict the text.
# Red-team fixes (2026-11-20): "abstract" not "apply"; Eurostat-only headline with filled
# countries hollow; Fig 2 retitled; crop value per m3 dropped from the main table;
# 2 x 2 specification table.

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
r <- readRDS("output/cache/results.rds")
res <- r$res
wrap <- function(x, width) paste(strwrap(x, width), collapse = "\n")
wrap_cap <- function(x) paste(vapply(strsplit(x, "\n")[[1]], wrap, "", width = 110), collapse = "\n")
pal <- ac_palette(2)
grp_lab <- c("Mediterranean climate", "Other")
res <- mutate(res, group = factor(group, levels = grp_lab),
              lab_name = if_else(filled, paste0(name, "*"), name))
crop_src <- if (raw$crop$source[1] == "qv") "FAOSTAT QV (constant 2014-2016 int. $)" else "Eurostat aact_eaa01"
cap_base <- c("Eurostat env_wat_abs, ef_fsi_irri, sdg_06_60", crop_src, "FAOSTAT RL",
              "FAO AQUASTAT via World Bank")
fill_note <- paste0("* No Eurostat irrigation data: AQUASTAT agricultural withdrawal scaled by ",
                    k("fill_ratio", unit = TRUE), " (hollow bars; not in the headline medians or ",
                    "models; France's national statistics give ", k("ref_fr_m3ha"), " m3/ha for 2020).")

# ---- Figure 1 (hero): abstraction per irrigated hectare ----
cap1 <- wrap_cap(source_caption(cap_base, accessed = raw$accessed, note = paste0(
  "Irrigation abstraction (not water applied at the field), mean 2020-2022, divided by irrigated ",
  "area interpolated to 2021 from the 2016 and 2023 farm surveys. Medians over the ", k("n_main"),
  " countries with Eurostat irrigation data; ratio ", k("ratio_min"), "-", k("ratio_max"),
  " across samples and survey years; exact rank-sum p = ", k("p_ranksum"), ". The gap mixes ",
  "climate, crop mix, infrastructure and reporting. ", fill_note)))
title1 <- paste0("Mediterranean member states abstract ", k("ratio_min"), "-", k("ratio_max"),
                 " times more irrigation water per irrigated hectare than the rest (median ",
                 k("m3ha_med"), " vs ", k("m3ha_other"), " m3/ha)")
p1 <- ggplot(res, aes(m3_per_ha, reorder(lab_name, m3_per_ha))) +
  geom_col(aes(fill = if_else(filled, NA_character_, as.character(group)), colour = group),
           width = 0.7, linewidth = 0.6) +
  geom_text(aes(label = formatC(round(m3_per_ha, -1), format = "d", big.mark = ",")),
            hjust = -0.1, size = 3.6, colour = "grey20") +
  scale_fill_manual(values = setNames(pal, grp_lab), na.value = "white", guide = "none") +
  scale_colour_manual(values = setNames(pal, grp_lab), name = NULL,
                      guide = guide_legend(override.aes = list(fill = pal))) +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(title = wrap(title1, 62), x = "Irrigation abstraction per irrigated hectare, m3/ha",
       y = NULL, caption = cap1) +
  theme_ac(grid = "x") +
  theme(legend.position = "top", legend.justification = "left")
save_fig(p1, "fig1_hero", height_px = 1350)

# ---- Figure 2: two frontier models, headline sample ----
cap2 <- wrap_cap(source_caption(cap_base, accessed = raw$accessed, note = paste0(
  "Water-directional DEA, means 2020-2022, output crop production value, ", k("n_main"),
  " countries with Eurostat irrigation data. A: variable returns to scale, cropland and water. ",
  "B: constant returns to scale, rainfed cropland, irrigated area and water. Most of the gap ",
  "comes from returns to scale (", k("d_rts"), " points on average; splitting land: ", k("d_land"),
  "). Dropping one frontier country moves a single share by up to ", k("loo_shift_a"),
  " points (A) and ", k("loo_shift_b"), " (B). Model B mostly compares crop value per m3 with ",
  "largely rainfed peers (rank correlation ", k("rho_b_ppm"), ").")))
title2 <- paste0("Two common frontier models disagree: ", k("share_a"), "% or ", k("share_b"),
                 "% of irrigation water as 'saving potential'. Neither estimates saveable water")
d2 <- res |>
  filter(!filled) |>
  mutate(lab_name = reorder(lab_name, saving_B)) |>
  tidyr::pivot_longer(c(saving_A, saving_B), names_to = "spec", values_to = "saving") |>
  mutate(spec = recode(spec, saving_A = "A: VRS, cropland + water",
                       saving_B = "B: CRS, rainfed + irrigated land + water"))
p2 <- ggplot(d2, aes(saving, lab_name)) +
  geom_line(aes(group = lab_name), colour = "grey70", linewidth = 0.8) +
  geom_point(aes(colour = spec), size = 3) +
  scale_colour_manual(values = pal, name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = wrap(title2, 62), x = "Model 'saving potential', % of the country's irrigation water",
       y = NULL, caption = cap2) +
  theme_ac(grid = "x") +
  theme(legend.position = "top", legend.justification = "left", legend.direction = "vertical")
save_fig(p2, "fig2_two_models", height_px = 1300)

# ---- Table 1: by country ----
rng <- function(lo, hi) if_else(is.na(lo), "", paste0(round(lo), "-", round(hi)))
tab <- res |>
  arrange(desc(m3_per_ha)) |>
  transmute(Country = lab_name,
            Climate = as.character(group),
            `Abstraction per irrigated ha (m3)` = round(m3_per_ha, -1),
            `WEI+ (%)` = round(wei, 1),
            `Model A (%)` = if_else(filled, "", as.character(round(saving_A))),
            `Model A, leave-one-out (%)` = rng(loo_min_A, loo_max_A),
            `Model B (%)` = if_else(filled, "", as.character(round(saving_B))),
            `Model B, leave-one-out (%)` = rng(loo_min_B, loo_max_B))
readr::write_csv(tab, "output/tab_main.csv")
title3 <- paste0("Abstraction per irrigated hectare and frontier 'saving potential', ", k("n_all"),
                 " member states, means ", k("year_start"), "-", k("year_end"),
                 " (models on the ", k("n_main"), " countries with Eurostat irrigation data)")
note3 <- wrap_cap(source_caption(cap_base, accessed = raw$accessed, note = fill_note))
html <- knitr::kable(tab, format = "html", caption = title3, align = c("l", "l", rep("r", 6)))
writeLines(c(as.character(html), paste0("<p>", gsub("\n", "<br>", note3), "</p>")),
           "output/tab_main.html")

# ---- Table 2: the 2 x 2 specifications ----
tab2 <- r$specs |>
  transmute(Model = label,
            `Returns to scale` = toupper(rts),
            `Land input` = if_else(land == "total", "cropland", "rainfed + irrigated"),
            `Saving potential, % of sample water` = round(saving_share_eurostat),
            `Countries on the frontier` = paste0(n_frontier_eurostat, " (", frontier_eurostat, ")"))
readr::write_csv(tab2, "output/tab_specs_display.csv")
title4 <- paste0("Which modelling choice moves the 'saving potential'? Returns to scale: ",
                 k("d_rts"), " points on average; splitting land: ", k("d_land"), " points")
html2 <- knitr::kable(tab2, format = "html", caption = title4, align = c("l", "l", "l", "r", "l"))
writeLines(c(as.character(html2), paste0("<p>", gsub("\n", "<br>", note3), "</p>")),
           "output/tab_specs.html")
cat(title1, "\n", title2, "\n", title3, "\n", title4, "\n", sep = "")
