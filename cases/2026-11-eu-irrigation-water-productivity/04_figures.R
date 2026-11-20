# 04_figures.R — hero saving chart, saving vs water stress, rank comparison table ---
# Case: 2026-11-eu-irrigation-water-productivity (local folder: AgEcon/Issue 3/)
# Titles read key_numbers.csv and switch wording with the result, so the figures
# cannot contradict the text.

stopifnot("Run this script from the issue folder (AgEcon/Issue 3)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(ggplot2)
})
kn_file <- "output/key_numbers.csv"
k <- function(key, ...) kn(key, file = kn_file, ...)
kv <- function(key) {
  x <- kn_read(kn_file)
  x$value[x$key == key]
}
raw <- readRDS("output/cache/raw.rds")
out <- readRDS("output/cache/results.rds")
res <- out$res
wrap <- function(x, width) paste(strwrap(x, width), collapse = "\n")
wrap_cap <- function(x) paste(vapply(strsplit(x, "\n")[[1]], wrap, "", width = 110), collapse = "\n")
col_eu <- ac_palette(1)
col_fill <- ac_palette(2)[2]
src_lab <- c("Eurostat irrigation abstraction", "AQUASTAT agricultural withdrawal (filled)")
res <- mutate(res, src = factor(if_else(filled, src_lab[2], src_lab[1]), levels = src_lab))
fmt_m3 <- function(x) formatC(round(x), format = "d", big.mark = ",")
cap <- wrap_cap(source_caption(
  c("Eurostat env_wat_abs, aact_eaa01, ef_fsi_irri, sdg_06_60", "FAOSTAT RL",
    "FAO AQUASTAT via World Bank"),
  accessed = raw$accessed,
  note = paste0("Means 2020-2022. Water-directional DEA, variable returns to scale, inputs ",
                "cropland and irrigation water, output crop output. Countries with irrigated area >= ",
                k("irr_threshold"), "% of farmland. Country averages, not farms.")
))

# ---- Figure 1 (hero): water-saving potential by country ----
d1 <- res |>
  mutate(name = reorder(name, saving_pct),
         lab = if_else(frontier, "on the frontier",
                       paste0(round(saving_pct), "%  (", fmt_m3(saving_m3), " million m3)")))
top_name <- if (out$top$name == "Netherlands") "the Netherlands" else out$top$name
title1 <- paste0(
  "Matching the best performers would cut irrigation water by ", k("saving_share", unit = TRUE),
  " across ", k("n_countries"), " EU countries; ", top_name, " has the largest gap (",
  k("saving_top", unit = TRUE), ")"
)
p1 <- ggplot(d1, aes(saving_pct, name)) +
  geom_col(aes(fill = src), colour = col_eu, linewidth = 0.4, width = 0.7) +
  geom_text(aes(label = lab), hjust = -0.05, size = 3.6, colour = "grey20") +
  scale_fill_manual(values = c(col_eu, "white"), name = NULL, drop = FALSE) +
  scale_x_continuous(limits = c(0, 135), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(title = wrap(title1, 62),
       x = "Water-saving potential, % of irrigation water at the same cropland and crop output",
       y = "Member state", caption = cap) +
  theme_ac(grid = "x") +
  theme(legend.position = "top", legend.justification = "left")
save_fig(p1, "fig1_hero")

# ---- Figure 2: saving potential vs water stress ----
d2 <- filter(res, !is.na(wei))
rho <- kv("rho_wei")
pval <- kv("p_wei")
title2 <- if (rho >= 0.3 && pval < 0.1) {
  paste0("The largest water savings tend to be in the most water-stressed countries (rank correlation ",
         k("rho_wei"), ")")
} else if (rho <= -0.3 && pval < 0.1) {
  paste0("The largest water savings tend to be in the least water-stressed countries (rank correlation ",
         k("rho_wei"), ")")
} else {
  paste0("Saving potential is not concentrated where water is scarcest (rank correlation ",
         k("rho_wei"), ", ", k("n_wei"), " countries)")
}
p2 <- ggplot(d2, aes(wei, saving_pct)) +
  geom_vline(xintercept = median(d2$wei), linetype = "dashed", colour = "grey55") +
  geom_hline(yintercept = median(d2$saving_pct), linetype = "dashed", colour = "grey55") +
  geom_point(aes(fill = src), shape = 21, colour = col_eu, size = 3.2, stroke = 0.8) +
  ggrepel::geom_text_repel(aes(label = name), size = 3.4, colour = "grey20", seed = 1,
                           min.segment.length = 0.3, box.padding = 0.4) +
  scale_fill_manual(values = c(col_eu, "white"), name = NULL, drop = FALSE) +
  labs(title = wrap(title2, 62),
       subtitle = "Dashed lines: sample medians",
       x = "Water exploitation index plus (WEI+), % of renewable freshwater used",
       y = "Water-saving potential, %", caption = cap) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_ac(grid = "xy") +
  theme(legend.position = "bottom", legend.justification = "left")
save_fig(p2, "fig2_stress")

# ---- Table 1: output per m3 vs frontier ranking ----
ppm_unit <- if (grepl("^EUR", attr(readRDS("output/cache/panel.rds"), "output_measure"))) "EUR" else "I$"
tab <- res |>
  arrange(rank_dea) |>
  transmute(Country = name,
            `Water data` = as.character(src),
            !!paste0("Crop output per m3 (", ppm_unit, ")") := round(out_per_m3, 2),
            `Rank, output per m3 (1 = lowest)` = rank_ppm,
            `Saving potential (%)` = round(saving_pct),
            `Rank, saving potential (1 = largest)` = rank_dea,
            `Leave-one-out range (%)` = if_else(is.na(loo_min), "",
                                                paste0(round(loo_min), "-", round(loo_max))))
readr::write_csv(tab, "output/tab_ranks.csv")
title3 <- paste0("Ranking by output per m3 and by the frontier differs by 3 or more places for ",
                 k("n_disagree"), " of ", k("n_countries"), " countries")
html <- knitr::kable(tab, format = "html", caption = title3, align = c("l", "l", rep("r", 5)))
writeLines(c(as.character(html), paste0("<p>", gsub("\n", "<br>", cap), "</p>")),
           "output/tab_ranks.html")
cat(title1, "\n", title2, "\n", title3, "\n", sep = "")
