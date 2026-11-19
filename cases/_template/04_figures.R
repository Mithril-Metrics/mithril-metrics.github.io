# 04_figures.R — at most 3 figures, 1 hero figure readable on a phone -----------
# Case: {{CASE_ID}}
# Output: output/fig1_hero.png/.svg (+ fig2, fig3). Titles state the finding and pull
# their numbers from key_numbers.csv, so figures and text cannot disagree.

stopifnot("Run this script from the case folder" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")   # downloads can only land inside this folder
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(ggplot2)
})
kn_file <- "output/key_numbers.csv"
k <- function(key, ...) kn(key, file = kn_file, ...)

# --- Hero figure ---------------------------------------------------------------------
# Template placeholder: replace with the real data. Keep: finding-style title,
# direct labels where possible (some palette colours are < 3:1 on white), source line.
demo <- tibble(group = c("Group A", "Group B", "Group C"), value = c(-12.3, -4.1, 3.2))

p1 <- ggplot(demo, aes(x = reorder(group, value), y = value, fill = group)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  geom_text(aes(label = sprintf("%+.1f%%", value),
                hjust = ifelse(value < 0, 1.15, -0.15)), size = 5, colour = "grey15") +
  coord_flip(clip = "off") +
  scale_fill_ac(guide = "none") +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(
    title = paste0("Placeholder: X changed by ", k("example_change_pct", unit = TRUE),
                   " since ", k("example_period_start")),
    subtitle = "Template figure: replace with the case's hero chart",
    x = "Country", y = "Change (%)",              # always label both axes
    caption = source_caption("Placeholder dataset", accessed = Sys.Date())
  ) +
  theme_ac()

save_fig(p1, "fig1_hero")
message("Figures written to output/.")
