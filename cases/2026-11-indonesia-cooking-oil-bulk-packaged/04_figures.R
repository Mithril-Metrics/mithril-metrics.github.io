# 04_figures.R - figures for the report
# Case: 2026-11-indonesia-cooking-oil-bulk-packaged
# Input: output/tab_*.csv, output/key_numbers.csv, 00_meta.yml
# Output: output/fig1_hero, fig2_bulk_cap, fig3_producers (.png/.svg)

stopifnot("Run this script from the issue folder (AgEcon/Issue 7)" = file.exists("00_meta.yml"))
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
es <- read.csv("output/tab_event_study.csv") |>
  mutate(week = as.Date(week))
bulk_cap <- read.csv("output/tab_bulk_cap.csv")
prov_change <- read.csv("output/tab_province_change.csv")
accessed <- max(as.Date(access_log()$accessed))
cols <- ac_palette(3)
src <- "Bank Indonesia PIHPS, traditional markets"
d <- function(x) as.Date(x)

# ---- Fig 1 (hero): weekly change in the packaged-bulk gap ----
gap <- filter(es, outcome == "gap")
own <- filter(es, outcome != "gap") |>
  mutate(outcome = recode(outcome, packaged = "Branded packaged price", bulk = "Bulk price"))
top <- max(c(gap$hi_pct, own$beta_pct), na.rm = TRUE)
bands <- tibble(
  from = d(c("2022-02-01", "2022-04-28")),
  to = d(c("2022-03-15", "2022-05-23")),
  label = c("Caps on both oils", "Export ban")
)
p1 <- ggplot(gap, aes(week)) +
  geom_rect(data = bands, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey88") +
  geom_text(data = bands, aes(x = from + 2, y = top * 1.22, label = label),
            inherit.aes = FALSE, hjust = 0, size = 3.2, colour = "grey30") +
  annotate("segment", x = d("2022-04-02"), xend = d("2022-05-01"), y = top * 1.1,
           yend = top * 1.1, colour = "grey45", linewidth = 0.6) +
  annotate("text", x = d("2022-04-02"), y = top * 1.04, label = "Ramadan", hjust = 0,
           size = 3, colour = "grey40") +
  geom_vline(xintercept = d("2022-03-16"), colour = "grey25", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_line(data = own, aes(y = beta_pct, linetype = outcome), colour = "grey35",
            linewidth = 0.5) +
  geom_ribbon(aes(ymin = lo_pct, ymax = hi_pct), fill = cols[1], alpha = 0.25) +
  geom_line(aes(y = beta_pct, colour = "Packaged-bulk gap (95% interval)"), linewidth = 1) +
  geom_point(aes(y = beta_pct), colour = cols[1], size = 1.3) +
  scale_colour_manual(values = cols[1], name = NULL) +
  scale_linetype_manual(values = c("Branded packaged price" = "solid", "Bulk price" = "dotted"),
                        name = NULL) +
  labs(
    title = str_wrap(paste0("In traditional markets, branded cooking oil's premium over bulk oil was ",
                            k("nat_gap_ref_pct"), "% in the week of 7 March 2022, ",
                            k("nat_gap_post_preban_pct"), "% on average over the next six weeks and ",
                            k("nat_gap_post_ban_pct"), "% in June"), 60),
    subtitle = str_wrap(paste0("Indonesia, ", k("n_provinces"), " provinces, weekly, against the week of ",
                               "7 March. Blue: the gap between the two oils; grey lines: each oil's own ",
                               "price change"), 85),
    x = "Week (Monday)", y = "Change vs 7 Mar, %",
    caption = source_caption(src, accessed = accessed,
                             note = paste0("\nDashed line: 16 March, packaged cap removed and bulk cap raised.",
                                           "\nProvince fixed effects; 95% intervals from CR2 errors ",
                                           "clustered by province, each province counted equally.\nTitle: PIHPS national series, Rp per litre."))
  ) +
  theme_ac() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 10))
save_fig(p1, "fig1_hero", height_px = 1100)

# ---- Fig 2: bulk price against its own cap, by province ----
east <- unlist(meta$eastern_provinces)
bc <- bulk_cap |>
  filter(period %in% c("caps", "post_preban")) |>
  mutate(
    period = recode(period, caps = "1 Feb - 13 Mar (cap Rp11,500/l)",
                    post_preban = "21 Mar - 25 Apr (cap Rp14,000/l)"),
    label = if_else(province %in% east, paste0(province, " (east)"), province),
    label = if_else(province == meta$decisions$drop_province_check,
                    paste0(province, " (recording change?)"), label)
  )
ord <- bc |>
  filter(grepl("^21 Mar", period)) |>
  arrange(excess_pct) |>
  pull(label)
bc$label <- factor(bc$label, levels = unique(c(ord, bc$label)))
p2 <- ggplot(bc, aes(excess_pct, label, colour = period)) +
  geom_vline(xintercept = 0, colour = "grey30") +
  geom_point(size = 2.2) +
  scale_colour_manual(values = c(cols[3], cols[1]), name = NULL) +
  labs(
    title = str_wrap(paste0("Bulk cooking oil sold above its cap both before and after 16 March 2022: ",
                            k("bulk_excess_caps_median_pct"), "% above the Rp11,500 cap and then ",
                            k("bulk_excess_post_median_pct"), "% above the Rp14,000 cap in the ",
                            "median province"), 60),
    subtitle = str_wrap(paste0("Average bulk price in traditional markets relative to the cap in force, %. ",
                               "Nationally, bulk rose from Rp", k("nat_bulk_caps"), " to Rp",
                               k("nat_bulk_post_preban"), " per litre; above the new cap in ",
                               k("n_prov_bulk_above_cap_post"), " of ", k("n_prov_bulk_post"),
                               " provinces"), 85),
    x = "Bulk price above its cap, %", y = "Province",
    caption = source_caption(paste0(src, "; caps: Permendag 6/2022 and 11/2022"), accessed = accessed,
                             note = "\nPIHPS records prices where bulk oil was found; it cannot show shortages.")
  ) +
  guides(colour = guide_legend(nrow = 2)) +
  theme_ac(grid = "x") +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 8))
save_fig(p2, "fig2_bulk_cap", height_px = 1300)

# ---- Fig 3: producer provinces vs the rest ----
p_g <- v("gamma_post_preban_p")
title3 <- if (p_g >= 0.05) {
  "We cannot tell whether the packaged-bulk gap widened more in palm-oil-producing provinces after 16 March 2022"
} else if (v("gamma_post_preban_pct") > 0) {
  "The packaged-bulk gap widened more in palm-oil-producing provinces after 16 March 2022"
} else {
  "The packaged-bulk gap widened less in palm-oil-producing provinces after 16 March 2022"
}
grp <- prov_change |>
  group_by(producer) |>
  summarise(mean_pp = mean(change_pp), .groups = "drop") |>
  mutate(group = if_else(producer == 1, "Producer (>= 2% of output)", "Other provinces"))
pc <- prov_change |>
  mutate(group = if_else(producer == 1, "Producer (>= 2% of output)", "Other provinces"),
         share = 100 * prod_share)
p3 <- ggplot(pc, aes(share, change_pp, colour = group)) +
  geom_hline(data = grp, aes(yintercept = mean_pp, colour = group), linetype = "dashed") +
  geom_point(size = 2.4) +
  scale_x_sqrt(breaks = c(0, 1, 2, 5, 10, 20)) +
  scale_colour_manual(values = c(cols[3], cols[1]), name = NULL) +
  labs(
    title = str_wrap(title3, 60),
    subtitle = str_wrap(paste0("Change in each province's gap, 21 Mar - 25 Apr vs week of 7 Mar; ",
                               "dashed lines: group means. Model difference: ",
                               k("gamma_post_preban_pct"), "% (95% CI ", k("gamma_post_preban_lo"),
                               " to ", k("gamma_post_preban_hi"), ")"), 75),
    x = "Share of national palm oil output, 2021, % (square-root scale)",
    y = "Gap change, log points x 100",
    caption = source_caption(c("Bank Indonesia PIHPS", "palm oil output: Ditjenbun, Table 1.6"),
                             accessed = accessed,
                             note = paste0("\n", k("n_producer"), " producer provinces against ",
                                           v("n_provinces") - v("n_producer"), " others; ",
                                           k("n_prov_fig3"), " shown (the rest have no price in the ",
                                           "week of 7 March).\nThe interval rules out neither no ",
                                           "difference nor a large one."))
  ) +
  theme_ac() +
  theme(legend.position = "bottom")
save_fig(p3, "fig3_producers", height_px = 1100)
