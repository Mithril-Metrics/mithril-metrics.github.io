# 02_clean.R — one row per country: water, crop output, cropland, stress (means 2020-2022)
# Input: output/cache/raw.rds. Output: output/cache/panel.rds, output/tab_inputs.csv
# Sample: member states with irrigated area >= 2% of UAA in 2023 (1% if fewer than 10).

stopifnot("Run this script from the issue folder (AgEcon/Issue 3)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})
raw <- readRDS("output/cache/raw.rds")
eu <- raw$eu
win <- 2020:2022

mean_win <- function(d) {
  d |>
    filter(time %in% win, !is.na(values)) |>
    group_by(geo) |>
    summarise(value = mean(values), n_years = n(), .groups = "drop")
}

# ---- Water ----
# Irrigation abstraction where reported; otherwise AQUASTAT agricultural withdrawals.
# AQUASTAT also counts livestock and aquaculture, so filled values are upper bounds.
w_irr <- raw$water |>
  filter(wat_proc == "ABS_AGR_IR") |>
  mean_win() |>
  rename(water = value, water_years = n_years)

aqua <- raw$aqua |>
  filter(!is.na(value)) |>
  select(iso3, year, indicator, value) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  filter(year %in% win) |>
  mutate(agri = `ER.H2O.FWAG.ZS` / 100 * `ER.H2O.FWTL.K3` * 1000) |>   # bn m3 -> million m3
  filter(!is.na(agri)) |>
  group_by(iso3) |>
  summarise(water_aqua = mean(agri), aqua_years = n(), .groups = "drop") |>
  left_join(eu, by = "iso3")

# How far apart are the two sources where both exist? Printed as a caveat.
both <- inner_join(w_irr, aqua, by = "geo")
fill_ratio <- median(both$water / both$water_aqua)
cat("Countries with both sources:", nrow(both),
    "| median Eurostat irrigation / AQUASTAT agriculture:", round(fill_ratio, 2), "\n")

water <- eu |>
  select(geo) |>
  left_join(w_irr, by = "geo") |>
  left_join(select(aqua, geo, water_aqua, aqua_years), by = "geo") |>
  mutate(filled = is.na(water) & !is.na(water_aqua),
         water = if_else(is.na(water), water_aqua, water),
         water_source = case_when(!filled & !is.na(water) ~ "Eurostat irrigation",
                                  filled ~ "AQUASTAT agriculture (filled)",
                                  TRUE ~ NA_character_)) |>
  select(geo, water, water_source, filled)
cat("Filled from AQUASTAT:", paste(water$geo[water$filled], collapse = ", "), "\n")

# ---- Other variables ----
crop <- mean_win(raw$crop) |>
  select(geo, output = value)
land <- mean_win(raw$cropland) |>
  select(geo, cropland = value)
wei <- mean_win(raw$wei) |>
  select(geo, wei = value)
irr_share <- raw$irr |>
  filter(unit == "PC", !is.na(values)) |>
  group_by(geo) |>
  filter(time == max(time)) |>
  ungroup() |>
  select(geo, irr_share = values, irr_year = time)
cat("Irrigated-area year(s):", paste(unique(irr_share$irr_year), collapse = ", "), "\n")

all <- eu |>
  left_join(water, by = "geo") |>
  left_join(crop, by = "geo") |>
  left_join(land, by = "geo") |>
  left_join(wei, by = "geo") |>
  left_join(irr_share, by = "geo")

# ---- Sample ----
threshold <- 2
keep <- filter(all, irr_share >= threshold)
if (nrow(keep) < 10) {
  threshold <- 1
  keep <- filter(all, irr_share >= threshold)
}
dropped_na <- keep$name[!complete.cases(keep[c("water", "output", "cropland")])]
if (length(dropped_na)) cat("Dropped for missing data:", paste(dropped_na, collapse = ", "), "\n")
panel <- keep |>
  filter(complete.cases(water, output, cropland))

cat("Threshold:", threshold, "% of UAA | countries:", nrow(panel),
    "| filled:", sum(panel$filled), "\n")
cat("Below threshold:", paste(setdiff(all$name, keep$name), collapse = ", "), "\n")
cat("Units: water million m3; output", unique(raw$crop$measure), "; cropland 1000 ha; WEI+ %\n")
print(as.data.frame(arrange(panel, desc(irr_share))), digits = 3)
stopifnot("Fewer than 8 countries left: check the inputs" = nrow(panel) >= 8)

attr(panel, "threshold") <- threshold
attr(panel, "fill_ratio") <- fill_ratio
attr(panel, "output_measure") <- unique(raw$crop$measure)
saveRDS(panel, "output/cache/panel.rds")
readr::write_csv(all, "output/tab_inputs.csv")
