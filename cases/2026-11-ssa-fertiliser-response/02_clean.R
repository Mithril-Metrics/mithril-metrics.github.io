# 02_clean.R — price ratio, maize and nitrogen panel, subsidy groups -------------
# Case: 2026-11-ssa-fertiliser-response (local folder: AgEcon/Issue 2/)
# Input:  output/cache/raw.rds
# Output: output/cache/panel.rds, ratio_month.rds, ratio_year.rds, output/tab_subsidy_groups.csv

stopifnot("Run this script from the issue folder (AgEcon/Issue 2)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})
raw <- readRDS("output/cache/raw.rds")
countries <- raw$countries

# ---- Urea-to-maize price ratio ----
# Both series in nominal $/mt, so the ratio is unit-free: tonnes of maize that buy a tonne of urea.
ratio_month <- raw$pink |>
  select(date, commodity, price) |>
  pivot_wider(names_from = commodity, values_from = price) |>
  filter(!is.na(Urea), !is.na(Maize)) |>
  mutate(ratio = Urea / Maize,
         month = as.integer(format(date, "%m")),
         year = as.integer(format(date, "%Y")),
         crop_year = year + as.integer(month >= 10)) |>
  arrange(date)

# Crop year t = Oct(t-1) to Sep(t): covers planting in southern (Nov-Dec), eastern (Mar)
# and western Africa (May-Jul) for a harvest that FAOSTAT records in year t.
ratio_year <- ratio_month |>
  group_by(crop_year) |>
  summarise(ratio = mean(ratio), n_months = n(), .groups = "drop") |>
  filter(n_months == 12) |>
  transmute(year = crop_year, ratio, log_ratio = log(ratio)) |>
  mutate(log_ratio_lag = lag(log_ratio))
stopifnot(all(diff(ratio_year$year) == 1))

cat("Ratio, crop years", paste(range(ratio_year$year), collapse = "-"),
    "| latest month", format(max(ratio_month$date)), "=", round(tail(ratio_month$ratio, 1), 2), "\n")
print(tail(ratio_year, 6))

# ---- ENSO: DJF anomaly of year t (Dec t-1 to Feb t) ----
oni <- raw$oni |>
  filter(SEAS == "DJF") |>
  transmute(year = YR, oni = ANOM)

# ---- Maize yield in t/ha ----
yield_factor <- function(unit) {
  case_when(unit %in% c("hg/ha", "100 g/ha") ~ 1e-4,
            unit == "kg/ha" ~ 1e-3,
            unit == "t/ha" ~ 1,
            TRUE ~ NA_real_)
}
maize <- raw$maize |>
  filter(!is_aggregate) |>
  left_join(countries, by = c("area" = "fao_name")) |>
  mutate(var = recode(element, "Yield" = "yield", "Area harvested" = "area", "Production" = "prod"),
         value = ifelse(var == "yield", value * yield_factor(unit), value))
if (anyNA(maize$value[maize$var == "yield"])) stop("Unknown yield unit: ", paste(unique(maize$unit), collapse = ", "))
maize_w <- maize |>
  select(iso3, year, var, value) |>
  pivot_wider(names_from = var, values_from = value) |>
  left_join(maize |> filter(var == "yield") |> select(iso3, year, yield_flag = flag), by = c("iso3", "year"))

xchk <- median(maize_w$yield / (maize_w$prod / maize_w$area), na.rm = TRUE)
cat("Maize: median of reported yield / (production / area) =", round(xchk, 3), "\n")
stopifnot("Yield unit conversion looks wrong" = abs(xchk - 1) < 0.02)

# ---- Nitrogen use per ha of cropland ----
# Flags: A official, X from international organisations, I imputed by FAO, E estimated.
nuse <- raw$rfn |>
  filter(!is_aggregate, element == "Use per area of cropland") |>
  left_join(countries, by = c("area" = "fao_name")) |>
  transmute(iso3, year, n_ha = value, n_unit = unit, n_flag = flag,
            n_official = flag %in% c("A", "X"))
cat("N use unit:", paste(unique(nuse$n_unit), collapse = ", "), "\n")
stopifnot("N use per area expected in kg/ha" = all(nuse$n_unit == "kg/ha"))
cat("N use flags (share):\n")
print(round(prop.table(table(nuse$n_flag, useNA = "ifany")), 2))

# ---- MAFAP: input-subsidy intensity, fixed on budget years up to 2019 ----
norm_name <- function(x) sub("^united republic of ", "", tolower(trimws(x)))
mafap <- raw$mafap
col_b1  <- "B1. Variable inputs"
col_tot <- grep("^III\\. Total expenditures", names(mafap), value = TRUE)
stopifnot(length(col_tot) == 1)

subsidy <- mafap |>
  filter(`Budgeted/actual` == "Actual", `Donor/national/total` == "Total") |>
  transmute(key = norm_name(Country),
            year = as.integer(substr(Year, 1, 4)),
            b1 = suppressWarnings(as.numeric(.data[[col_b1]])),
            total = suppressWarnings(as.numeric(.data[[col_tot]]))) |>
  filter(!is.na(b1), !is.na(total), total > 0) |>
  distinct(key, year, .keep_all = TRUE) |>        # same shares in every currency variant
  mutate(share = 100 * b1 / total)

groups <- countries |>
  mutate(key = norm_name(fao_name)) |>
  left_join(subsidy |> filter(year <= 2019), by = "key") |>
  group_by(iso3, country = fao_name) |>
  summarise(b1_share = mean(share), first_year = min(year), last_year = max(year),
            n_years = sum(!is.na(share)), .groups = "drop")
miss <- groups$country[is.na(groups$b1_share)]
if (length(miss)) stop("No MAFAP B1 data up to 2019 for: ", paste(miss, collapse = ", "))
groups <- groups |>
  mutate(high = b1_share > median(b1_share),
         group = ifelse(high, "High input subsidies", "Low input subsidies")) |>
  arrange(desc(b1_share))
cat("\nMAFAP B1 (variable inputs) as % of total support to food and agriculture:\n")
print(groups, n = 20)
readr::write_csv(groups |> mutate(b1_share = round(b1_share, 1)), "output/tab_subsidy_groups.csv")

# ---- Panel ----
panel <- countries |>
  rename(country = fao_name) |>
  tidyr::crossing(year = 1990:2024) |>
  left_join(maize_w, by = c("iso3", "year")) |>
  left_join(nuse, by = c("iso3", "year")) |>
  left_join(ratio_year, by = "year") |>
  left_join(oni, by = "year") |>
  left_join(groups |> select(iso3, b1_share, high, group), by = "iso3") |>
  mutate(t = year - 1990,
         log_yield = ifelse(yield > 0, log(yield), NA_real_),
         log_n = ifelse(n_ha > 0, log(n_ha), NA_real_),
         ratio_high = log_ratio * high) |>
  arrange(iso3, year)

check_panel(panel, id = "iso3", time = "year", vars = c("log_yield", "log_n", "log_ratio", "oni"))
cat("\nZero or missing N use (dropped from N models):", sum(is.na(panel$log_n)), "country-years\n")

saveRDS(panel, "output/cache/panel.rds")
saveRDS(ratio_month, "output/cache/ratio_month.rds")
saveRDS(ratio_year, "output/cache/ratio_year.rds")
