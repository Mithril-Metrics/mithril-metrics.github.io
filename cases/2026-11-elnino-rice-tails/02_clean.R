# 02_clean.R — units, flags, detrending, El Niño classification ----------------
# Case: 2026-11-elnino-rice-tails (local folder: AgEcon/Issue 1/)
# Input:  output/cache/raw.rds   Output: output/cache/panel.rds, output/cache/oni_annual.rds
# All decisions below were fixed in the PLAN stage (see 00_meta.yml -> decisions).

stopifnot("Run this script from the issue folder (AgEcon/Issue 1)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})
raw <- readRDS("output/cache/raw.rds")

# --- 1. Rice panel in interpretable units -------------------------------------------
# FAOSTAT has reported yield as hg/ha ("100 g/ha") and, in some releases, kg/ha.
# Read the unit and convert to t/ha; cross-check against production / area.
yield_factor <- function(unit) {
  dplyr::case_when(unit %in% c("hg/ha", "100 g/ha") ~ 1e-4,
                   unit == "kg/ha" ~ 1e-3,
                   unit == "t/ha"  ~ 1,
                   TRUE ~ NA_real_)
}

rice <- raw$rice |>
  left_join(raw$countries, by = c("area_code" = "fao_code")) |>
  mutate(var = recode(element, "Yield" = "yield", "Area harvested" = "area", "Production" = "prod"),
         f = ifelse(var == "yield", yield_factor(unit), 1),
         value = value * f)
bad_units <- unique(rice$unit[rice$var == "yield" & is.na(rice$f)])
if (length(bad_units)) stop("Unknown yield unit(s): ", paste(bad_units, collapse = ", "))
cat("Units: ", paste(unique(paste(rice$var, rice$unit, sep = " in ")), collapse = "; "),
    " (yield converted to t/ha)\n")

panel <- rice |>
  select(iso3, country = area, year, var, value) |>
  pivot_wider(names_from = var, values_from = value) |>
  left_join(rice |> filter(var == "yield") |> select(iso3, year, flag), by = c("iso3", "year")) |>
  arrange(iso3, year)

# Unit cross-check: reported yield vs production / area (should agree within ~1%)
xchk <- with(panel, summary(yield / (prod / area)))
cat("Reported yield / (production/area):\n")
print(xchk)
stopifnot("Yield unit conversion looks wrong" = abs(median(panel$yield / (panel$prod / panel$area), na.rm = TRUE) - 1) < 0.02)

# Pre-registered sample rule: Cambodia from 1980 (1970s values unreliable / imputed)
panel <- panel |> filter(!(iso3 == "KHM" & year < 1980), !is.na(yield), yield > 0)

cat("\nYield (t/ha) by country, first and last year:\n")
print(panel |> group_by(iso3) |> summarise(first = first(yield), last = last(yield),
                                          years = paste(range(year), collapse = "-"), n = n()))
cat("\nFAO flags on yield (A = official, E = estimated, I = imputed):\n")
print(table(panel$iso3, panel$flag, useNA = "ifany"))

# --- 2. El Niño classification from ONI ---------------------------------------------------
# Onset-year index: mean ONI anomaly over Jul-Dec (seasons JAS, ASO, SON, OND) of year t.
# NOAA threshold: >= +0.5 El Niño, >= +1.5 strong; <= -0.5 La Niña.
oni_annual <- raw$oni |>
  filter(SEAS %in% c("JAS", "ASO", "SON", "OND")) |>
  group_by(year = YR) |>
  summarise(oni_jd = mean(ANOM), n_seas = n(), .groups = "drop") |>
  filter(n_seas == 4) |>
  mutate(elnino = oni_jd >= 0.5, strong = oni_jd >= 1.5, lanina = oni_jd <= -0.5)

panel <- panel |>
  left_join(oni_annual |> select(year, oni_jd, elnino, strong, lanina), by = "year") |>
  left_join(oni_annual |> transmute(year = year + 1L, elnino_lag = elnino, lanina_lag = lanina), by = "year")
stopifnot(!anyNA(panel$elnino), !anyNA(panel$elnino_lag))

cat("\nEl Niño onset years in sample:", paste(sort(unique(panel$year[panel$elnino])), collapse = ", "), "\n")
cat("Strong El Niño years:", paste(sort(unique(panel$year[panel$strong])), collapse = ", "), "\n")

# --- 3. Detrending: yield shock = % deviation from each country's technology trend -----
# Main: log yield ~ natural spline (4 df) per country; Indonesia gets a level shift from 2018,
# when BPS replaced eye-estimates with an area-sampling frame (series break).
# R2: robust LOESS (span 0.5) on log yield after removing the Indonesian level shift.
detrend <- function(d) {
  d <- d[order(d$year), ]
  brk <- as.numeric(d$iso3 == "IDN" & d$year >= 2018)
  X <- splines::ns(d$year, df = 4)
  m <- if (any(brk == 1)) lm(log(d$yield) ~ X + brk) else lm(log(d$yield) ~ X)
  d$shock <- residuals(m)
  shift <- if (any(brk == 1)) coef(m)[["brk"]] else 0
  y_adj <- log(d$yield) - shift * brk
  lo <- stats::loess(y_adj ~ d$year, span = 0.5, family = "symmetric", degree = 1)
  d$shock_loess <- y_adj - fitted(lo)
  d$idn_shift <- shift
  # Same trend model for harvested area and production (used in the "second look"):
  # log(production) = log(area) + log(yield), so the three shocks decompose additively (approx.)
  for (v in c("area", "prod")) {
    mv <- if (any(brk == 1)) lm(log(d[[v]]) ~ X + brk) else lm(log(d[[v]]) ~ X)
    d[[paste0("shock_", v)]] <- residuals(mv)
  }
  # Alternative production trends (third review): a more flexible 6 df spline, and robust LOESS
  # after removing the same Indonesian level shift
  X6 <- splines::ns(d$year, df = 6)
  m6 <- if (any(brk == 1)) lm(log(d$prod) ~ X6 + brk) else lm(log(d$prod) ~ X6)
  d$shock_prod_ns6 <- residuals(m6)
  mp <- if (any(brk == 1)) lm(log(d$prod) ~ X + brk) else lm(log(d$prod) ~ X)
  p_adj <- log(d$prod) - (if (any(brk == 1)) coef(mp)[["brk"]] else 0) * brk
  lo_p <- stats::loess(p_adj ~ d$year, span = 0.5, family = "symmetric", degree = 1)
  d$shock_prod_loess <- p_adj - fitted(lo_p)
  d
}
panel <- panel |> group_split(iso3) |> lapply(detrend) |> bind_rows()
cat("\nIndonesia 2018 level shift (log points):", round(unique(panel$idn_shift[panel$iso3 == "IDN"]), 3), "\n")

# Typical variability per country: robust SD (MAD, consistent for the normal) of shocks.
# The "normal assumption" benchmark uses this sigma: what a planner assuming normal shocks
# with the usual year-to-year noise would expect.
panel <- panel |>
  group_by(iso3) |>
  mutate(sigma = mad(shock), sigma_loess = mad(shock_loess)) |>
  ungroup() |>
  mutate(
    thr5  = log(0.95), thr10 = log(0.90),                 # 5% / 10% below trend, in log points
    loss5 = shock <= thr5, loss10 = shock <= thr10,
    loss5_loess = shock_loess <= thr5,
    pnorm5 = pnorm(thr5 / sigma), pnorm10 = pnorm(thr10 / sigma),
    z_loss = -shock / sigma                                # standardised loss, for the tail fit
  )

check_panel(panel, id = "iso3", time = "year", vars = c("yield", "area", "prod", "shock"))
cat("\nShock SD by country (% , robust):\n")
print(panel |> distinct(iso3, sigma) |> mutate(sigma_pct = round(100 * sigma, 2)))

# --- 4. Flags for the "second look" (added after the main results, see 03b) ------------
# ENSO phase with neutral years as reference; maritime vs mainland; major armed conflict
# (UCDP intensity 2 on the country's contested government); 1997-98 Asian financial crisis.
gw <- list(VNM = c(816, 817), IDN = 850, PHL = 840, THA = 800, MYS = 820, KHM = 811)
ucdp_cy <- raw$ucdp |>
  mutate(year = as.integer(year), intensity_level = as.integer(intensity_level)) |>
  filter(intensity_level == 2) |>
  tidyr::separate_rows(gwno_loc, sep = ",\\s*") |>
  mutate(gwno = as.integer(gwno_loc))
war <- bind_rows(lapply(names(gw), function(i)
  tibble(iso3 = i, year = unique(ucdp_cy$year[ucdp_cy$gwno %in% gw[[i]]]))))
panel <- panel |>
  mutate(enso3 = factor(ifelse(elnino, "El Nino", ifelse(lanina, "La Nina", "Neutral")),
                        levels = c("Neutral", "El Nino", "La Nina")),
         region = ifelse(iso3 %in% c("IDN", "PHL", "MYS"), "Maritime", "Mainland"),
         crisis9798 = year %in% 1997:1998) |>
  left_join(war |> mutate(major_conflict = TRUE), by = c("iso3", "year")) |>
  mutate(major_conflict = dplyr::coalesce(major_conflict, FALSE))
cat("\nENSO phase counts (country-years):\n")
print(table(panel$enso3))
cat("Major-conflict country-years (UCDP intensity 2):\n")
print(panel |> filter(major_conflict) |> group_by(iso3) |>
        summarise(n = n(), years = paste(range(year), collapse = "-"), .groups = "drop"))

saveRDS(panel, "output/cache/panel.rds")
saveRDS(oni_annual, "output/cache/oni_annual.rds")
message("02_clean.R done: ", nrow(panel), " country-years.")
