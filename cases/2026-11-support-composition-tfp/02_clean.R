# 02_clean.R — support ratios by window, TFP growth, analysis samples --------------
# Case: 2026-11-support-composition-tfp (local folder: AgEcon/Issue 5/)
# Input:  output/cache/raw.rds
# Output: output/cache/clean.rds, output/tab_coverage.csv

stopifnot("Run this script from the issue folder (AgEcon/Issue 5)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(tidyr)
})
raw <- readRDS("output/cache/raw.rds")
oecd <- raw$oecd
stock_heavy <- c("IND", "CHN", "IDN", "PHL")

# OECD values come with a unit multiplier (6 = millions)
tidy_oecd <- function(d) {
  d |>
    filter(REF_AREA %in% raw$units, !is.na(OBS_VALUE)) |>
    mutate(mult = ifelse(is.na(UNIT_MULT), 0, as.numeric(UNIT_MULT)),
           value = OBS_VALUE * 10^mult,
           year = as.integer(TIME_PERIOD))
}

# Commodity-level tables also carry an all-commodity row; keep only that one
pick_total <- function(d, flow) {
  if (!"COMMODITY" %in% names(d) || n_distinct(d$COMMODITY) == 1) {
    return(d)
  }
  labs <- distinct(d, COMMODITY, Commodity)
  hit <- labs$COMMODITY[grepl("^total|^all commodities|^all$", labs$Commodity, ignore.case = TRUE)]
  if (length(hit) != 1) {
    print(labs, n = 80)
    stop(flow, ": cannot tell which commodity code is the all-commodity total (see list above).")
  }
  cat(flow, ": all-commodity code", hit, "(", labs$Commodity[labs$COMMODITY == hit], ")\n")
  filter(d, COMMODITY == hit)
}

# unit = NULL keeps whatever unit the measure has (shares of GSSE come as percentages)
get_measure <- function(flow, measure, unit = NULL) {
  d <- tidy_oecd(oecd[[flow]]) |>
    filter(MEASURE == measure, is.null(unit) | UNIT_MEASURE %in% unit)
  if (nrow(d) == 0) {
    return(NULL)
  }
  d <- pick_total(d, flow)
  stopifnot("Duplicate unit-years" = !anyDuplicated(d[c("REF_AREA", "year")]))
  transmute(d, iso3 = REF_AREA, year, value)
}

# VP and MPS: the PSE table first, the commodity table as fallback
measure_any <- function(measure, unit) {
  for (f in c("DF_PSE", "DF_COMM")) {
    d <- get_measure(f, measure, unit)
    if (!is.null(d)) {
      cat(measure, unit, "from", f, "\n")
      return(d)
    }
  }
  NULL
}

gsse_xdc <- get_measure("DF_GSSE", "GSSE", "XDC")
gsse_usd <- get_measure("DF_GSSE", "GSSE", "USD")
akis_sh <- get_measure("DF_COMGSSE", "GSSEHSH")
infra_sh <- get_measure("DF_COMGSSE", "GSSEJSH")
stock_sh <- get_measure("DF_COMGSSE", "GSSELSH")
vp_usd <- measure_any("VP", "USD")
vp_xdc <- measure_any("VP", "XDC")
mps_usd <- measure_any("MPS", "USD")
mps_xdc <- measure_any("MPS", "XDC")
tse_usd <- get_measure("DF_TSE", "TSE", "USD")
stopifnot("GSSE, AKIS share or value of production not found" =
            !is.null(gsse_xdc) && !is.null(gsse_usd) && !is.null(akis_sh) &&
            (!is.null(vp_usd) || !is.null(vp_xdc)))
if (is.null(tse_usd)) {
  message("TSE in USD not found in DF_TSE: the MPS share of total support is skipped.")
}

j <- function(x, name) {
  if (is.null(x)) {
    return(tibble::tibble(iso3 = character(), year = integer(), !!name := numeric()))
  }
  rename(x, !!name := value)
}
ann <- expand_grid(iso3 = raw$units, year = 1990:2025) |>
  left_join(j(gsse_xdc, "gsse_xdc"), by = c("iso3", "year")) |>
  left_join(j(gsse_usd, "gsse_usd"), by = c("iso3", "year")) |>
  left_join(j(akis_sh, "akis_sh"), by = c("iso3", "year")) |>
  left_join(j(infra_sh, "infra_sh"), by = c("iso3", "year")) |>
  left_join(j(stock_sh, "stock_sh"), by = c("iso3", "year")) |>
  left_join(j(vp_usd, "vp_usd"), by = c("iso3", "year")) |>
  left_join(j(vp_xdc, "vp_xdc"), by = c("iso3", "year")) |>
  left_join(j(mps_usd, "mps_usd"), by = c("iso3", "year")) |>
  left_join(j(mps_xdc, "mps_xdc"), by = c("iso3", "year")) |>
  left_join(j(tse_usd, "tse_usd"), by = c("iso3", "year"))

# USD per unit of national currency, implied by GSSE; used only where the USD series is missing.
# Every ratio is computed within a year, so the rate only sets the weights in multi-year sums.
ann <- ann |>
  mutate(fx = ifelse(gsse_xdc != 0, gsse_usd / gsse_xdc, NA_real_),
         vp = coalesce(vp_usd, vp_xdc * fx),
         mps = coalesce(mps_usd, mps_xdc * fx),
         akis = akis_sh / 100 * gsse_usd,
         infra = infra_sh / 100 * gsse_usd,
         stock = stock_sh / 100 * gsse_usd)

cat("\nAnnual panel:", nrow(ann), "unit-years |", length(unique(ann$iso3)), "units\n")
cat("NA share: akis", round(mean(is.na(ann$akis)), 2), "| vp", round(mean(is.na(ann$vp)), 2),
    "| mps", round(mean(is.na(ann$mps)), 2), "| infra", round(mean(is.na(ann$infra)), 2), "\n")
cat("Units: support in USD; AKIS, infrastructure and stockholding are % of GSSE in the source\n")
stopifnot("AKIS share of GSSE outside 0-100" = all(between(ann$akis_sh, 0, 100), na.rm = TRUE))

coverage <- ann |>
  filter(iso3 != "OECD") |>
  group_by(iso3) |>
  summarise(akis_first = suppressWarnings(min(year[!is.na(akis) & !is.na(vp)])),
            akis_last = suppressWarnings(max(year[!is.na(akis) & !is.na(vp)])),
            n_years = sum(!is.na(akis) & !is.na(vp)),
            .groups = "drop")
print(coverage, n = 40)
readr::write_csv(coverage, "output/tab_coverage.csv")

# ---- Support ratios by window: ratio of sums, >= 3 years with both parts ----
win <- function(years, label) {
  ann |>
    filter(year %in% years) |>
    group_by(iso3) |>
    summarise(
      n = sum(!is.na(akis) & !is.na(vp)),
      akis = 100 * sum(akis[!is.na(akis) & !is.na(vp)]) / sum(vp[!is.na(akis) & !is.na(vp)]),
      mps = 100 * sum(mps[!is.na(mps) & !is.na(vp)]) / sum(vp[!is.na(mps) & !is.na(vp)]),
      infra = 100 * sum(infra[!is.na(infra) & !is.na(vp)]) / sum(vp[!is.na(infra) & !is.na(vp)]),
      .groups = "drop"
    ) |>
    mutate(across(c(akis, mps, infra), ~ ifelse(n >= 3, .x, NA_real_)),
           window = label)
}

# ---- TFP growth: 100 x slope of ln TFP on year, base year included ----
tfp <- raw$tfp |>
  transmute(iso3, year, tfp = value)
stopifnot("TFP index must be positive" = all(tfp$tfp > 0, na.rm = TRUE))
growth <- function(years, label) {
  tfp |>
    filter(year %in% years) |>
    group_by(iso3) |>
    summarise(n = sum(!is.na(tfp)),
              g = if (n == length(years)) 100 * unname(coef(lm(log(tfp) ~ year))[2]) else NA_real_,
              .groups = "drop") |>
    mutate(window = label)
}

# ---- Long difference ----
gdp <- raw$wdi |>
  group_by(iso3 = iso3c) |>
  summarise(ln_gdppc = log(mean(gdppc, na.rm = TRUE)), .groups = "drop")
ld_all <- tibble::tibble(iso3 = raw$countries) |>
  left_join(select(growth(2009:2023, "post"), iso3, g_post = g), by = "iso3") |>
  left_join(select(growth(1990:2000, "pre"), iso3, g_pre = g), by = "iso3") |>
  left_join(select(win(2000:2009, "2000-09"), iso3, akis, mps, infra, n_win = n), by = "iso3") |>
  left_join(gdp, by = "iso3")
ld <- filter(ld_all, if_all(c(g_post, g_pre, akis, mps, infra, ln_gdppc), ~ !is.na(.x)))
cat("\nLong difference: ", nrow(ld), "of", nrow(ld_all), "countries\n")
if (nrow(ld) < nrow(ld_all)) {
  cat("Excluded (missing data):", paste(setdiff(ld_all$iso3, ld$iso3), collapse = ", "), "\n")
}
print(summary(select(ld, g_post, g_pre, akis, mps, infra, ln_gdppc)))

# ---- FE panel: outcome period p, support in the previous 5-year window ----
periods <- tibble::tribble(
  ~period, ~g_years, ~lag_years,
  "2005-09", 2004:2009, 2000:2004,
  "2010-14", 2009:2014, 2005:2009,
  "2015-19", 2014:2019, 2010:2014,
  "2020-23", 2019:2023, 2015:2019
)
fe <- bind_rows(lapply(seq_len(nrow(periods)), function(i) {
  growth(periods$g_years[[i]], periods$period[i]) |>
    select(iso3, g) |>
    left_join(select(win(periods$lag_years[[i]], "lag"), iso3, akis_l = akis, mps_l = mps,
                     infra_l = infra), by = "iso3") |>
    mutate(period = periods$period[i])
})) |>
  filter(iso3 %in% raw$countries, !is.na(g), !is.na(akis_l), !is.na(mps_l), !is.na(infra_l))
cat("FE panel:", nrow(fe), "country-periods |", length(unique(fe$iso3)), "countries\n")
print(count(fe, period))

# ---- Descriptive windows (EU included) ----
base <- win(2000:2004, "2000-04")
now5 <- win(2021:2025, "2021-25")
now3 <- ann |>
  filter(year %in% 2023:2025) |>
  group_by(iso3) |>
  summarise(n_now = sum(!is.na(akis) & !is.na(vp)),
            akis_usd = mean(akis, na.rm = TRUE),
            vp_usd = mean(vp, na.rm = TRUE),
            mps_usd = mean(mps, na.rm = TRUE),
            tse_usd = mean(tse_usd, na.rm = TRUE),
            akis_now = 100 * sum(akis[!is.na(akis) & !is.na(vp)]) / sum(vp[!is.na(akis) & !is.na(vp)]),
            .groups = "drop")

rebal <- select(base, iso3, akis_base = akis) |>
  inner_join(now3, by = "iso3") |>
  filter(iso3 != "OECD", !is.na(akis_base), n_now >= 2) |>
  mutate(gap_pp = akis_base - akis_now,
         gap_usd = pmax(gap_pp, 0) / 100 * vp_usd,
         gap_pct_mps = ifelse(mps_usd > 0, 100 * gap_usd / mps_usd, NA_real_),
         mps_pct_tse = ifelse(!is.na(tse_usd) & tse_usd > 0, 100 * mps_usd / tse_usd, NA_real_),
         stock_heavy = iso3 %in% stock_heavy)
cat("Rebalancing table:", nrow(rebal), "units\n")

dumb <- select(base, iso3, akis_base = akis) |>
  inner_join(select(now5, iso3, akis_now = akis), by = "iso3") |>
  filter(iso3 != "OECD", !is.na(akis_base), !is.na(akis_now)) |>
  mutate(stock_heavy = iso3 %in% stock_heavy)
cat("Dumbbell:", nrow(dumb), "units\n")

saveRDS(list(ann = ann, ld = ld, ld_all = ld_all, fe = fe, rebal = rebal, dumb = dumb,
             coverage = coverage, stock_heavy = stock_heavy, accessed = raw$accessed),
        "output/cache/clean.rds")
