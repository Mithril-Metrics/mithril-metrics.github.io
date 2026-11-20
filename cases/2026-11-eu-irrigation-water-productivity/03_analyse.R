# 03_analyse.R — water-directional DEA, output per m3, link to water stress, robustness
# Revised after the first run (2026-11-20): inputs rainfed cropland, irrigated area and
# water; constant returns to scale.
# Input: output/cache/panel.rds. Output: output/key_numbers.csv, output/tab_results.csv,
# output/tab_robustness.csv, output/cache/results.rds, output/sessionInfo.txt

stopifnot("Run this script from the issue folder (AgEcon/Issue 3)" = file.exists("00_meta.yml"))
options(acutils.data_dir = "data_raw")
suppressPackageStartupMessages({
  library(acutils)
  library(dplyr)
  library(Benchmarking)
})
set.seed(20261120)
panel <- readRDS("output/cache/panel.rds")
src <- "Eurostat env_wat_abs, aact_eaa01, ef_fsi_irri, sdg_06_60; FAOSTAT RL; AQUASTAT via World Bank"

# Sub-vector DEA: contract water only, holding rainfed land, irrigated land and crop
# output fixed. beta = share of water a country could save inside the frontier.
# Separate land inputs stop rainfed farming from standing in for irrigation water;
# CRS because country totals are not firms choosing a size (VRS put the smallest and
# largest countries on the frontier by construction in the first run).
water_dea <- function(d, ref = d, w = d$water, w_ref = ref$water) {
  x <- cbind(d$rainfed, d$irrigated, w)
  y <- cbind(d$output)
  xr <- cbind(ref$rainfed, ref$irrigated, w_ref)
  yr <- cbind(ref$output)
  e <- dea.direct(x, y, DIRECT = cbind(0, 0, w), RTS = "crs", ORIENTATION = "in",
                  XREF = xr, YREF = yr)
  e$objval
}

res <- panel |>
  mutate(beta = water_dea(panel),
         saving_pct = 100 * beta,
         saving_m3 = beta * water,                       # million m3
         out_per_m3 = output / water,                    # output units per m3 (million / million)
         m3_per_ha = water / irrigated * 1000,           # million m3 / 1000 ha -> m3/ha
         rank_dea = rank(-saving_pct, ties.method = "min"),  # 1 = largest saving potential
         rank_ppm = rank(out_per_m3, ties.method = "min"),   # 1 = least output per m3
         frontier = beta < 1e-6)
stopifnot("DEA returned missing or negative values" = all(!is.na(res$beta) & res$beta > -1e-6))

cat("Water-directional DEA (CRS, input orientation; inputs rainfed cropland, irrigated area,",
    "water; output crop output)\n")
print(as.data.frame(res |>
                      arrange(desc(saving_pct)) |>
                      select(name, filled, water, output, rainfed, irrigated, wei,
                             m3_per_ha, out_per_m3, saving_pct, saving_m3, frontier)), digits = 3)

# ---- Link to water stress ----
st <- filter(res, !is.na(wei))
sp <- suppressWarnings(cor.test(st$saving_pct, st$wei, method = "spearman", exact = FALSE))
cat("Spearman saving share vs WEI+: rho", round(sp$estimate, 2), "| p", round(sp$p.value, 3),
    "| n", nrow(st), "\n")
hi_stress <- st$wei >= median(st$wei)
hi_saving <- st$saving_pct >= median(st$saving_pct)

# ---- Partial productivity vs DEA ----
n_disagree <- sum(abs(res$rank_dea - res$rank_ppm) >= 3)
rho_ppm <- cor(res$saving_pct, 1 / res$out_per_m3, method = "spearman")

# ---- R1: Eurostat-only sample ----
eo <- filter(res, !filled)
r1 <- eo |>
  mutate(saving_eo = 100 * water_dea(eo)) |>
  select(geo, saving_eo)
r1_rho <- cor(res$saving_pct[match(r1$geo, res$geo)], r1$saving_eo, method = "spearman")
cat("R1 Eurostat-only: n", nrow(eo), "| Spearman with headline", round(r1_rho, 2), "\n")

# ---- R2: leave one frontier country out of the reference set ----
front <- res$geo[res$frontier]
loo <- lapply(front, function(g) {
  ref <- filter(res, geo != g)
  tibble::tibble(geo = res$geo, dropped = g, saving = 100 * water_dea(res, ref))
}) |>
  bind_rows() |>
  filter(geo != dropped) |>
  mutate(saving = if_else(is.finite(saving), saving, NA_real_))   # infeasible without that peer
cat("R2 infeasible evaluations:", sum(is.na(loo$saving)), "\n")
r2 <- loo |>
  group_by(geo) |>
  summarise(loo_min = min(saving, na.rm = TRUE), loo_max = max(saving, na.rm = TRUE),
            .groups = "drop")
r2_shift <- max(abs(r2$loo_min - res$saving_pct[match(r2$geo, res$geo)]))
cat("R2 leave-one-out over", length(front), "frontier countries | largest change",
    round(r2_shift, 1), "pp\n")

# ---- D1 (diagnostic): filled countries with unscaled AQUASTAT water ----
d1 <- res |>
  mutate(saving_unscaled = 100 * water_dea(res, w = res$water_unscaled, w_ref = res$water_unscaled)) |>
  filter(filled) |>
  select(geo, saving_unscaled)
print(as.data.frame(left_join(d1, select(res, geo, name, saving_pct), by = "geo")), digits = 3)

res <- res |>
  left_join(r1, by = "geo") |>
  left_join(r2, by = "geo") |>
  left_join(d1, by = "geo")

# ---- Key numbers ----
kn_reset()
top <- slice_max(res, saving_pct, n = 1, with_ties = FALSE)
kn_add("n_countries", nrow(res), "countries", 0, "Member states in the sample", src)
kn_add("n_filled", sum(res$filled), "countries", 0, "Water filled from AQUASTAT", src)
kn_add("n_frontier", sum(res$frontier), "countries", 0, "Countries on the frontier (no saving)", src)
kn_add("irr_threshold", attr(panel, "threshold"), "%", 0,
       "Minimum irrigated area, % of UAA (latest survey year)", "Eurostat ef_fsi_irri")
kn_add("fill_ratio", 100 * attr(panel, "fill_ratio"), "%", 0,
       "Median Eurostat irrigation abstraction as % of AQUASTAT agricultural withdrawal", src)
kn_add("year_start", 2020, "", 0, "First year of the averaging window", src, big_mark = "")
kn_add("year_end", 2022, "", 0, "Last year of the averaging window", src, big_mark = "")
kn_add("water_total", sum(res$water), "million m3", 0, "Irrigation water, sample total (mean 2020-2022)", src)
kn_add("saving_total", sum(res$saving_m3), "million m3", 0,
       "Water saving if every country matched the frontier", src)
kn_add("saving_share", 100 * sum(res$saving_m3) / sum(res$water), "%", 0,
       "Saving as % of the sample's irrigation water", src)
kn_add("saving_mean", mean(res$saving_pct), "%", 0, "Unweighted mean saving share", src)
kn_add("saving_median", median(res$saving_pct), "%", 0, "Median saving share", src)
kn_add("saving_top", top$saving_pct, "%", 0, paste("Largest saving share:", top$name), src)
kn_add("saving_top_m3", top$saving_m3, "million m3", 0, paste("Saving in", top$name), src)
kn_add("rho_wei", unname(sp$estimate), "", 2, "Spearman, saving share vs WEI+", src)
kn_add("p_wei", sp$p.value, "", 2, "p-value of the Spearman test", src)
kn_add("n_wei", nrow(st), "countries", 0, "Countries with WEI+", src)
kn_add("n_hi_both", sum(hi_stress & hi_saving), "countries", 0,
       "Countries at or above the median of both stress and saving share", src)
kn_add("n_disagree", n_disagree, "countries", 0,
       "Countries whose rank by output per m3 and by DEA differ by 3 or more places", src)
kn_add("rho_ppm", rho_ppm, "", 2, "Spearman, DEA saving share vs water per unit of output", src)
ppm_unit <- if (grepl("^EUR", attr(panel, "output_measure"))) "EUR/m3" else "I$/m3"
kn_add("ppm_min", min(res$out_per_m3), ppm_unit, 2, "Lowest crop output per m3 of irrigation water", src)
kn_add("ppm_max", max(res$out_per_m3), ppm_unit, 2, "Highest crop output per m3 of irrigation water", src)
kn_add("ppm_median", median(res$out_per_m3), ppm_unit, 2, "Median crop output per m3 of irrigation water", src)
kn_add("r1_n", nrow(eo), "countries", 0, "R1 sample: Eurostat irrigation data only", src)
kn_add("r1_rho", r1_rho, "", 2, "R1: Spearman of saving shares with the headline run", src)
kn_add("r2_shift", r2_shift, "pp", 0, "R2: largest change in a saving share, leave-one-out", src)
kn_add("m3_per_ha_median", median(res$m3_per_ha), "m3/ha", 0,
       "Median irrigation water per irrigated hectare", src)
kn_add("d1_max_change", max(abs(d1$saving_unscaled - res$saving_pct[match(d1$geo, res$geo)])),
       "pp", 0, "D1: largest change for a filled country with unscaled AQUASTAT water", src)

readr::write_csv(res, "output/tab_results.csv")
readr::write_csv(loo, "output/tab_robustness.csv")
saveRDS(list(res = res, sp = sp, top = top), "output/cache/results.rds")
write_session_info()
print(kn_read())
