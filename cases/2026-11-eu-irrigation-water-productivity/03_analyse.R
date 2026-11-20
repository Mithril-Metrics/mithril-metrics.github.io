# 03_analyse.R — can one EU benchmark rank irrigation efficiency? ----------------------
# Reframed after two runs (2026-11-20, JPH): the case now tests the assumption behind an
# EU-wide water-efficiency benchmark instead of ranking countries.
#   Step 1: water per irrigated hectare, Mediterranean-climate member states vs the rest.
#   Step 2: water-directional DEA under two reasonable specifications
#           A = VRS, inputs cropland + water (the planned model, first run)
#           B = CRS, inputs rainfed cropland + irrigated area + water (second run)
#           and how far the "saving potential" follows water per hectare.
# Robustness: R1 Eurostat-only countries; R2 irrigated area from the 2016 or 2023 survey.
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
src <- "Eurostat env_wat_abs, ef_fsi_irri, sdg_06_60; FAOSTAT QV, RL; AQUASTAT via World Bank"

# Mediterranean climate, fixed before looking at water use: member states whose farmland
# lies mainly in the Mediterranean climate zone. France is mixed and stays in "other".
med <- c("CY", "EL", "ES", "IT", "MT", "PT")

# Sub-vector DEA: contract water only, holding land and crop output fixed.
# beta = share of water a country could save while staying inside the frontier.
water_dea <- function(d, spec, ref = d) {
  land <- function(z) if (spec == "A") cbind(z$cropland) else cbind(z$rainfed, z$irrigated)
  x <- cbind(land(d), d$water)
  xr <- cbind(land(ref), ref$water)
  dir <- cbind(matrix(0, nrow(d), ncol(x) - 1), d$water)
  e <- dea.direct(x, cbind(d$output), DIRECT = dir, RTS = if (spec == "A") "vrs" else "crs",
                  ORIENTATION = "in", XREF = xr, YREF = cbind(ref$output))
  e$objval
}

res <- panel |>
  mutate(group = if_else(geo %in% med, "Mediterranean climate", "Other"),
         m3_per_ha = water / irrigated * 1000,           # million m3 / 1000 ha -> m3/ha
         out_per_m3 = output / water,                    # int. $ per m3 (million / million)
         saving_a = 100 * water_dea(panel, "A"),
         saving_b = 100 * water_dea(panel, "B"),
         frontier_a = saving_a < 1e-4,
         frontier_b = saving_b < 1e-4)
stopifnot("DEA returned missing or negative values" =
            all(!is.na(c(res$saving_a, res$saving_b)) & c(res$saving_a, res$saving_b) > -1e-4))
print(as.data.frame(res |>
                      arrange(desc(m3_per_ha)) |>
                      select(name, group, filled, water, irrigated, m3_per_ha, out_per_m3, wei,
                             saving_a, saving_b)), digits = 3)

# ---- Step 1: water per irrigated hectare by climate group ----
gap <- function(d, col = "m3_per_ha") {
  m <- median(d[[col]][d$group == "Mediterranean climate"])
  o <- median(d[[col]][d$group == "Other"])
  c(med = m, other = o, ratio = m / o)
}
g <- gap(res)
st <- filter(res, !is.na(wei))
sp_wei <- suppressWarnings(cor.test(st$m3_per_ha, st$wei, method = "spearman", exact = FALSE))
cat("m3/ha: Mediterranean median", round(g["med"]), "| other", round(g["other"]),
    "| ratio", round(g["ratio"], 1), "| Spearman with WEI+", round(sp_wei$estimate, 2), "\n")

# ---- Step 2: what the frontier picks up ----
share <- function(s) 100 * sum(s / 100 * res$water) / sum(res$water)
rho_ab <- cor(res$saving_a, res$saving_b, method = "spearman")
rho_b_m3 <- cor(res$saving_b, res$m3_per_ha, method = "spearman")
rho_a_m3 <- cor(res$saving_a, res$m3_per_ha, method = "spearman")
cat("Saving share of sample water: A", round(share(res$saving_a)), "% | B",
    round(share(res$saving_b)), "% | Spearman A vs B", round(rho_ab, 2),
    "| B vs m3/ha", round(rho_b_m3, 2), "\n")
cat("Frontier A:", paste(res$name[res$frontier_a], collapse = ", "),
    "| Frontier B:", paste(res$name[res$frontier_b], collapse = ", "), "\n")

# Leave one frontier country out (spec B): how stable are single shares?
front_b <- res$geo[res$frontier_b]
loo <- lapply(front_b, function(gg) {
  tibble::tibble(geo = res$geo, dropped = gg,
                 saving = 100 * water_dea(res, "B", ref = filter(res, geo != gg)))
}) |>
  bind_rows() |>
  filter(geo != dropped) |>
  mutate(saving = if_else(is.finite(saving), saving, NA_real_))
loo_rng <- loo |>
  group_by(geo) |>
  summarise(loo_min = min(saving, na.rm = TRUE), loo_max = max(saving, na.rm = TRUE),
            .groups = "drop")
loo_shift <- max(abs(loo_rng$loo_min - res$saving_b[match(loo_rng$geo, res$geo)]))
cat("Spec B leave-one-out over", length(front_b), "frontier countries | largest change",
    round(loo_shift), "pp\n")

# ---- R1: Eurostat-only countries ----
g_r1 <- gap(filter(res, !filled))
# ---- R2: irrigated area from one survey instead of the 2021 interpolation ----
g_16 <- gap(mutate(res, m3 = water / irrigated_2016 * 1000), "m3")
g_23 <- gap(mutate(res, m3 = water / irrigated_2023 * 1000), "m3")
cat("Med/other ratio: R1 Eurostat-only", round(g_r1["ratio"], 1), "| R2 2016 survey",
    round(g_16["ratio"], 1), "| 2023 survey", round(g_23["ratio"], 1), "\n")

res <- left_join(res, loo_rng, by = "geo")

# ---- Key numbers ----
kn_reset()
hi <- slice_max(res, m3_per_ha, n = 1, with_ties = FALSE)
lo <- slice_min(res, m3_per_ha, n = 1, with_ties = FALSE)
kn_add("n_countries", nrow(res), "countries", 0, "Member states in the sample", src)
kn_add("n_med", sum(res$group == "Mediterranean climate"), "countries", 0,
       "Mediterranean-climate member states in the sample", src)
kn_add("n_filled", sum(res$filled), "countries", 0, "Water filled from AQUASTAT (scaled)", src)
kn_add("irr_threshold", attr(panel, "threshold"), "%", 0,
       "Minimum irrigated area, % of UAA (latest survey year)", "Eurostat ef_fsi_irri")
kn_add("fill_ratio", 100 * attr(panel, "fill_ratio"), "%", 0,
       "Median Eurostat irrigation abstraction as % of AQUASTAT agricultural withdrawal", src)
kn_add("year_start", 2020, "", 0, "First year of the averaging window", src, big_mark = "")
kn_add("year_end", 2022, "", 0, "Last year of the averaging window", src, big_mark = "")
kn_add("m3ha_med", g["med"], "m3/ha", 0, "Median water per irrigated ha, Mediterranean climate", src)
kn_add("m3ha_other", g["other"], "m3/ha", 0, "Median water per irrigated ha, other member states", src)
kn_add("m3ha_ratio", g["ratio"], "x", 1, "Mediterranean median / other median", src)
kn_add("m3ha_max", hi$m3_per_ha, "m3/ha", 0, paste("Highest water per irrigated ha:", hi$name), src)
kn_add("m3ha_min", lo$m3_per_ha, "m3/ha", 0, paste("Lowest water per irrigated ha:", lo$name), src)
kn_add("rho_m3ha_wei", unname(sp_wei$estimate), "", 2, "Spearman, water per ha vs WEI+", src)
kn_add("p_m3ha_wei", sp_wei$p.value, "", 3, "p-value of that Spearman test", src)
kn_add("saving_share_a", share(res$saving_a), "%", 0,
       "Spec A (VRS, cropland + water): saving as % of sample water", src)
kn_add("saving_share_b", share(res$saving_b), "%", 0,
       "Spec B (CRS, rainfed + irrigated land + water): saving as % of sample water", src)
kn_add("n_frontier_a", sum(res$frontier_a), "countries", 0,
       paste("Spec A frontier:", paste(res$name[res$frontier_a], collapse = ", ")), src)
kn_add("n_frontier_b", sum(res$frontier_b), "countries", 0,
       paste("Spec B frontier:", paste(res$name[res$frontier_b], collapse = ", ")), src)
kn_add("rho_ab", rho_ab, "", 2, "Spearman, saving shares spec A vs spec B", src)
kn_add("rho_b_m3ha", rho_b_m3, "", 2, "Spearman, spec B saving share vs water per ha", src)
kn_add("rho_a_m3ha", rho_a_m3, "", 2, "Spearman, spec A saving share vs water per ha", src)
kn_add("loo_shift_b", loo_shift, "pp", 0, "Spec B: largest change in a saving share, leave-one-out", src)
kn_add("r1_ratio", g_r1["ratio"], "x", 1, "R1: Med/other ratio, Eurostat-only countries", src)
kn_add("r2_ratio_2016", g_16["ratio"], "x", 1, "R2: Med/other ratio with 2016 irrigated area", src)
kn_add("r2_ratio_2023", g_23["ratio"], "x", 1, "R2: Med/other ratio with 2023 irrigated area", src)

readr::write_csv(res, "output/tab_results.csv")
readr::write_csv(loo, "output/tab_robustness.csv")
saveRDS(list(res = res), "output/cache/results.rds")
write_session_info()
print(kn_read())
