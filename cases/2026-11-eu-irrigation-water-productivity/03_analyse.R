# 03_analyse.R — can one EU benchmark rank irrigation efficiency? ----------------------
# Reframed after two runs (2026-11-20, JPH): the case tests the assumption behind
# comparing countries on irrigation water per hectare instead of ranking countries.
#   Step 1: irrigation abstraction per irrigated hectare, Mediterranean-climate member
#           states vs the rest; headline sample = countries with Eurostat irrigation data.
#   Step 2: water-directional DEA, 2 x 2 specifications (returns to scale x land inputs),
#           how far the "saving potential" follows water per hectare and crop value per m3.
# Red-team fixes (2026-11-20): Eurostat-only headline sample, 2 x 2 specifications,
# ratio range and exact rank-sum test, leave-one-out for both headline models with counts
# of infeasible runs.
# P4 revisited (2026-11-26, JPH): step 1 is the whole post; step 2 moves to the technical
# note as a diagnostic ("why a national frontier gives no answer here"). No new models:
# added the six variant ratios, the group-separation check, the DEA rule-of-thumb sample
# sizes and crop value per m3 at the extremes as key numbers.
# Input: output/cache/panel.rds. Output: output/key_numbers.csv, output/tab_results.csv,
# output/tab_specs.csv, output/tab_robustness.csv, output/tab_variants.csv,
# output/cache/results.rds,
# output/sessionInfo.txt

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

# Mediterranean climate: member states whose farmland lies mainly in the Mediterranean
# climate zone. France is mixed and stays in "Other". (Grouping chosen after the first two
# model runs; disclosed in the post.)
med <- c("CY", "EL", "ES", "IT", "MT", "PT")

# Sub-vector DEA: contract water only, holding land and crop output fixed.
# land = "total" (cropland) or "split" (rainfed cropland + irrigated area).
water_dea <- function(d, land, rts, ref = d) {
  lx <- function(z) if (land == "total") cbind(z$cropland) else cbind(z$rainfed, z$irrigated)
  x <- cbind(lx(d), d$water)
  xr <- cbind(lx(ref), ref$water)
  dir <- cbind(matrix(0, nrow(d), ncol(x) - 1), d$water)
  e <- dea.direct(x, cbind(d$output), DIRECT = dir, RTS = rts, ORIENTATION = "in",
                  XREF = xr, YREF = cbind(ref$output))
  100 * e$objval
}
specs <- tibble::tribble(
  ~spec, ~land, ~rts, ~label,
  "A", "total", "vrs", "A: VRS, cropland + water",
  "A_crs", "total", "crs", "CRS, cropland + water",
  "B_vrs", "split", "vrs", "VRS, rainfed + irrigated land + water",
  "B", "split", "crs", "B: CRS, rainfed + irrigated land + water"
)
run_specs <- function(d) {
  for (i in seq_len(nrow(specs))) {
    d[[paste0("saving_", specs$spec[i])]] <- water_dea(d, specs$land[i], specs$rts[i])
  }
  d
}
share <- function(d, col) 100 * sum(d[[col]] / 100 * d$water) / sum(d$water)

res <- panel |>
  mutate(group = if_else(geo %in% med, "Mediterranean climate", "Other"),
         m3_per_ha = water / irrigated * 1000,           # million m3 / 1000 ha -> m3/ha
         m3_per_ha_2016 = water / irrigated_2016 * 1000,
         m3_per_ha_2023 = water / irrigated_2023 * 1000,
         out_per_m3 = output / water)                    # int. $ per m3 (million / million)
stopifnot("Water per ha outside 100-20,000 m3/ha: check units" =
            all(res$m3_per_ha > 100 & res$m3_per_ha < 20000))
main <- run_specs(filter(res, !filled))                  # headline: Eurostat irrigation data
full <- run_specs(res)                                    # context: incl. 4 AQUASTAT fills
for (d in list(main, full)) {
  v <- unlist(d[paste0("saving_", specs$spec)])
  stopifnot("DEA returned missing or negative values" = all(!is.na(v) & v > -1e-4))
}
cat("Headline sample (Eurostat irrigation data):", paste(main$name, collapse = ", "), "\n")
print(as.data.frame(full |>
                      arrange(desc(m3_per_ha)) |>
                      select(name, group, filled, m3_per_ha, out_per_m3, wei,
                             starts_with("saving_"))), digits = 3)

# ---- Step 1: water per irrigated hectare by climate group ----
gap <- function(d, col = "m3_per_ha") {
  m <- median(d[[col]][d$group == "Mediterranean climate"])
  o <- median(d[[col]][d$group == "Other"])
  c(med = m, other = o, ratio = m / o)
}
g <- gap(main)
variants <- c(main = g["ratio"], main_2016 = gap(main, "m3_per_ha_2016")["ratio"],
              main_2023 = gap(main, "m3_per_ha_2023")["ratio"], all = gap(full)["ratio"],
              all_2016 = gap(full, "m3_per_ha_2016")["ratio"],
              all_2023 = gap(full, "m3_per_ha_2023")["ratio"])
print(round(variants, 2))
names(variants) <- sub("\\.ratio$", "", names(variants))
# Separation: is every Mediterranean country above every other one?
sep_main <- min(main$m3_per_ha[main$group == "Mediterranean climate"]) >
  max(main$m3_per_ha[main$group == "Other"])
stopifnot("Text claims full separation of the groups in the headline sample" = sep_main)
tab_variants <- tibble::tibble(
  variant = names(variants),
  sample = if_else(grepl("^main", variant), "Eurostat only", "All, incl. AQUASTAT estimates"),
  irrigated_area = case_when(grepl("2016", variant) ~ "2016 survey",
                             grepl("2023", variant) ~ "2023 survey",
                             TRUE ~ "Interpolated to 2021"),
  ratio = unname(variants))
rs_main <- wilcox.test(m3_per_ha ~ group, data = main, exact = TRUE)
rs_all <- wilcox.test(m3_per_ha ~ group, data = full, exact = TRUE)
st <- filter(main, !is.na(wei))
sp_wei <- suppressWarnings(cor.test(st$m3_per_ha, st$wei, method = "spearman", exact = FALSE))
cat("m3/ha (Eurostat-only): Mediterranean median", round(g["med"]), "| other", round(g["other"]),
    "| ratio", round(g["ratio"], 1), "| range over variants", round(min(variants), 1), "-",
    round(max(variants), 1), "| exact rank-sum p", signif(rs_main$p.value, 2), "\n")

# ---- Step 2 (technical note): what the frontier picks up ----
# Rule of thumb for DEA sample size (Cooper, Seiford and Tone): n >= max(m * s, 3 * (m + s)),
# m inputs, s outputs. A: 2 inputs, B: 3 inputs, both 1 output.
rot <- function(m, s = 1) max(m * s, 3 * (m + s))
rot_a <- rot(2)
rot_b <- rot(3)
ppm <- arrange(main, desc(out_per_m3))
sh <- sapply(paste0("saving_", specs$spec), function(s) share(main, s))
sh_all <- sapply(paste0("saving_", specs$spec), function(s) share(full, s))
tab_specs <- specs |>
  mutate(saving_share_eurostat = sh, saving_share_all = sh_all,
         n_frontier_eurostat = sapply(paste0("saving_", spec), function(s) sum(main[[s]] < 1e-4)),
         frontier_eurostat = sapply(paste0("saving_", spec),
                                    function(s) paste(main$name[main[[s]] < 1e-4], collapse = ", ")))
print(as.data.frame(tab_specs), digits = 3)
rho_ab <- cor(main$saving_A, main$saving_B, method = "spearman")
rho_b_m3 <- cor(main$saving_B, main$m3_per_ha, method = "spearman")
rho_b_ppm <- cor(main$saving_B, main$out_per_m3, method = "spearman")
# Which choice drives the A-B gap? Change one dimension at a time.
d_rts <- mean(c(sh["saving_A_crs"] - sh["saving_A"], sh["saving_B"] - sh["saving_B_vrs"]))
d_land <- mean(c(sh["saving_B_vrs"] - sh["saving_A"], sh["saving_B"] - sh["saving_A_crs"]))
z0 <- function(x) if (round(x) == 0) 0 else x                 # avoid printing "-0"
d_rts <- z0(d_rts)
d_land <- z0(d_land)
cat("Average change in the saving share: returns to scale", round(d_rts), "pp | land split",
    round(d_land), "pp\n")

# Leave one frontier country out, models A and B (headline sample).
loo_one <- function(d, spec_id) {
  sp <- specs[specs$spec == spec_id, ]
  col <- paste0("saving_", spec_id)
  front <- d$geo[d[[col]] < 1e-4]
  lapply(front, function(gg) {
    tibble::tibble(spec = spec_id, geo = d$geo, dropped = gg,
                   saving = water_dea(d, sp$land, sp$rts, ref = filter(d, geo != gg)))
  }) |>
    bind_rows() |>
    filter(geo != dropped)
}
loo <- bind_rows(loo_one(main, "A"), loo_one(main, "B")) |>
  mutate(infeasible = !is.finite(saving), saving = if_else(infeasible, NA_real_, saving))
loo_rng <- loo |>
  group_by(spec, geo) |>
  summarise(loo_min = suppressWarnings(min(saving, na.rm = TRUE)),
            loo_max = suppressWarnings(max(saving, na.rm = TRUE)),
            n_infeasible = sum(infeasible), .groups = "drop") |>
  mutate(across(c(loo_min, loo_max), ~ if_else(is.finite(.x), .x, NA_real_)))
shift <- function(s) {
  r <- filter(loo_rng, spec == s)
  max(abs(r$loo_min - main[[paste0("saving_", s)]][match(r$geo, main$geo)]), na.rm = TRUE)
}
cat("Leave-one-out: largest change A", round(shift("A")), "pp | B", round(shift("B")),
    "pp | infeasible runs", sum(loo$infeasible), "of", nrow(loo), "\n")

out <- full |>
  select(-starts_with("saving_")) |>
  left_join(select(main, geo, starts_with("saving_")), by = "geo") |>
  left_join(select(full, geo, saving_B_all = saving_B), by = "geo") |>
  left_join(filter(loo_rng, spec == "B") |> select(geo, loo_min_B = loo_min, loo_max_B = loo_max),
            by = "geo") |>
  left_join(filter(loo_rng, spec == "A") |> select(geo, loo_min_A = loo_min, loo_max_A = loo_max),
            by = "geo")

# ---- Key numbers ----
kn_reset()
hi <- slice_max(main, m3_per_ha, n = 1, with_ties = FALSE)
lo <- slice_min(main, m3_per_ha, n = 1, with_ties = FALSE)
kn_add("n_main", nrow(main), "countries", 0, "Member states with Eurostat irrigation data (headline)", src)
kn_add("n_main_med", sum(main$group == "Mediterranean climate"), "countries", 0,
       paste("Mediterranean-climate, headline sample:",
             paste(main$name[main$group == "Mediterranean climate"], collapse = ", ")), src)
kn_add("n_main_other", sum(main$group == "Other"), "countries", 0, "Other, headline sample", src)
kn_add("n_all", nrow(full), "countries", 0, "All member states incl. AQUASTAT fills", src)
kn_add("n_filled", sum(full$filled), "countries", 0,
       paste("Filled from AQUASTAT (scaled):", paste(full$name[full$filled], collapse = ", ")), src)
kn_add("irr_threshold", attr(panel, "threshold"), "%", 0,
       "Minimum irrigated area, % of UAA (latest survey year)", "Eurostat ef_fsi_irri")
kn_add("fill_ratio", 100 * attr(panel, "fill_ratio"), "%", 0,
       "Median Eurostat irrigation abstraction as % of AQUASTAT agricultural withdrawal", src)
kn_add("year_start", 2020, "", 0, "First year of the averaging window", src, big_mark = "")
kn_add("year_end", 2022, "", 0, "Last year of the averaging window", src, big_mark = "")
kn_add("m3ha_med", g["med"], "m3/ha", 0, "Median abstraction per irrigated ha, Mediterranean (headline)", src)
kn_add("m3ha_other", g["other"], "m3/ha", 0, "Median abstraction per irrigated ha, other (headline)", src)
kn_add("m3ha_ratio", g["ratio"], "x", 1, "Mediterranean median / other median (headline)", src)
kn_add("ratio_min", min(variants), "x", 1, "Lowest ratio over samples and survey years", src)
kn_add("ratio_max", max(variants), "x", 1, "Highest ratio over samples and survey years", src)
for (v in names(variants)) {
  kn_add(paste0("ratio_", v), variants[[v]], "x", 1,
         paste("Ratio of medians, variant", v, "(sample _ irrigated-area year)"), src)
}
kn_add("p_ranksum", rs_main$p.value, "", 3, "Exact Wilcoxon rank-sum p, headline sample", src)
kn_add("p_ranksum_all", rs_all$p.value, "", 4, "Exact Wilcoxon rank-sum p, all 14", src)
kn_add("m3ha_max", hi$m3_per_ha, "m3/ha", 0, paste("Highest, headline sample:", hi$name), src)
kn_add("m3ha_min", lo$m3_per_ha, "m3/ha", 0, paste("Lowest, headline sample:", lo$name), src)
kn_add("rho_m3ha_wei", unname(sp_wei$estimate), "", 2,
       "Spearman, water per ha vs WEI+ (partly mechanical: shared numerator)", src)
for (i in seq_len(nrow(specs))) {
  kn_add(paste0("share_", tolower(specs$spec[i])), sh[i], "%", 0,
         paste0("Saving share of sample water, ", specs$label[i], " (headline sample)"), src)
  kn_add(paste0("nfront_", tolower(specs$spec[i])), tab_specs$n_frontier_eurostat[i], "countries", 0,
         paste0("Frontier, ", specs$label[i], ": ", tab_specs$frontier_eurostat[i]), src)
}
kn_add("rot_a", rot_a, "countries", 0, "DEA rule of thumb, model A (2 inputs, 1 output)",
       "Cooper, Seiford and Tone (2007), Data Envelopment Analysis, 2nd ed.")
kn_add("rot_b", rot_b, "countries", 0, "DEA rule of thumb, model B (3 inputs, 1 output)",
       "Cooper, Seiford and Tone (2007), Data Envelopment Analysis, 2nd ed.")
kn_add("ppm_max", ppm$out_per_m3[1], "int. $/m3", 1,
       paste("Highest crop value per m3 irrigation water, headline sample:", ppm$name[1]), src)
kn_add("ppm_min", ppm$out_per_m3[nrow(ppm)], "int. $/m3", 1,
       paste("Lowest crop value per m3 irrigation water, headline sample:", ppm$name[nrow(ppm)]), src)
kn_add("share_b_all", sh_all["saving_B"], "%", 0, "Model B saving share, all 14", src)
kn_add("share_a_all", sh_all["saving_A"], "%", 0, "Model A saving share, all 14", src)
kn_add("d_rts", d_rts, "pp", 0, "Average change in saving share from VRS to CRS", src)
kn_add("d_land", d_land, "pp", 0, "Average change in saving share from total to split land", src)
kn_add("rho_ab", rho_ab, "", 2, "Spearman, saving shares A vs B", src)
kn_add("rho_b_m3ha", rho_b_m3, "", 2, "Spearman, B saving share vs water per ha", src)
kn_add("rho_b_ppm", rho_b_ppm, "", 3, "Spearman, B saving share vs crop value per m3", src)
kn_add("loo_shift_a", shift("A"), "pp", 0, "Model A: largest change in a saving share, leave-one-out", src)
kn_add("loo_shift_b", shift("B"), "pp", 0, "Model B: largest change in a saving share, leave-one-out", src)
kn_add("loo_infeasible", sum(loo$infeasible), "runs", 0, "Leave-one-out runs with no feasible peer", src)
# Reference values quoted in the post (not computed here).
kn_add("ref_wrs_target", 10, "%", 0, "EU Water Resilience Strategy: water-efficiency improvement target by 2030",
       "European Commission (2025), European Water Resilience Strategy")
kn_add("ref_fr_m3ha", 1902, "m3/ha", 0, "France, irrigation abstraction per irrigated ha, 2020",
       "INSEE (2024), https://www.insee.fr/fr/statistiques/7728881")

readr::write_csv(out, "output/tab_results.csv")
readr::write_csv(tab_specs, "output/tab_specs.csv")
readr::write_csv(loo, "output/tab_robustness.csv")
readr::write_csv(tab_variants, "output/tab_variants.csv")
saveRDS(list(res = out, main = main, specs = tab_specs, variants = tab_variants),
        "output/cache/results.rds")
write_session_info()
print(kn_read(), n = 60)
