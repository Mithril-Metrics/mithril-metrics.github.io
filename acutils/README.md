# acutils

Helpers for the AgEcon Practical Experience case studies.

## Install

```r
# from the repository root
install.packages(c("devtools", "eurostat", "WDI", "countrycode", "ecmwfr", "terra", "svglite"))
devtools::document("acutils")
devtools::install_local("acutils", upgrade = "never")
# or, once pushed: renv::install("PeterSala94/agecon-cases/acutils")
devtools::test("acutils")   # offline unit tests
```

## Functions

| Function | What it does | Source (verified 2026-11-18) |
|---|---|---|
| `get_faostat(domain, items, areas, years, elements)` | Whole domain via bulk ZIP, cached, filtered; ISO3 → FAO codes | `bulks-faostat.fao.org/production/datasets_E.json` |
| `faostat_catalog()` | Bulk catalogue with `date_update` per domain | same |
| `get_eurostat(code, filters)` | `eurostat::get_eurostat()` + cache + `<dim>_label` columns | eurostat ≥ 4.0 (dissemination API) |
| `get_wdi(indicators, countries, years)` | `WDI::WDI()` + cache | api.worldbank.org/v2 |
| `get_usda_tfp()` | ERS International Agricultural Productivity, long CSV, tidy | 1961–2023 release, updated 2026-01-27 |
| `get_pinksheet()` | World Bank CMO monthly prices, long format | November 2026 release (2026-11-03) |
| `get_era5_monthly(var, bbox, years)` | CDS request template via `ecmwfr` ≥ 2.0 (PAT) | `reanalysis-era5-single-levels-monthly-means` |
| `check_panel(df, id, time)` | N, T, balance, gaps, duplicates, NA shares | — |
| `theme_ac()`, `scale_*_ac*()`, `source_caption()` | Phone-readable theme, CVD-checked palette, source line | — |
| `save_fig(plot, name)` | PNG 1600×1000 + SVG to `output/` | — |
| `kn_add()`, `kn()`, `kn_read()`, `kn_reset()` | `output/key_numbers.csv` store for text/figures/posts | — |
| `access_log()`, `write_session_info()` | Access dates for `00_meta.yml`; `sessionInfo()` to `output/` | — |

All fetchers cache to `getOption("acutils.data_dir", "data_raw")` as `<name>_<YYYY-MM-DD>.<ext>`
and log the download in `_access_log.csv`. Use `refresh = TRUE` for a new download.

When a provider moves a file (USDA and the World Bank change the path with each release), set
`options(acutils.usda_tfp_url = "...")` or `options(acutils.pinksheet_url = "...")`.
