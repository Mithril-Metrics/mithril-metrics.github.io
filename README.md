# agecon-cases

Short, reproducible, data-driven policy case studies in agricultural, production and
applied microeconomics — the code behind the **AgEcon Practical Experience** series
by Pietro Sala (TUM, Chair of Agricultural Production and Resource Economics).

> I quantify how farms and food systems turn inputs and natural resources (land, water,
> nutrients, climate) into outputs, and how policy, prices and weather shocks shift that
> relationship, using public data and reproducible code.

Each case answers one narrow policy question with public data in about four hours,
end to end. Blog: <https://petersala94.github.io/agecon-cases/>

## Repository layout

```
agecon-cases/
├── acutils/                 # helper R package (fetchers, panel check, theme, export, key numbers)
├── cases/
│   ├── _template/           # copied by new_case.R (ignored by the website because of the "_")
│   └── YYYY-MM-slug/
│       ├── 00_meta.yml      # question, hook, sources, access dates, manual steps
│       ├── 01_get_data.R    # fetch + cache into data_raw/ (git-ignored)
│       ├── 02_clean.R       # tidy, check_panel(), save to output/cache/
│       ├── 03_analyse.R     # estimation, writes output/key_numbers.csv
│       ├── 04_figures.R     # ≤ 3 figures (PNG 1600×1000 + SVG)
│       ├── report.qmd       # blog post (HTML) + one-page brief (PDF) + LinkedIn text
│       ├── data_raw/        # git-ignored, recreated by 01_get_data.R
│       └── output/          # figures, key_numbers.csv, sessionInfo (committed); cache/ ignored
├── tools/
│   ├── feasibility_check.R  # 2-minute GREEN/AMBER/RED data check from a YAML spec
│   └── specs/               # example specs
├── _quarto.yml, index.qmd, about.qmd, styles.scss   # Quarto blog (GitHub Pages)
├── new_case.R               # scaffold a new case from the template
├── Makefile                 # shortcuts (make new / run / site / check / test)
└── README.md
```

## How to reproduce a case

Requirements: R ≥ 4.3 and the packages listed in [SETUP.md](SETUP.md). Nothing else is
needed to run the analysis; the blog is rendered by GitHub Actions.

```r
remotes::install_github("PeterSala94/agecon-cases", subdir = "acutils")
setwd("cases/2026-11-slug")      # the case folder
for (s in c("01_get_data.R", "02_clean.R", "03_analyse.R", "04_figures.R")) source(s, echo = TRUE)
```

Raw data are never committed: `01_get_data.R` downloads them again and stores them in
`data_raw/` with the access date in the file name. Any manual step (rare) is documented in
the case's `00_meta.yml` under `manual_steps`. Numbers in the text, figures and LinkedIn
post all come from `output/key_numbers.csv`, so they cannot disagree.

Some sources need a free key: ERA5 (Copernicus CDS personal access token, set with
`ecmwfr::wf_set_key()`), UN Comtrade. Keys are never stored in the repository.

Tooling for a full local setup (optional): `new_case.R` scaffolds a case from
`cases/_template/`, `tools/feasibility_check.R` runs a 2-minute data check from a YAML spec,
and the `Makefile` wraps both plus `quarto render`.

## Licence

- **Code** (R scripts, `acutils`, tooling): [MIT](LICENSE).
- **Text, figures and briefs** (blog posts, `report.qmd` prose, PNG/SVG/PDF outputs):
  [CC BY 4.0](LICENSE-CONTENT.md). Please cite as: Sala, P. (year). *Title*. AgEcon Practical Experience.
- **Data** remain under their providers' licences (e.g. FAOSTAT CC BY-NC-SA 3.0 IGO, World
  Bank CC BY 4.0, Eurostat reuse policy). Check each case's `00_meta.yml`.

Views are my own and do not represent TUM or any project consortium.
