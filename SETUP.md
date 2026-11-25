# How this repository works

Each case is a reproducible analysis built from public data. The R scripts run on the
author's own computer; the blog and the PDF briefs are rendered by GitHub Actions on every push.

Blog: <https://mithril-metrics.github.io/>

## Reproduce a case

One-time setup in R (no other software needed):

```r
install.packages(c("remotes", "tidyverse", "yaml", "fixest", "modelsummary", "svglite",
                   "eurostat", "WDI", "countrycode"))
remotes::install_github("Mithril-Metrics/mithril-metrics.github.io", subdir = "acutils")
```

Method packages (`sfaR`, `frontier`, `Benchmarking`, `DoubleML`, ...) are installed only when a
case needs them; each case's `01_get_data.R` lists what it uses.

Then copy a case folder, e.g. `cases/2026-11-elnino-rice-tails/`, and run it from that folder:

```r
setwd("path/to/the/case/folder")
source("run_all.R")   # runs 01 -> 04 in order, logs the console to output/run_log.txt
```

Downloads go only into the case's `data_raw/` folder (typically 5-50 MB) and are never committed.
Delete it whenever you like; `01_get_data.R` downloads the data again. Every file name carries its
access date, and `data_raw/_access_log.csv` records source, URL and version.

Keys (only for ERA5 climate data): `ecmwfr::wf_set_key(key = "<CDS personal access token>")`
stores the token in the system keyring; it never goes into the repository.

## Case folders

Cases live in `cases/YYYY-MM-slug/`. Authors keep a local copy in `AgEcon/Issue N/`, where N is the
issue number in the portfolio log; `00_meta.yml` records the mapping (`issue`, `local_folder`).
A new case starts from the template:

```sh
Rscript new_case.R <slug> --issue N --title "..."
```

## Publishing

New posts start with `draft: true` in `report.qmd` (rendered but not listed, not in RSS) and are
set to `draft: false` once the case has been reviewed and approved. The push triggers the workflow
and the post is live a few minutes later. The LinkedIn text is rendered into
`cases/<case>/output/linkedin_post.txt` during the build.

## Maintainer setup on GitHub (once)

1. **Settings > Actions > General > Workflow permissions:** *Read and write permissions*.
2. **Settings > Pages > Build and deployment > Source:** *Deploy from a branch*, branch `gh-pages`,
   folder `/ (root)`.
3. **Actions > Publish blog > Run workflow** (or push). The first run takes about 5 minutes; later
   runs are faster because packages are cached.
