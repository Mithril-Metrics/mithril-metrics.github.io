# How this repository is used

The workflow runs online. Your laptop only runs the analysis scripts in your own R.

| Step | Where | Who |
|---|---|---|
| Scan, shortlist, feasibility, plan, write the scripts | Claude (project "AgEcon Practical Experience") | Claude |
| Commit the case folder (`cases/YYYY-MM-slug/`) to this repository | GitHub | Claude |
| Run `01_get_data.R` → `04_figures.R` | Your R / RStudio | You |
| Upload `output/` (figures, `key_numbers.csv`, `sessionInfo.txt`) or paste errors | Claude | You |
| Red-team, write the blog post, LinkedIn post and brief in `report.qmd` | Claude | Claude |
| Commit outputs + report, update the portfolio log | GitHub | Claude |
| Render the blog and PDF briefs, publish to GitHub Pages | GitHub Actions (`.github/workflows/publish.yml`) | Automatic on every push |

Blog: <https://petersala94.github.io/agecon-cases/>

## One-time setup on GitHub (2 minutes)

1. **Settings → Actions → General → Workflow permissions →** select *Read and write permissions* → Save.
2. **Settings → Pages → Build and deployment → Source:** *Deploy from a branch*; **Branch:** `gh-pages`, folder `/ (root)` → Save.
3. **Actions** tab → *Publish blog* → *Run workflow* (or wait for the next push). The first run
   takes about 5 minutes; later runs are faster because packages are cached.
4. Optional: upload your CV as `files/Sala_Pietro_CV.pdf` (*Add file → Upload files* on
   GitHub), which the About page links to.

## One-time setup in your local R (no other software needed)

```r
install.packages(c("remotes", "tidyverse", "yaml", "fixest", "modelsummary", "svglite",
                   "eurostat", "WDI", "countrycode"))
remotes::install_github("PeterSala94/agecon-cases", subdir = "acutils")
```

Method packages (`sfaR`, `frontier`, `Benchmarking`, `DoubleML`, …) are installed only when a
case needs them; each case's `01_get_data.R` lists what it uses.

## Running a case locally

1. Download the case folder from GitHub. The simplest way is *Code → Download ZIP* and then
   keep only `cases/YYYY-MM-slug/`. Alternatively, Claude sends the scripts directly in the chat.
2. In RStudio: *Session → Set Working Directory → To Source File Location* (the case folder),
   then run the scripts in order, or in the R console:

```r
setwd("path/to/cases/2026-11-slug")
for (s in c("01_get_data.R", "02_clean.R", "03_analyse.R", "04_figures.R")) source(s, echo = TRUE)
```

3. Upload the contents of `output/` (not `data_raw/`) to the chat. `data_raw/` holds the
   downloaded data (usually a few MB). You can delete it after each case, because
   `01_get_data.R` downloads it again when needed.

Keys (only for ERA5 climate data): `ecmwfr::wf_set_key(key = "<CDS personal access token>")`
stores the token in your system keyring; it never goes into the repository.

## Publishing

New posts start with `draft: true` in `report.qmd` (rendered but not listed, not in RSS).
Claude sets `draft: false` once the case is red-teamed and you approve; the push triggers the
workflow and the post is live a few minutes later. The LinkedIn text is rendered into
`cases/<case>/output/linkedin_post.txt` during the build; Claude also gives it to you in the chat.
