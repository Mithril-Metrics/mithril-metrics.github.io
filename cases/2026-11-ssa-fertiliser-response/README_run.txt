Issue 2 — Expensive fertiliser, smaller maize harvests? Sub-Saharan Africa's response to price spikes, and the role of input subsidies
Case id (GitHub): cases/2026-11-ssa-fertiliser-response/

1. Unzip this archive into AgEcon/Issue 2/ (the files sit directly in that folder).
2. Open R or RStudio and set the working directory to that folder:
     setwd("C:/.../AgEcon/Issue 2")
3. Run:
     source("run_all.R")
   The first run downloads about 45 MB (FAOSTAT maize file ~34 MB, nitrogen, Pink Sheet,
   MAFAP, ONI) and installs fixest, readxl or svglite if missing. Expect 3-6 minutes.
4. Upload the contents of output/ to the chat (figures, key_numbers.csv, tab_*, sessionInfo.txt, run_log.txt).
   Do NOT upload data_raw/.
5. Optional: delete data_raw/ afterwards. 01_get_data.R downloads it again if needed.

If a script stops with an error, upload output/run_log.txt (or paste the error).
One-time setup: remotes::install_github("Mithril-Metrics/mithril-metrics.github.io", subdir = "acutils")
