Issue 7 - When Indonesia freed packaged cooking oil from its price cap in March 2022, how far did it drift from capped bulk oil, and did bulk oil stay at its cap?
Case id (GitHub): cases/2026-11-indonesia-cooking-oil-bulk-packaged/

1. Unzip this archive into AgEcon/Issue 7/ (the files sit directly in that folder).
   Your current "Issue 7" folder holds Issue 6: rename it to "Issue 6" first.
2. Open R or RStudio and set the working directory to that folder:
     setwd("C:/.../AgEcon/Issue 7")
3. Run:
     source("run_all.R")
   01_get_data.R makes six small requests to Bank Indonesia's PIHPS site (about 20 seconds).
4. Upload the contents of output/ to the chat (figures, key_numbers.csv, tab_*, sessionInfo.txt, run_log.txt).
   Do NOT upload data_raw/.
5. Optional: delete data_raw/ afterwards. 01_get_data.R downloads it again if needed.

If a script stops with an error, upload output/run_log.txt (or paste the error).
One-time setup: remotes::install_github("Mithril-Metrics/mithril-metrics.github.io", subdir = "acutils")
New package for this case: clubSandwich (installed automatically by 01_get_data.R).
