Issue {{ISSUE}} — {{TITLE}}
Case id (GitHub): cases/{{CASE_ID}}/

1. Unzip this archive into AgEcon/Issue {{ISSUE}}/ (the files sit directly in that folder).
2. Open R or RStudio and set the working directory to that folder:
     setwd("C:/.../AgEcon/Issue {{ISSUE}}")
3. Run:
     source("run_all.R")
4. Upload the contents of output/ to the chat (figures, key_numbers.csv, tab_*, sessionInfo.txt, run_log.txt).
   Do NOT upload data_raw/.
5. Optional: delete data_raw/ afterwards. 01_get_data.R downloads it again if needed.

If a script stops with an error, upload output/run_log.txt (or paste the error).
One-time setup: remotes::install_github("Mithril-Metrics/mithril-metrics.github.io", subdir = "acutils")
