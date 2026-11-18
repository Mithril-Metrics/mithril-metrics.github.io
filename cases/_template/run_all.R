# run_all.R — run the whole case in one go ------------------------------------------
# Case: {{CASE_ID}}  (local folder: AgEcon/Issue {{ISSUE}}/)
# Usage, in R or RStudio:
#   setwd("C:/.../AgEcon/Issue {{ISSUE}}")   # or Session > Set Working Directory > To Source File Location
#   source("run_all.R")
# Runs 01 -> 04 in order, writes the full console log to output/run_log.txt.
# Downloads go only to data_raw/ inside this folder. Upload the CONTENTS of output/ to the chat.

stopifnot("Set the working directory to the issue folder first" = file.exists("00_meta.yml"))
dir.create("output", showWarnings = FALSE)
dir.create("data_raw", showWarnings = FALSE)

run_case <- function() {
  log_file <- file.path("output", "run_log.txt")
  con <- file(log_file, open = "wt")
  sink(con, split = TRUE)                 # console output -> screen + log
  sink(con, type = "message")             # messages and warnings -> log
  on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)  # inside a function: runs at the end

  cat("Run started:", format(Sys.time()), "\nWorking directory:", getwd(), "\n\n")
  for (s in c("01_get_data.R", "02_clean.R", "03_analyse.R", "04_figures.R")) {
    cat("\n==================== ", s, " ====================\n", sep = "")
    t0 <- Sys.time()
    source(s, echo = FALSE, local = new.env())
    cat(sprintf("-- %s finished in %.1f s\n", s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  cat("\nDone:", format(Sys.time()), "\n")
  up <- list.files("output", recursive = TRUE)
  up <- up[!grepl("^cache/", up)]
  cat("Upload these files from output/ to the chat (not the cache/ folder):\n")
  print(up)
  cat(sprintf("data_raw/ uses %.1f MB; delete it after uploading if you like.\n",
              sum(file.size(list.files("data_raw", full.names = TRUE, recursive = TRUE))) / 1e6))
}
run_case()
