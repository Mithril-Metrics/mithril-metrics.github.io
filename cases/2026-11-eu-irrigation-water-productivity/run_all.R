# run_all.R — run the whole case in one go ------------------------------------------
# Case: 2026-11-eu-irrigation-water-productivity  (local folder: AgEcon/Issue 3/)
# Usage, in R or RStudio:
#   setwd("C:/.../AgEcon/Issue 3")   # or Session > Set Working Directory > To Source File Location
#   source("run_all.R")
# Runs 01 -> 04 in order, writes the full console log to output/run_log.txt.
# Downloads go only to data_raw/ inside this folder. Upload the CONTENTS of output/ to the chat.

stopifnot("Set the working directory to the issue folder first" = file.exists("00_meta.yml"))
dir.create("output", showWarnings = FALSE)
dir.create("data_raw", showWarnings = FALSE)

# Keep the log readable: no progress bars or colour codes (they flood run_log.txt)
options(readr.show_progress = FALSE, readr.show_col_types = FALSE, acutils.quiet = TRUE,
        cli.num_colors = 1, cli.dynamic = FALSE, crayon.enabled = FALSE)

run_case <- function() {
  log_file <- file.path("output", "run_log.txt")
  con <- file(log_file, open = "wt")
  sink(con, split = TRUE)                 # console output -> screen + log
  sink(con, type = "message")             # messages and warnings -> log
  # inside a function, so this runs when the run ends
  on.exit({
    sink(type = "message")
    sink()
    close(con)
  }, add = TRUE)

  cat("Run started:", format(Sys.time()), "\nWorking directory:", getwd(), "\n\n")
  # every NN_*.R / NNx_*.R script in order (01, 02, 03, 03b, 04, ...)
  scripts <- sort(list.files(".", pattern = "^[0-9]{2}[a-z]?_.*\\.R$"))
  for (s in scripts) {
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
