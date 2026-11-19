#!/usr/bin/env Rscript
# new_case.R — scaffold a new case from cases/_template/
#
# Usage (from the repository root):
#   Rscript new_case.R <slug> --issue N [--title "..."] [--region EU] [--method SFA] [--employer OECD]
#                             [--date 2026-11]
# Example:
#   Rscript new_case.R fertiliser-squeeze --title "How much did the fertiliser shock cost EU cereal farms?" \
#     --region EU --method SFA --employer OECD
#
# Creates cases/YYYY-MM-<slug>/ and fills the {{PLACEHOLDERS}} in every text file
# (00_meta.yml, R stubs, report.qmd front matter and categories).

args <- commandArgs(trailingOnly = TRUE)
usage <- 'Usage: Rscript new_case.R <slug> --issue N [--title "..."] [--region R] [--method M] [--employer E] [--date YYYY-MM]'
if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
  cat(usage, "\n")
  quit(status = if (length(args) == 0) 1 else 0)
}

slug <- tolower(args[1])
if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", slug)) {
  stop("Slug must be lowercase letters/digits separated by hyphens, e.g. 'fertiliser-squeeze'.")
}

# --- Parse --key value pairs ---------------------------------------------------
opts <- list(issue = NULL, title = NULL, region = "Global", method = "Descriptive",
             employer = "WB", date = format(Sys.Date(), "%Y-%m"))
rest <- args[-1]
if (length(rest) %% 2 != 0) stop("Options must come in --key value pairs.\n", usage)
for (i in seq(1, length(rest), by = 2)) {
  key <- sub("^--", "", rest[i])
  if (!key %in% names(opts)) stop("Unknown option --", key, "\n", usage)
  opts[[key]] <- rest[i + 1]
}
if (!grepl("^\\d{4}-\\d{2}$", opts$date)) stop("--date must be YYYY-MM.")
if (is.null(opts$issue) || !grepl("^[0-9]+$", opts$issue)) stop("--issue N is required (case number in the portfolio log).")
if (is.null(opts$title)) opts$title <- tools::toTitleCase(gsub("-", " ", slug))

case_id  <- paste0(opts$date, "-", slug)
template <- file.path("cases", "_template")
target   <- file.path("cases", case_id)

if (!dir.exists(template)) stop("Run from the repository root (cases/_template/ not found).")
if (dir.exists(target)) stop("Case already exists: ", target)

# --- Copy the template ------------------------------------------------------------
dir.create(target, recursive = TRUE)
files <- list.files(template, recursive = TRUE, all.files = TRUE, no.. = TRUE)
for (f in files) {
  dest <- file.path(target, f)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(template, f), dest)
}
dir.create(file.path(target, "data_raw"), showWarnings = FALSE)
dir.create(file.path(target, "output", "cache"), recursive = TRUE, showWarnings = FALSE)

# --- Fill placeholders --------------------------------------------------------------
yaml_escape <- function(x) gsub('"', '\\\\"', x)
subs <- c(
  "{{CASE_ID}}"  = case_id,
  "{{SLUG}}"     = slug,
  "{{ISSUE}}"    = opts$issue,
  "{{TITLE}}"    = yaml_escape(opts$title),
  "{{REGION}}"   = opts$region,
  "{{METHOD}}"   = opts$method,
  "{{EMPLOYER}}" = opts$employer,
  "{{DATE}}"     = format(Sys.Date(), "%Y-%m-%d")
)
text_ext <- "\\.(R|yml|yaml|qmd|md|csv|txt)$"
for (f in list.files(target, pattern = text_ext, recursive = TRUE, full.names = TRUE)) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  for (p in names(subs)) x <- gsub(p, subs[[p]], x, fixed = TRUE)
  writeLines(x, f, useBytes = TRUE)
}

cat("Created ", target, "\n\n",
    "Next steps:\n",
    "  1. Edit ", file.path(target, "00_meta.yml"), " (hook, sources).\n",
    "  2. Check data:  Rscript tools/feasibility_check.R <spec.yml>\n",
    "  3. Run:         make run CASE=", target, "\n", sep = "")
