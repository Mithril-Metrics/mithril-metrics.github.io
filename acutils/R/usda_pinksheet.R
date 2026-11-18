# USDA ERS International Agricultural Productivity and World Bank Pink Sheet -----

# Verified 2026-11-18 on https://www.ers.usda.gov/data-products/international-agricultural-productivity
# (data 1961-2023, "Last updated 1/27/2026"). The ?v= suffix and the /media/<id>/ path
# change with each release: override with options(acutils.usda_tfp_url = "...").
.usda_tfp_url <- paste0(
  "https://ers.usda.gov/media/5403/machine-readable-and-long-format-file-of-tfp-indices-",
  "and-components-for-countries-regions-countries-grouped-by-income-level-and-the-world-",
  "1961-2023.csv?v=18941"
)

#' USDA ERS International Agricultural Productivity (country TFP), tidy
#'
#' Downloads the machine-readable long-format CSV (TFP, output and input indices and
#' quantities for countries, regions, income groups and the world).
#'
#' @param url Download URL; defaults to the release verified on 2026-11-18
#'   (1961–2023, updated 2026-01-27). Set `options(acutils.usda_tfp_url = ...)` after a new release.
#' @param wide Return one column per variable instead of long format.
#' @inheritParams cached_fetch
#' @return Long tibble: `order`, `fao_code`, `iso3`, `country`, `region`, `sub_region`,
#'   `income_group`, `year`, `variable`, `value`, plus `type` (`"index"` for `*_Index`,
#'   `"quantity"` for `*_Q`) and `is_country`.
#' @section Pitfalls:
#' * `*_Index` series are indices (base year in the documentation), not levels: compare
#'   growth rates across countries, not index levels.
#' * Quantities (`*_Q`) have heterogeneous units by input (ha, workers, tonnes nutrient,
#'   constant-price USD): see the ERS documentation before building ratios.
#' * The US series here differs from the ERS "Agricultural Productivity in the U.S." product.
#' @export
get_usda_tfp <- function(url = getOption("acutils.usda_tfp_url", .usda_tfp_url),
                         wide = FALSE, dir = NULL, refresh = FALSE) {
  path <- cached_fetch("usda-ers-iap", "csv", function(p) .download(url, p),
                       source = "USDA ERS International Agricultural Productivity",
                       url = url, version = "1961-2023 release (updated 2026-01-27)",
                       dir = dir, refresh = refresh)
  df <- .tidy_usda_tfp(readr::read_csv(path, col_types = readr::cols(.default = "c"),
                                       progress = FALSE))
  if (wide) {
    df <- tidyr::pivot_wider(df[setdiff(names(df), "type")], names_from = "variable",
                             values_from = "value")
  }
  .with_source(df, dataset = "USDA ERS International Agricultural Productivity",
               accessed = attr(path, "accessed"), url = url)
}

.tidy_usda_tfp <- function(raw) {
  names(raw) <- .clean_names(names(raw))
  ren <- c(fao = "fao_code", country_territory = "country", inc_i = "income_group")
  for (o in names(ren)) if (o %in% names(raw)) names(raw)[names(raw) == o] <- ren[[o]]
  need <- c("iso3", "year", "variable", "value")
  miss <- setdiff(need, names(raw))
  if (length(miss)) stop("USDA TFP file layout changed; missing columns: ",
                         paste(miss, collapse = ", "), call. = FALSE)
  raw$year  <- as.integer(raw$year)
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw$type  <- ifelse(grepl("_Index$", raw$variable), "index",
                      ifelse(grepl("_Q$", raw$variable), "quantity", "other"))
  raw$is_country <- !is.na(raw$iso3) & grepl("^[A-Z]{3}$", raw$iso3)
  tibble::as_tibble(raw)
}

# Verified 2026-11-18 on https://www.worldbank.org/en/research/commodity-markets
# (November 2026 release, published 2026-11-03; next update 2026-12-02). The document
# id in the path changes each year: override with options(acutils.pinksheet_url = "...").
.pinksheet_url <- paste0(
  "https://thedocs.worldbank.org/en/doc/74e8be41ceb20fa0da750cda2f6b9e4e-0050012026/",
  "related/CMO-Historical-Data-Monthly.xlsx"
)

#' World Bank Pink Sheet: monthly commodity prices, tidy long format
#'
#' Reads sheet "Monthly Prices" of `CMO-Historical-Data-Monthly.xlsx` (nominal USD,
#' 1960 to present; includes urea, DAP, TSP, potassium chloride, phosphate rock, grains,
#' oils, natural gas).
#'
#' @param url Download URL (default verified 2026-11-18).
#' @param commodities Optional character vector; keeps commodities whose name contains
#'   any of these strings (case-insensitive), e.g. `c("Urea", "Wheat", "Maize")`.
#' @inheritParams cached_fetch
#' @return Tibble: `date` (first of month), `commodity`, `unit`, `code`, `price`.
#'   Attribute `source$version` holds the "Updated on ..." line of the file.
#' @section Pitfalls:
#' * Prices are nominal USD: deflate (e.g. with the MUV index or US CPI) for real comparisons.
#' * Some series change specification over time (e.g. wheat US HRW vs SRW); check notes.
#' * The file is revised every month: always cite the release date.
#' @export
get_pinksheet <- function(url = getOption("acutils.pinksheet_url", .pinksheet_url),
                          commodities = NULL, dir = NULL, refresh = FALSE) {
  path <- cached_fetch("wb-pinksheet-monthly", "xlsx", function(p) .download(url, p),
                       source = "World Bank Commodity Price Data (Pink Sheet), monthly",
                       url = url, dir = dir, refresh = refresh)
  raw <- readxl::read_excel(path, sheet = "Monthly Prices", col_names = FALSE,
                            col_types = "text", .name_repair = "minimal")
  df <- .parse_pinksheet(raw)
  if (!is.null(commodities)) {
    pat <- paste(tolower(commodities), collapse = "|")
    df <- df[grepl(pat, tolower(df$commodity)), , drop = FALSE]
  }
  .with_source(df, dataset = "World Bank Commodity Price Data (The Pink Sheet)",
               version = attr(df, "updated"), accessed = attr(path, "accessed"), url = url)
}

# Parses the raw cell grid; robust to the number of title rows above the header.
.parse_pinksheet <- function(raw) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  first <- as.character(raw[[1]])
  data_rows <- which(grepl("^\\d{4}M\\d{2}$", trimws(first)))
  if (length(data_rows) == 0) stop("Pink Sheet layout changed: no 'YYYYMmm' rows found.",
                                   call. = FALSE)
  header <- raw[seq_len(min(data_rows) - 1), , drop = FALSE]
  cells <- function(i) trimws(as.character(unlist(header[i, -1])))
  # Units row: most cells look like "($/mt)"; code row: most cells look like CODE_NAME;
  # name row: the remaining row with the most filled cells.
  share <- function(i, pat) mean(grepl(pat, cells(i)) , na.rm = TRUE)
  rows <- seq_len(nrow(header))
  unit_row <- rows[which.max(vapply(rows, share, 0, pat = "^\\(.*\\)$"))]
  code_scores <- vapply(rows, share, 0, pat = "^[A-Z][A-Z0-9_]+$")
  code_row <- if (max(code_scores) > 0.5) rows[which.max(code_scores)] else NA
  filled <- vapply(rows, function(i) sum(!is.na(cells(i)) & cells(i) != ""), 0)
  filled[c(unit_row, code_row)[!is.na(c(unit_row, code_row))]] <- -1
  name_row <- rows[which.max(filled)]
  upd <- first[rows][grepl("updated", tolower(first[rows]))]
  updated <- if (length(upd)) trimws(upd[1]) else NA_character_

  meta <- tibble::tibble(
    col = seq_len(ncol(raw))[-1],
    commodity = gsub("\\s+", " ", cells(name_row)),
    unit = gsub("^\\(|\\)$", "", cells(unit_row)),
    code = if (is.na(code_row)) NA_character_ else cells(code_row)
  )
  meta <- meta[!is.na(meta$commodity) & meta$commodity != "", , drop = FALSE]
  body <- raw[data_rows, , drop = FALSE]
  ym <- trimws(as.character(body[[1]]))
  date <- as.Date(paste0(substr(ym, 1, 4), "-", substr(ym, 6, 7), "-01"))
  out <- lapply(seq_len(nrow(meta)), function(j) {
    v <- trimws(as.character(body[[meta$col[j]]]))
    v[v %in% c("..", "…", "", "NA")] <- NA
    tibble::tibble(date = date, commodity = meta$commodity[j], unit = meta$unit[j],
                   code = meta$code[j], price = suppressWarnings(as.numeric(v)))
  })
  out <- dplyr::bind_rows(out)
  attr(out, "updated") <- updated
  out
}
