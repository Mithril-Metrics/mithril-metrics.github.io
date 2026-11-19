# Offline tests of the parsing/caching logic (no network needed).

test_that("key numbers round-trip and format", {
  f <- tempfile(fileext = ".csv")
  kn_reset(f)
  kn_add("tfp_change_pct", -12.345, unit = "%", digits = 1, file = f)
  kn_add("n_countries", 1234, unit = "countries", digits = 0, file = f)
  kn_add("start_year", 2005, digits = 0, big_mark = "", file = f)
  kn_add("tfp_change_pct", -11.0, unit = "%", digits = 1, file = f)   # replace, not duplicate
  x <- kn_read(f)
  expect_equal(nrow(x), 3)
  expect_equal(kn("tfp_change_pct", file = f), "-11.0")
  expect_equal(kn("tfp_change_pct", file = f, unit = TRUE, abs = TRUE), "11.0%")
  expect_equal(kn("n_countries", file = f, unit = TRUE), "1,234 countries")
  expect_equal(kn("start_year", file = f), "2005")
  expect_error(kn("missing_key", file = f), "not in")
  expect_error(kn_add("Bad Key", 1, file = f), "snake_case")
})

test_that("cache names carry the access date and the newest copy is reused", {
  d <- tempfile()
  dir.create(d)
  writeLines("old", file.path(d, "src_2026-01-01.txt"))
  writeLines("new", file.path(d, "src_2026-06-01.txt"))
  hit <- acutils:::.latest_cache("src", "txt", d)
  expect_equal(basename(hit), "src_2026-06-01.txt")
  calls <- 0
  fetch_one <- function(path) {
    calls <<- calls + 1
    writeLines("x", path)
  }
  p <- cached_fetch("other", "txt", fetch_one, dir = d)
  expect_equal(calls, 1)
  expect_match(basename(p), paste0("other_", Sys.Date(), ".txt"))
  p2 <- suppressMessages(cached_fetch("other", "txt", function(path) calls <<- calls + 1, dir = d))
  expect_equal(calls, 1)
  expect_equal(nrow(access_log(d)), 1)
})

test_that("USDA TFP layout is tidied", {
  raw <- readr::read_csv(test_path("fixtures", "usda_tfp_sample.csv"),
                         col_types = readr::cols(.default = "c"))
  df <- acutils:::.tidy_usda_tfp(raw)
  expect_true(all(c("fao_code", "iso3", "country", "income_group", "year", "variable",
                    "value", "type", "is_country") %in% names(df)))
  expect_type(df$value, "double")
  expect_equal(sort(unique(df$type)), c("index", "quantity"))
  expect_false(all(df$is_country))
})

test_that("Pink Sheet grid is parsed to long format", {
  grid <- data.frame(
    c("World Bank Commodity Price Data (The Pink Sheet)", "monthly prices in nominal US dollars",
      "Updated on November 03, 2026", NA, NA, NA, "1960M01", "1960M02", "2026M10"),
    c(NA, NA, NA, "Urea", "($/mt)", "UREA_EE_BULK", "..", "..", "395.5"),
    c(NA, NA, NA, "Wheat, US HRW", "($/mt)", "WHEAT_US_HRW", "60.1", "61.0", "230.25"),
    stringsAsFactors = FALSE
  )
  names(grid) <- c("a", "b", "c")
  df <- acutils:::.parse_pinksheet(grid)
  expect_equal(nrow(df), 6)
  expect_equal(unique(df$commodity), c("Urea", "Wheat, US HRW"))
  expect_equal(unique(df$unit), "$/mt")
  expect_equal(df$code[1], "UREA_EE_BULK")
  expect_true(is.na(df$price[1]))
  expect_equal(df$price[df$commodity == "Urea" & df$date == as.Date("2026-10-01")], 395.5)
  expect_equal(attr(df, "updated"), "Updated on November 03, 2026")
})

test_that("FAOSTAT tidy + filter handles names, codes and aggregates", {
  raw <- data.frame(
    `Area Code` = c("238", "238", "5100", "114"), `Area Code (M49)` = c("'231", "'231", "'002", "'404"),
    Area = c("Ethiopia", "Ethiopia", "Africa", "Kenya"),
    `Item Code` = c("15", "15", "15", "56"), Item = c("Wheat", "Wheat", "Wheat", "Maize (corn)"),
    `Element Code` = c("5412", "5412", "5412", "5412"), Element = "Yield",
    Year = c("2020", "2021", "2021", "2021"), Unit = "kg/ha",
    Value = c("2900", "3000", "1800", "1700"), Flag = c("A", "A", "A", "E"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  df <- acutils:::.tidy_faostat(raw)
  expect_equal(df$area_code_m49[1], "231")
  expect_equal(df$is_aggregate, c(FALSE, FALSE, TRUE, FALSE))
  f1 <- acutils:::.filter_faostat(df, items = "wheat", years = 2021)
  expect_equal(nrow(f1), 2)
  f2 <- acutils:::.filter_faostat(df, areas = c("404", "Ethiopia"))
  expect_equal(sort(unique(f2$area)), c("Ethiopia", "Kenya"))
  expect_warning(acutils:::.filter_faostat(df, items = "Rice"), "not found")
})

test_that("ERA5 request converts bbox to N/W/S/E and is not sent on dry run", {
  r <- get_era5_monthly("2m_temperature", bbox = c(102, 8, 110, 24), years = 2020:2021,
                        months = 1:2, dry_run = TRUE, dir = tempdir())
  expect_equal(r$area, c(24, 102, 8, 110))
  expect_equal(unlist(r$month), c("01", "02"))
  expect_equal(r$dataset_short_name, "reanalysis-era5-single-levels-monthly-means")
  expect_match(r$target, "\\.nc$")
})

test_that("theme, palette, caption and save_fig work", {
  expect_length(ac_palette(3), 3)
  expect_error(ac_palette(9), "At most 8")
  cap <- source_caption(c("FAOSTAT QCL", "WDI"), accessed = "2026-11-18")
  expect_match(cap, "FAOSTAT QCL; WDI \\(accessed 2026-11-18\\)")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_line() + theme_ac()
  d <- tempfile()
  paths <- suppressMessages(save_fig(p, "t", dir = d))
  expect_true(all(file.exists(paths)))
  dims <- dim(png::readPNG(paths[1]))
  expect_equal(dims[1:2], c(1000, 1600))
})
