balanced <- data.frame(
  iso3 = rep(c("ETH", "KEN", "TZA"), each = 4),
  year = rep(2000:2003, 3),
  yield = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  area = 1:12
)

test_that("balanced panel is recognised", {
  res <- check_panel(balanced, id = "iso3", time = "year", quiet = TRUE)
  expect_s3_class(res, "panel_check")
  expect_equal(res$n_units, 3)
  expect_equal(res$n_periods, 4)
  expect_equal(res$n_obs, 12)
  expect_true(res$balanced)
  expect_equal(res$n_duplicates, 0)
  expect_equal(res$time_range, c(2000L, 2003L))
  expect_length(res$units_with_gaps, 0)
})

test_that("NA shares are computed per variable", {
  res <- check_panel(balanced, id = "iso3", time = "year", quiet = TRUE)
  expect_equal(res$na_share[["yield"]], 1 / 12)
  expect_equal(res$na_share[["area"]], 0)
  expect_named(res$na_share, c("yield", "area"))
})

test_that("unbalanced panels and internal gaps are detected", {
  unb <- balanced[!(balanced$iso3 == "KEN" & balanced$year == 2001), ]
  res <- check_panel(unb, id = "iso3", time = "year", quiet = TRUE)
  expect_false(res$balanced)
  expect_equal(res$units_with_gaps, "KEN")
  expect_equal(unname(res$t_per_unit[["Min."]]), 3)
})

test_that("a unit that starts late is unbalanced but has no internal gap", {
  late <- balanced[!(balanced$iso3 == "TZA" & balanced$year == 2000), ]
  res <- check_panel(late, id = "iso3", time = "year", quiet = TRUE)
  expect_false(res$balanced)
  expect_length(res$units_with_gaps, 0)
})

test_that("duplicated unit-periods are counted and reported", {
  dup <- rbind(balanced, balanced[c(1, 5), ])
  res <- check_panel(dup, id = "iso3", time = "year", quiet = TRUE)
  expect_equal(res$n_duplicates, 2)
  expect_equal(nrow(res$duplicates), 2)
  expect_false(res$balanced)
})

test_that("multi-column ids work", {
  df <- data.frame(region = rep(c("N", "S"), each = 4), type = rep(c("crop", "dairy"), 4),
                   year = rep(c(2020, 2020, 2021, 2021), 2), y = 1:8)
  res <- check_panel(df, id = c("region", "type"), time = "year", quiet = TRUE)
  expect_equal(res$n_units, 4)
  expect_true(res$balanced)
})

test_that("vars restricts the NA report", {
  res <- check_panel(balanced, id = "iso3", time = "year", vars = "yield", quiet = TRUE)
  expect_named(res$na_share, "yield")
})

test_that("missing columns give an informative error", {
  expect_error(check_panel(balanced, id = "country", time = "year"), "country")
})

test_that("print method runs and flags duplicates", {
  dup <- rbind(balanced, balanced[1, ])
  expect_output(check_panel(dup, id = "iso3", time = "year"), "DUPLICATES")
  expect_output(check_panel(balanced, id = "iso3", time = "year"), "Balanced: yes")
})
