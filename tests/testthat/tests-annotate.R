library(testthat)
#filePathOutcomes <- file.path(base::path.package("Wats"), "extdata", "BirthRatesOk.txt")
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.

test_that("Annotate Data With Month Resolution-Median", {
  ds_linear <- county_month_birth_rate_2005_version
  ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
  ds_linear <- augment_year_data_with_month_resolution(ds_linear, date_name="date")

  h_spread <- function(scores) {
    quantile(x = scores, probs = c(.25, .75))
  }
  portfolio <-
    annotate_data(
      ds_linear       = ds_linear,
      dv_name         = "birth_rate",
      center_function = median,
      spread_function = h_spread
    )

  #   head(portfolio$ds_stage_cycle); dput(portfolio$ds_stage_cycle)
  #   head(portfolio$ds_linear); dput(head(portfolio$ds_linear))
  #   head(portfolio$ds_periodic); dput(head(portfolio$ds_periodic))

  expected_stage_cycle <-
    structure(list(stage_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
    1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
    proportion_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1,
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), proportion_through_cycle = c(0.0416666666666667,
    0.125, 0.208333333333333, 0.291666666666667, 0.375, 0.458333333333333,
    0.541666666666667, 0.625, 0.708333333333333, 0.791666666666667,
    0.875, 0.958333333333333, 0.0416666666666667, 0.125, 0.208333333333333,
    0.291666666666667, 0.375, 0.458333333333333, 0.541666666666667,
    0.625, 0.708333333333333, 0.791666666666667, 0.875, 0.958333333333333
    ), position_lower = c(`25%` = 66.82, `25%` = 68.935, `25%` = 66.1325,
    `25%` = 67.72, `25%` = 67.6575, `25%` = 70.8825, `25%` = 70.41,
    `25%` = 71.1725, `25%` = 71.775, `25%` = 68.4825, `25%` = 67.04,
    `25%` = 66.1325, `25%` = 70.755, `25%` = 69.3475, `25%` = 70.6975,
    `25%` = 71.1375, `25%` = 69.565, `25%` = 70.9325, `25%` = 75.8825,
    `25%` = 75.6075, `25%` = 79.9125, `25%` = 74.72, `25%` = 69.205,
    `25%` = 72.3875), position_center = c(70.14, 69.54, 69.7,
    69.3, 68.59, 71.13, 70.755, 72.935, 72.11, 70.275, 68.695,
    66.685, 73.51, 71.965, 71.125, 72.49, 69.99, 73.755, 77.5,
    77.525, 81.555, 75.875, 71.84, 74.62), position_upper = c(`75%` = 70.55,
    `75%` = 71.7275, `75%` = 69.915, `75%` = 71.57, `75%` = 70.55,
    `75%` = 72.885, `75%` = 73.635, `75%` = 74.9675, `75%` = 73.1875,
    `75%` = 72.1125, `75%` = 69.015, `75%` = 69.1725, `75%` = 73.595,
    `75%` = 74.15, `75%` = 72.5475, `75%` = 73.4525, `75%` = 70.49,
    `75%` = 77.005, `75%` = 78.815, `75%` = 78.6275, `75%` = 82.93,
    `75%` = 76.5675, `75%` = 72.33, `75%` = 76.55)), class = c("tbl_df",
    "tbl", "data.frame"), row.names = c(NA, -24L))

  expected_linear_head <-
    structure(list(fips = c("40109", "40109", "40109", "40109", "40109",
    "40109"), county_name = c("oklahoma", "oklahoma", "oklahoma",
    "oklahoma", "oklahoma", "oklahoma"), year = c(1990L, 1990L, 1990L,
    1990L, 1990L, 1990L), month = 1:6, fecund_population = c(143192L,
    143278L, 143365L, 143452L, 143538L, 143625L), birth_count = c(853L,
    758L, 886L, 871L, 822L, 834L), date = structure(c(7319, 7350,
    7378, 7409, 7439, 7470), class = "Date"), days_in_month = c(Jan = 31L,
    Feb = 28L, Mar = 31L, Apr = 30L, May = 31L, Jun = 30L), days_in_year = c(365L,
    365L, 365L, 365L, 365L, 365L), stage_id = c(1L, 1L, 1L, 1L, 1L,
    1L), birth_rate_monthly = c(5.96, 5.29, 6.19, 6.08, 5.74, 5.82
    ), birth_rate = c(Jan = 70.14, Feb = 69.01, Mar = 72.85, Apr = 74.01,
    May = 67.59, Jun = 70.86), cycle_tally = c(0, 0, 0, 0, 0, 0),
    proportion_through_cycle = c(0.0416666666666667, 0.125, 0.208333333333333,
    0.291666666666667, 0.375, 0.458333333333333), proportion_id = c(1,
    2, 3, 4, 5, 6), starting_point_in_cycle = c(TRUE, FALSE, FALSE,
    FALSE, FALSE, FALSE), terminal_point_in_cycle = c(FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE), stage_progress = c(1, 1.5, 1.5,
    1.5, 1.5, 1.5), rolling_lower = c(NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_, NA_real_), rolling_center = c(NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), rolling_upper = c(NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)), row.names = c(NA,
    -6L), class = c("tbl_df", "tbl", "data.frame"))

  expected_periodic_head <-
    structure(list(date = structure(c(7319, 7319, 7350, 7350, 7378,
    7378), class = "Date"), stage_id_time = c(1L, 1L, 1L, 1L, 1L, 1L
    ), proportion_id = c(1, 1, 2, 2, 3, 3), stage_progress = c(1, 1,
    1.5, 1.5, 1.5, 1.5), stage_id_band = c(1L, 2L, 1L, 2L, 1L, 2L),
    proportion_through_cycle = c(0.0416666666666667, 0.0416666666666667,
    0.125, 0.125, 0.208333333333333, 0.208333333333333), position_lower = c(`25%` = 66.82,
    `25%` = 70.755, `25%` = 68.935, `25%` = 69.3475, `25%` = 66.1325,
    `25%` = 70.6975), position_center = c(70.14, 73.51, 69.54,
    71.965, 69.7, 71.125), position_upper = c(`75%` = 70.55, `75%` = 73.595,
    `75%` = 71.7275, `75%` = 74.15, `75%` = 69.915, `75%` = 72.5475
    )), row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"
    ))

  expect_equal(dim(portfolio$ds_stage_cycle), expected = c(24,  6), label="The dimensions of ds_stage_cycle should be correct.")
  expect_equal(dim(portfolio$ds_linear     ), expected = c(120, 21), label="The dimensions of ds_linear should be correct.")
  expect_equal(dim(portfolio$ds_periodic   ), expected = c(240,  9), label="The dimensions of ds_periodic should be correct.")

  expect_equal(portfolio$ds_stage_cycle   , expected = expected_stage_cycle  , ignore_attr = TRUE)
  expect_equal(head(portfolio$ds_linear)  , expected = expected_linear_head  , ignore_attr = TRUE)
  expect_equal(head(portfolio$ds_periodic), expected = expected_periodic_head, ignore_attr = TRUE)
  portfolio$ds_periodic |>
    head() |>
    expect_equal(expected = expected_periodic_head, ignore_attr = TRUE)
})
