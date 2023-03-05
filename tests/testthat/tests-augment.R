library(testthat)
#filePathOutcomes <- file.path(base::path.package("Wats"), "extdata", "BirthRatesOk.txt")
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.

test_that("augment_year_data_with_month_resolution", {
  ds_birth_rate <- county_month_birth_rate_2005_version[county_month_birth_rate_2005_version$county_name=="oklahoma", ]
#   ds_birth_rate$date <- as.Date(ds_birth_rate$date)
#   change_month <- as.Date("1996-02-15") # as.Date(dateBombing + weeks(40))
#   ds_birth_rate$stage_id <- ifelse(ds_birth_rate$date < change_month, 1L, 2L)

  expected_column_names <- c(colnames(ds_birth_rate), "cycle_tally", "proportion_through_cycle", "proportion_id"
                           , "starting_point_in_cycle", "terminal_point_in_cycle", "stage_progress")
  actual <- Wats::augment_year_data_with_month_resolution(ds=ds_birth_rate, date_name="date")

  expect_equal(mean(actual$proportion_through_cycle), expected=0.5, label="The mean of the proportion should be 0.5.")
  expect_equal(actual$proportion_id, expected=rep(1:12, times=10), label="The `proportion_id` should be correct.")
  expect_equal(actual$cycle_tally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indexes.")
  expect_equal(actual$starting_point_in_cycle, expected=rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), time=10), label="The `starting_point_in_cycle` should be correct.")
  expect_equal(actual$terminal_point_in_cycle, expected=rep(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), time=10), label="The `terminal_point_in_cycle` should be correct.")
  expect_equal(colnames(actual), expected=expected_column_names, label="The correct columns should be added.")
})

test_that("augment_year_data_with_second_resolution", {
  ds_birth_rate <- county_month_birth_rate_2005_version
  ds_birth_rate <- ds_birth_rate[ds_birth_rate$county_name=="oklahoma", ]
  ds_birth_rate$date <- as.POSIXct(ds_birth_rate$date, tz="GMT")

  expected_column_names <- c(colnames(ds_birth_rate), "cycle_tally", "proportion_through_cycle", "proportion_id"
                           , "starting_point_in_cycle", "terminal_point_in_cycle", "stage_progress")
  actual <- Wats::augment_year_data_with_second_resolution(ds=ds_birth_rate, date_name="date")

  expect_equal(mean(actual$proportion_through_cycle), expected=0.4933366, tolerance=1e-7, label="The mean of the proportion should be a little less than 0.5 (ie, ~.4933366) because the calender's first months are shorter than its last.")
  expect_equal(actual$proportion_id, expected=rep(1:12, times=10), label="The `proportion_id` should be correct.")
  expect_equal(actual$cycle_tally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(actual$starting_point_in_cycle, expected=rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), time=10), label="The `starting_point_in_cycle` should be correct.")
  expect_equal(actual$terminal_point_in_cycle, expected=rep(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), time=10), label="The `terminal_point_in_cycle` should be correct.")
  expect_equal(colnames(actual), expected=expected_column_names, label="The correct columns should be added.")
})
