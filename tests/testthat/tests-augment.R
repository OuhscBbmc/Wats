library(testthat)
#filePathOutcomes <- file.path(base::path.package("Wats"), "extdata", "BirthRatesOk.txt")
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.

test_that("augment_year_data_with_month_resolution", {
  dsBirthRate <- county_month_birth_rate_2005_version[county_month_birth_rate_2005_version$county_name=="oklahoma", ]
#   dsBirthRate$date <- as.Date(dsBirthRate$date)
#   changeMonth <- as.Date("1996-02-15") # as.Date(dateBombing + weeks(40))
#   dsBirthRate$stage_id <- ifelse(dsBirthRate$date < changeMonth, 1L, 2L)

  expectedColumnNames <- c(colnames(dsBirthRate), "cycle_tally", "proportion_through_cycle", "proportion_id"
                           , "StartingPointInCycle", "terminal_point_in_cycle", "stage_progress")
  actual <- Wats::augment_year_data_with_month_resolution(ds=dsBirthRate, date_name="date")

  expect_equal(mean(actual$proportion_through_cycle), expected=0.5, label="The mean of the proportion should be 0.5.")
  expect_equal(actual$proportion_id, expected=rep(1:12, times=10), label="The `proportion_id` should be correct.")
  expect_equal(actual$cycle_tally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(actual$StartingPointInCycle, expected=rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), time=10), label="The `StartingPointInCycle` should be correct.")
  expect_equal(actual$terminal_point_in_cycle, expected=rep(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), time=10), label="The `terminal_point_in_cycle` should be correct.")
  expect_equal(colnames(actual), expected=expectedColumnNames, label="The correct columns should be added.")
})

test_that("augment_year_data_with_second_resolution", {
  dsBirthRate <- county_month_birth_rate_2005_version
  dsBirthRate <- dsBirthRate[dsBirthRate$county_name=="oklahoma", ]
  dsBirthRate$date <- as.POSIXct(dsBirthRate$date, tz="GMT")

  expectedColumnNames <- c(colnames(dsBirthRate), "cycle_tally", "proportion_through_cycle", "proportion_id"
                           , "StartingPointInCycle", "terminal_point_in_cycle", "stage_progress")
  actual <- Wats::augment_year_data_with_second_resolution(ds=dsBirthRate, date_name="date")

  expect_equal(mean(actual$proportion_through_cycle), expected=0.4933366, tolerance=1e-7, label="The mean of the proportion should be a little less than 0.5 (ie, ~.4933366) because the calender's first months are shorter than its last.")
  expect_equal(actual$proportion_id, expected=rep(1:12, times=10), label="The `proportion_id` should be correct.")
  expect_equal(actual$cycle_tally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(actual$StartingPointInCycle, expected=rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), time=10), label="The `StartingPointInCycle` should be correct.")
  expect_equal(actual$terminal_point_in_cycle, expected=rep(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), time=10), label="The `terminal_point_in_cycle` should be correct.")
  expect_equal(colnames(actual), expected=expectedColumnNames, label="The correct columns should be added.")
})
