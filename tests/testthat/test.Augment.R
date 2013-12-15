require(testthat)
#filePathOutcomes <- file.path(base::path.package("Wats"), "extdata", "BirthRatesOk.txt")
filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.


###########
context("Augment")
###########
test_that("AugmentYearDataWithMonthResolution", {
  dsBirthRate <- read.table(filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
  dsBirthRate$Date <- as.Date(dsBirthRate$Date)
  changeMonth <- as.Date("1996-02-15") # as.Date(dateBombing + weeks(40))
  dsBirthRate$StageID <- ifelse(dsBirthRate$Date < changeMonth, 1L, 2L)
  
  expectedColumnNames <- c(colnames(dsBirthRate), "CycleTally", "ProportionThroughCycle", "ProportionID"
                           , "StartingPointInCycle", "TerminalPointInCycle", "StageProgress")
  actual <- Wats::AugmentYearDataWithMonthResolution(ds=dsBirthRate, dateName="Date")
  
  expect_equal(mean(actual$ProportionThroughCycle), expected=0.5, label="The mean of the proportion should be 0.5.")
  expect_equal(actual$MonthID, expected=seq_len(120), label="There should be 120 Month IDs")
  expect_equal(actual$CycleTally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(colnames(actual), expected=expectedColumnNames, label="The correct columns should be added.")
})

test_that("AugmentYearDataWithSecondResolution", {
  dsBirthRate <- read.table(filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
  dsBirthRate$Date <- as.POSIXct(dsBirthRate$Date, tz="GMT")
  changeMonth <- as.POSIXct(as.Date("1996-02-15") )
  dsBirthRate$StageID <- ifelse(dsBirthRate$Date < changeMonth, 1L, 2L)
  
  expectedColumnNames <- c(colnames(dsBirthRate), "CycleTally", "ProportionThroughCycle", "ProportionID"
                           , "StartingPointInCycle", "TerminalPointInCycle", "StageProgress")
  actual <- Wats::AugmentYearDataWithSecondResolution(ds=dsBirthRate, dateName="Date")
  
#   expect_equal(mean(actual$ProportionThroughCycle), expected=.5, label="The mean of the proportion should be 0.5.")
  expect_equal(actual$MonthID, expected=seq_len(120), label="There should be 120 Month IDs")
  expect_equal(actual$CycleTally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(colnames(actual), expected=expectedColumnNames, label="The correct columns should be added.")
})
