require(testthat)
#filePathOutcomes <- file.path(base::path.package("Wats"), "extdata", "BirthRatesOk.txt")
filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt") #This approach accounts for working on developmental box.

# changeMonth <- as.Date("1996-02-15") # as.Date(dateBombing + weeks(40))
# dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)

###########
context("Augment")
###########
test_that("AugmentYearDataWithMonthResolution", {
  dsBirthRate <- read.table(filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
  dsBirthRate$Date <- as.Date(dsBirthRate$Date)
  
  expectedColumnNames <- c(colnames(dsBirthRate), "CycleTally", "ProportionThroughCycle", "TerminalPointInCycle")
  actual <- Wats::AugmentYearDataWithMonthResolution(ds=dsBirthRate, dateName="Date")
  
  expect_equal(mean(actual$ProportionThroughCycle), expected=0.5, label="The mean of the proportion should be 0.5.")
  expect_equal(actual$MonthID, expected=seq_len(120), label="There should be 120 Month IDs")
  expect_equal(actual$CycleTally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(colnames(actual), expected=expectedColumnNames, label="Three columns should be added (ie, CycleTally, ProportionThroughCycle, TerminalPointInCycle).")
})

test_that("AugmentYearDataWithSecondResolution", {
  dsBirthRate <- read.table(filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
  dsBirthRate$Date <- as.POSIXct(dsBirthRate$Date, tz="GMT")
  
  expectedColumnNames <- c(colnames(dsBirthRate), "CycleTally", "ProportionThroughCycle", "TerminalPointInCycle")
  actual <- Wats::AugmentYearDataWithSecondResolution(ds=dsBirthRate, dateName="Date")
  
#   expect_equal(mean(actual$ProportionThroughCycle), expected=.5, label="The mean of the proportion should be 0.5.")
  expect_equal(actual$MonthID, expected=seq_len(120), label="There should be 120 Month IDs")
  expect_equal(actual$CycleTally, expected=rep(0:9, each=12), label="There should be 120 Cycle Indicies.")
  expect_equal(colnames(actual), expected=expectedColumnNames, label="Three columns should be added (ie, CycleTally, ProportionThroughCycle, TerminalPointInCycle).")
})
