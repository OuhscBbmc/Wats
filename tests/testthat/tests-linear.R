# library(testthat)
#
# ds_linear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
# ds_linear$Date <- as.Date(ds_linear$Date)
# changeMonth <- as.Date("1996-02-15") # as.Date(dateBombing + weeks(40))
# ds_linear$StageID <- ifelse(ds_linear$Date < changeMonth, 1L, 2L)
#
# test_that("Smoke Test", {
# #   Wats::LinearPlot(dsPlot=ds_linear, xName="Date", yName="BirthRate", idName="StageID")
#
# #   expect_equal(returned_object$data, expected=data.frame(), label="An empty data.frame should be returned.")
# #   expect_equal(returned_object$raw_csv, expected=raw(0))
# #   expect_true(is.null(returned_object$records_collapsed))
# #   expect_true(is.null(returned_object$fields_collapsed))
# #   expect_equal(returned_object$status_message, expected="Reading the REDCap data was not successful.  The error message was:\nError in textConnection(text) : invalid 'text' argument\n")
# #   expect_false(returned_object$success)
# })
