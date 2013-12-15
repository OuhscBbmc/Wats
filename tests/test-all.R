# library(testthat)
library(Wats)

packages <- utils::installed.packages()
if( packages[packages[, 1]=="testthat", "Version"] >= "0.8" ) {
  testthat::test_check("Wats")
}
rm(packages)
# testthat::test_package("Wats")
