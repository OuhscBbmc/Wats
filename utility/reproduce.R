###################################
### Reproducible Research
###################################
# When executed by R, this file will manipulate the original data sources (Oklahoma vital statistics
# and US Census estimates) to produce a groomed dataset suitable for analysis and graphing.

###################################
# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))

###################################
# Verify the working directory has been set correctly.  Much of the code assumes the working directory is the repository's root directory.
if (base::basename(base::getwd()) != "Wats") {
  base::stop("The working directory should be set to the root of the package/repository.  ",
             "It's currently set to `", base::getwd(), "`.")
}
###################################
# Install the necessary packages.
pathInstallPackages <- "utility/install-packages.R"
if ( !file.exists(pathInstallPackages)) {
  base::stop("The file `", pathInstallPackages, "` was not found.  Make sure the working directory is set to the root of the repository.")
}
# base::source(pathInstallPackages, local=new.env())

base::rm(pathInstallPackages)
###################################
# Load the necessary packages.
base::library(base)
base::library(knitr)
base::library(markdown)
base::library(testit)

###################################
# Declare the paths of the necessary files.

# The raw/input data files:
pathCensus199x <- base::paste0("datasets/raw/census-199x/STCH-icen199", 0:9, ".txt")
pathCensus200x <- "datasets/raw/census-200x/CO-EST00INT-AGESEX-5YR.csv"
pathCountyFips <- "datasets/raw/county-fips.csv"

# The derived/intermediate data files (which are produced by the repository's code files):
pathCensusYearly <- "datasets/derived/census-county-year.csv"
pathCensusMonthly <- "datasets/derived/census-county-month.csv"
pathDataForAnalaysis2005 <- "datasets/derived/county-month-birth-rate-2005-version.csv"
pathDataForAnalaysis2014 <- "datasets/derived/county-month-birth-rate-2014-version.csv"

# Code Files:
pathManipulateCensus <- "utility/isolate-census-pops-for-gfr.R"
pathCalculateGfr <- "utility/calculate-gfr.R"

#Report Files:
pathsReports <- base::file.path("./vignettes", c("mbr-figures.Rmd"))#, "OkFertilityWithIntercensalEstimates.Rmd"))

###################################
# Verify the necessary path can be found.

# The raw/input data files:
testit::assert("The 10 census files from 199x should exist.", base::file.exists(pathCensus199x))
testit::assert("The 200x census file should exist.", base::file.exists(pathCensus200x))
testit::assert("The county FIPS values should exist.", base::file.exists(pathCountyFips))

# Code Files:
testit::assert("The file that restructures the census data should exist.", base::file.exists(pathManipulateCensus))
testit::assert("The file that calculates the GFR should exist.", base::file.exists(pathCalculateGfr))

#Report Files:
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathsReports))

###################################
# Run the files that manipulate and analyze.

# Execute code that restructures the Census data
base::source(pathManipulateCensus, local=base::new.env())

# Assert that the intermediate files exist (the two files produced by `IsolateCensusPopsForGfr.R`)
testit::assert("The yearly records should exist.", base::file.exists(pathCensusYearly))
testit::assert("The monthly records should exist.", base::file.exists(pathCensusMonthly))

#Execute code that combines the census and birth count data.
base::source(pathCalculateGfr, local=base::new.env())

# Verify that the two human readable datasets are present.
testit::assert("The CSV for the 2005 Version should exist.", base::file.exists(pathDataForAnalaysis2005))
testit::assert("The CSV for the 2014 Version should exist.", base::file.exists(pathDataForAnalaysis2014))

###################################
# Build the reports
for (pathRmd in pathsReports) {
  pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
  pathHtml <- base::gsub(pattern=".Rmd$", replacement=".html", x=pathRmd)
  knitr::knit(input=pathRmd, output=pathMd)
  markdown::markdownToHTML(file=pathMd, output=pathHtml)
}
