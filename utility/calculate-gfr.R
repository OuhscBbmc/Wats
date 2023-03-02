###################
# Clear memory, load packages, and define global variables
###################
rm(list=ls(all=TRUE))
requireNamespace("dplyr")
requireNamespace("testit")

inputPathCensusCountyMonth              <- "datasets/CensusIntercensal/CensusCountyMonth.csv"
inputPathBirthCountCountyMonth          <- "datasets/BirthCountState.csv"
outputPathBirthCountCountyMonthCsv2014  <- "datasets/county_month_birth_rate_2014_version.csv"
outputPathBirthCountCountyMonthCsv2005  <- "datasets/county_month_birth_rate_2005_version.csv"
outputPathBirthCountCountyMonthRda2014  <- "data/county_month_birth_rate_2014_version.rda"
outputPathBirthCountCountyMonthRda2005  <- "data/county_month_birth_rate_2005_version.rda"
changeMonth                             <- as.Date("1996-02-15")

###################
# Read in the datasets, lightly groom, & merge.
###################
dsCensus <- utils::read.csv(inputPathCensusCountyMonth)
dsBirthCount <- utils::read.csv(inputPathBirthCountCountyMonth)
# sapply(dsCensus, class)
# sapply(dsBirthCount, class)

dsBirthCount <-
  dsBirthCount |>
  dplyr::mutate(
    Dummy       = TRUE,
    Year        = 1900L + Year,

    CountyName  =
      dplyr::case_match(
        CountyName,
        "canadian"  ~ "canadian",
        "clevelan"  ~ "cleveland",
        "comanche"  ~ "comanche",
        "creek"     ~ "creek",
        "logan"     ~ "logan",
        "mcclain"   ~ "mcclain",
        "oklahoma"  ~ "oklahoma",
        "osage"     ~ "osage",
        "pottawat"  ~ "pottawatomie",
        "rogers"    ~ "rogers",
        "tulsa"     ~ "tulsa",
        "wagoner"   ~ "wagoner"
      ),
  )


dsCountyMonth <-
  dsCensus |>
  dplyr::left_join(
    dsBirthCount,
    by = c("CountyName", "Year", "Month")
  ) |>
  dplyr::mutate(
    Date        = as.Date(ISOdate(Year, Month, 15L)),
    DaysInMonth = lubridate::days_in_month(Date),
    DaysInYear  = as.integer(365L + lubridate::leap_year(Date)),

    StageID     = dplyr::if_else(Date < changeMonth, 1L, 2L), # Define pre/post bombing stages (+9 months)
  )

testit::assert("All left records should find a right record", all(dsCountyMonth$Dummy))
dsCountyMonth$Dummy <- NULL

rm(dsCensus, dsBirthCount, inputPathCensusCountyMonth, inputPathBirthCountCountyMonth)


###################
# Calculate GFR for the 2005 and the 2014 Versions
###################
dsCountyMonth2014 <-  dsCountyMonth #This is what fertility researchers should use.
dsCountyMonth2005 <-  dsCountyMonth #This is better for 2014 article, and recreates the 2005 article.

#The 2014 version uses the interpolated
dsCountyMonth2014$BirthRateMonthly <- dsCountyMonth2014$BirthCount / dsCountyMonth2014$FecundPopulation * 1000L

# To recreate the 2005 paper, use only the 1990 estimate.
dsCountyMonth2005 <-
  dsCountyMonth2005 |>
  dplyr::group_by(Fips) |>
  dplyr::mutate(
    BirthRateMonthly = (BirthCount / FecundPopulation[1] * 1000L)
  ) |>
  dplyr::ungroup()

#Adjust for months of unequal days.  Each monthly record is multiplied by abou 12.
dsCountyMonth2014$BirthRate <- dsCountyMonth2014$BirthRateMonthly * dsCountyMonth2014$DaysInYear / dsCountyMonth2014$DaysInMonth
dsCountyMonth2005$BirthRate <- dsCountyMonth2005$BirthRateMonthly * dsCountyMonth2005$DaysInYear / dsCountyMonth2005$DaysInMonth

# library(ggplot2)
# ggplot(dsCountyMonth, aes(x=Date, y=BirthRate, color=factor(Fips))) + geom_line() + labs(title="Distributions of County Fertility")
# ggplot(dsCountyMonth, aes(x=BirthRate, color=factor(Fips))) + geom_density()

# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
# dsOld <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=FALSE)
# dsOld$Date <- as.Date(dsOld$Date) + days(15)
#
# ggplot(dsCountyMonth[dsCountyMonth$Fips==40109, ], aes(x=Date, color=factor(Fips))) +
#   geom_line(aes(y=BirthRate), color="tomato") +
#   geom_line(aes(y=BirthRateUnadjustedFrom1990), color="blue") +
#   geom_line(mapping=aes(y=BirthRate), data=dsOld, color="green")

###################
# Write to disk
###################
county_month_birth_rate_2014_version <- dsCountyMonth2014
county_month_birth_rate_2005_version <- dsCountyMonth2005

write.csv(county_month_birth_rate_2014_version, file=outputPathBirthCountCountyMonthCsv2014, row.names=FALSE)
write.csv(county_month_birth_rate_2005_version, file=outputPathBirthCountCountyMonthCsv2005, row.names=FALSE)
save(county_month_birth_rate_2014_version, file=outputPathBirthCountCountyMonthRda2014, compress="xz")
save(county_month_birth_rate_2005_version, file=outputPathBirthCountCountyMonthRda2005, compress="xz")
