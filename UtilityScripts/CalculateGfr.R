rm(list=ls(all=TRUE))
require(plyr)
require(testit)
inputPathCensusCountyMonth <- "./Datasets/CensusIntercensal/CensusCountyMonth.csv"
inputPathBirthCountCountyMonth <- "./Datasets/BirthCountState.csv"
outputPathBirthCountCountyMonthCsv <- "./Datasets/CountyMonthBirthRate.csv"
outputPathBirthCountCountyMonthRda <- "./data/CountyMonthBirthRate.rda"

###################
# Read in the datasets, lightly groom, & merge.
###################
dsCensus <- read.csv(inputPathCensusCountyMonth, stringsAsFactors=F)
dsBirthCount <- read.csv(inputPathBirthCountCountyMonth, stringsAsFactors=F)
# sapply(dsCensus, class)
# sapply(dsBirthCount, class)

dsBirthCount$Year <- 1900L + dsBirthCount$Year
dsBirthCount$CountyName <- plyr::revalue(dsBirthCount$CountyName, replace=c(
  "clevelan" = "cleveland",
  "pottawat" = "pottawatomie"
))
dsBirthCount$Dummy <- TRUE

#dsCountyMonth <- merge(x=dsCensus, y=dsBirthCount, by=c("CountyName", "Year", "Month"), all.x=TRUE, all.y=FALSE)
dsCountyMonth <- plyr::join(x=dsCensus, y=dsBirthCount, by=c("CountyName", "Year", "Month"), type="left")
testit::assert("All left records should find a right record", all(dsCountyMonth$Dummy))
dsCountyMonth$Dummy <- NULL

dsCountyMonth$Date <- as.Date(ISOdate(dsCountyMonth$Year, dsCountyMonth$Month, 15L))
dsCountyMonth$DaysInMonth <- lubridate::days_in_month(dsCountyMonth$Date)
dsCountyMonth$DaysInYear <- as.integer(365L + lubridate::leap_year(dsCountyMonth$Date))

rm(dsCensus, dsBirthCount, inputPathCensusCountyMonth, inputPathBirthCountCountyMonth)

###################
# Calculate GFR
###################L
dsCountyMonth$BirthRateMonthly <- dsCountyMonth$BirthCount / dsCountyMonth$FecundPopulation * 1000L

dsCountyMonth$BirthRate <- dsCountyMonth$BirthRateMonthly * dsCountyMonth$DaysInYear / dsCountyMonth$DaysInMonth

#For diagnostic purposes, use only the 1990 estimate.
dsCountyMonth <- plyr::ddply(dsCountyMonth, .variables="Fips", transform,
                             BirthRateUnadjustedFrom1990=BirthCount / FecundPopulation[1] * 1000L)

# require(ggplot2)
# ggplot(dsCountyMonth, aes(x=Date, y=BirthRate, color=factor(Fips))) + geom_line() + labs(title="Distributions of County Fertility")
# ggplot(dsCountyMonth, aes(x=BirthRate, color=factor(Fips))) + geom_density()

# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
# dsOld <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
# dsOld$Date <- as.Date(dsOld$Date) + days(15)
# 
# ggplot(dsCountyMonth[dsCountyMonth$Fips==40109, ], aes(x=Date, color=factor(Fips))) + 
#   geom_line(aes(y=BirthRate), color="tomato") +
#   geom_line(aes(y=BirthRateUnadjustedFrom1990), color="blue") +
#   geom_line(mapping=aes(y=BirthRate), data=dsOld, color="green")

###################
# Write to disk
###################
CountyMonthBirthRate <- dsCountyMonth
write.csv(CountyMonthBirthRate, file=outputPathBirthCountCountyMonthCsv, row.names=FALSE)
save(CountyMonthBirthRate, file=outputPathBirthCountCountyMonthRda, compress="xz")
