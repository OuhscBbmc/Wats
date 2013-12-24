###################
# Clear memory, load packages, and define global variables
###################
rm(list=ls(all=TRUE))
require(plyr)
require(testit)
inputPathCensusCountyMonth <- "./Datasets/CensusIntercensal/CensusCountyMonth.csv"
inputPathBirthCountCountyMonth <- "./Datasets/BirthCountState.csv"
outputPathBirthCountCountyMonthCsv2014 <- "./Datasets/CountyMonthBirthRate2014Version.csv"
outputPathBirthCountCountyMonthCsv2005 <- "./Datasets/CountyMonthBirthRate2005Version.csv"
outputPathBirthCountCountyMonthRda2014 <- "./data/CountyMonthBirthRate2014Version.rda"
outputPathBirthCountCountyMonthRda2005 <- "./data/CountyMonthBirthRate2005Version.rda"
changeMonth <- as.Date("1996-02-15")

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
# Define pre/post bombing stages (+9 months)
###################
dsCountyMonth$StageID <- ifelse(dsCountyMonth$Date < changeMonth, 1L, 2L)

###################
# Calculate GFR for the 2005 and the 2014 Versions
###################
dsCountyMonth2014 <-  dsCountyMonth #This is what fertility researchers should use.
dsCountyMonth2005 <-  dsCountyMonth #This is better for 2014 article, and recreates the 2005 article.

#The 2014 version uses the interpolated
dsCountyMonth2014$BirthRateMonthly <- dsCountyMonth2014$BirthCount / dsCountyMonth2014$FecundPopulation * 1000L

#To recreate the 2005 paper, use only the 1990 estimate.
dsCountyMonth2005 <- plyr::ddply(
  .data= dsCountyMonth2005, 
  .variables = "Fips",
  .fun = transform, 
  BirthRateMonthly = (BirthCount / FecundPopulation[1] * 1000L)
)

#Adjust for months of unequal days.  Each monthly record is multiplied by abou 12.
dsCountyMonth2014$BirthRate <- dsCountyMonth2014$BirthRateMonthly * dsCountyMonth2014$DaysInYear / dsCountyMonth2014$DaysInMonth
dsCountyMonth2005$BirthRate <- dsCountyMonth2005$BirthRateMonthly * dsCountyMonth2005$DaysInYear / dsCountyMonth2005$DaysInMonth

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
CountyMonthBirthRate2014Version <- dsCountyMonth2014
CountyMonthBirthRate2005Version <- dsCountyMonth2005

write.csv(CountyMonthBirthRate2014Version, file=outputPathBirthCountCountyMonthCsv2014, row.names=FALSE)
write.csv(CountyMonthBirthRate2005Version, file=outputPathBirthCountCountyMonthCsv2005, row.names=FALSE)
save(CountyMonthBirthRate2014Version, file=outputPathBirthCountCountyMonthRda2014, compress="xz")
save(CountyMonthBirthRate2005Version, file=outputPathBirthCountCountyMonthRda2005, compress="xz")
