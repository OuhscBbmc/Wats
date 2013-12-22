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

rm(dsCensus, dsBirthCount, inputPathCensusCountyMonth, inputPathBirthCountCountyMonth)

###################
# Calculate GFR
###################
dsCountyMonth$Gfr <- dsCountyMonth$BirthCount / dsCountyMonth$FecundPopulation * 1000L

# require(ggplot2)
ggplot(dsCountyMonth, aes(x=Date, y=Gfr, color=factor(Fips))) + geom_line() + labs(title="Distributions of County Fertility")
# ggplot(dsCountyMonth, aes(x=Gfr, color=factor(Fips))) + geom_density()

###################
# Write to disk
###################
write.csv(dsCountyMonth, file=outputPathBirthCountCountyMonthCsv, row.names=FALSE)
save(dsCountyMonth, file=outputPathBirthCountCountyMonthRda, compress="xz")
