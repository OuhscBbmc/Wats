##' @name AugmentCycleData
##' @aliases AugmentYearDataWithMonthResolution AugmentYearDataWithSecondResolution
##' @export AugmentYearDataWithMonthResolution AugmentYearDataWithSecondResolution
##' 
##' @title Calculates variables necessary for WATS Plots
##' 
##' @description Calculates variables necessary for WATS Plots
##' 
##' @param ds The \code{data.frame} to containing the detailed data.
##' @param dateName The variable name in \code{ds} containing the date or datetime value.
##' @return Returns a \code{data.frame} with additional variables: \code{CycleTally}, \code{ProportionThroughCycle}, \code{ProportionID}, and \code{TerminalPointInCycle}.
##' @examples
##' a <- 32+323
##' 
AugmentYearDataWithMonthResolution <- function( ds, dateName ) {
  yearOfEvent <- lubridate::year(ds[, dateName])

  minYearOfEvent <- min(yearOfEvent)
  ds$CycleTally <- (yearOfEvent - minYearOfEvent)
  monthsThroughTheYear <- lubridate::month(ds[, dateName]) - .5
  monthsInTheYear <- 12L
  ds$ProportionThroughCycle <- monthsThroughTheYear /  monthsInTheYear
  ds$ProportionID <- rank(ds$ProportionThroughCycle, ties.method="max") / max(ds$CycleTally + 1)
  ds <- plyr::ddply(ds, 
                    "CycleTally", 
                    transform, 
                    TerminalPointInCycle=(rank(ProportionThroughCycle)==max(rank(ProportionThroughCycle))))  
  
  return( ds )
}
AugmentYearDataWithSecondResolution <- function( ds, dateName ) {
  yearOfEvent <- lubridate::year(ds[, dateName])
  firstOfYear <- base::ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT")
  lastOfYear <- firstOfYear + lubridate::years(1)  #ISOdate(year=yearOfEvent + 1, month=1, day=1, tz="GMT") 
  
  minYearOfEvent <- min(yearOfEvent)
  ds$CycleTally <- (yearOfEvent - minYearOfEvent)
  secondsThroughTheYear <- as.integer(base::difftime(time1=ds[, dateName], firstOfYear, units="sec")) - .5
  secondsInTheYear <- as.integer(base::difftime(lastOfYear, firstOfYear, units="sec"))
  ds$ProportionThroughCycle <- secondsThroughTheYear /  secondsInTheYear
  ds$ProportionID <- rank(ds$ProportionThroughCycle, ties.method="max") / max(ds$CycleTally + 1)
  ds <- plyr::ddply(ds, 
                    "CycleTally", 
                    transform, 
                    TerminalPointInCycle=(rank(ProportionThroughCycle)==max(rank(ProportionThroughCycle))))  
  return( ds )
}

# dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# sapply(dsLinear, class)
# ds <- AugmentYearDataWithMonthResolution(ds=dsLinear, dateName="Date")
# # dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# # dsLinear$Date <- as.POSIXct(dsLinear$Date, tz="GMT")
# # ds <- AugmentYearDataWithSecondResolution(ds=dsLinear, dateName="Date")
# 
# 
# 
# head(ds, 20)



# julian(ds[, dateName])
# yday(ds[, dateName])
# month(ds[, dateName])
# leap_year(ds[, dateName])


