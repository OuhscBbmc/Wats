##' @name AugmentCycleData
##' @export AugmentYearDataWithMonthResolution
##' 
##' @title Shows the interrupted time series in Cartesian coordinates
##' 
##' @description Shows the interrupted time series in Cartesian coordinates.
##' 
##' @param dsPlot The \code{data.frame} to cotaining the detailed data.
##' @param xName The variable name in \code{dsPlot} containing the date
##' @param linksPair The \code{data.frame} to validate.
##' @return Returns a \code{ggplot2} graphing object
##' @author Will Beasley
##' @keywords linear
##' @examples
##' a <- 32+323
##' 
AugmentYearDataWithMonthResolution <- function( ds, dateName ) {
  yearOfEvent <- lubridate::year(ds[, dateName])
  firstOfYear <- as.Date(ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT"))
  lastOfYear <- firstOfYear + lubridate::years(1)  
  
  minYearOfEvent <- min(yearOfEvent)
  ds$CycleIndex <- (yearOfEvent - minYearOfEvent)
  monthsThroughTheYear <- month(ds[, dateName]) - .5
  monthsInTheYear <- 12L
  ds$ProportionThroughCycle <- monthsThroughTheYear /  monthsInTheYear
  
  return( ds )
}
AugmentYearDataWithSecondResolution <- function( ds, dateName ) {
  yearOfEvent <- lubridate::year(ds[, dateName])
  firstOfYear <- ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT")
  lastOfYear <- firstOfYear + lubridate::years(1)  #ISOdate(year=yearOfEvent + 1, month=1, day=1, tz="GMT") 
  
  minYearOfEvent <- min(yearOfEvent)
  ds$CycleIndex <- (yearOfEvent - minYearOfEvent)
  secondsThroughTheYear <- as.integer(base::difftime(time1=ds[, dateName], firstOfYear, units="sec")) - .5
  secondsInTheYear <- as.integer(base::difftime(lastOfYear, firstOfYear, units="sec"))
  ds$ProportionThroughCycle <- secondsThroughTheYear /  secondsInTheYear
  
  return( ds )
}

# dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# sapply(dsLinear, class)
# AugmentYearDataWithMonthResolution(ds=dsLinear, dateName="Date")
# dsLinear$Date <- as.POSIXct(dsLinear$Date, tz="GMT")
# AugmentYearDataWithSecondResolution(ds=dsLinear, dateName="Date")



# julian(ds[, dateName])
# yday(ds[, dateName])
# month(ds[, dateName])
# leap_year(ds[, dateName])


