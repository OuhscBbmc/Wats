##' @name AugmentCycleData
##' @aliases AugmentYearDataWithMonthResolution AugmentYearDataWithSecondResolution
##' @export AugmentYearDataWithMonthResolution AugmentYearDataWithSecondResolution
##' 
##' @title Shows the interrupted time series in Cartesian coordinates
##' 
##' @description Shows the interrupted time series in Cartesian coordinates.
##' 
##' @param ds The \code{data.frame} to containing the detailed data.
##' @param dateName The variable name in \code{ds} containing the date or datetime value.
##' @return Returns a \code{data.frame} with two additional variables: \code{CycleIndex} and \code{ProportionThroughCycle}.
##' @author Will Beasley
##' @examples
##' a <- 32+323
##' 
AugmentYearDataWithMonthResolution <- function( ds, dateName ) {
  yearOfEvent <- lubridate::year(ds[, dateName])

  minYearOfEvent <- min(yearOfEvent)
  ds$CycleIndex <- (yearOfEvent - minYearOfEvent)
  monthsThroughTheYear <- lubridate::month(ds[, dateName]) - .5
  monthsInTheYear <- 12L
  ds$ProportionThroughCycle <- monthsThroughTheYear /  monthsInTheYear
  
  return( ds )
}
AugmentYearDataWithSecondResolution <- function( ds, dateName ) {
  yearOfEvent <- lubridate::year(ds[, dateName])
  firstOfYear <- base::ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT")
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


