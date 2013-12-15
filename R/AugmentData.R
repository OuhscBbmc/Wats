##' @name AugmentCycleData
##' @aliases AugmentYearDataWithMonthResolution AugmentYearDataWithSecondResolution
##' @export AugmentYearDataWithMonthResolution AugmentYearDataWithSecondResolution
##' 
##' @title Calculates variables necessary for WATS Plots
##' 
##' @description Calculates variables necessary for WATS Plots
##' 
##' @param dsLinear The \code{data.frame} to containing the detailed data.
##' @param dateName The variable name in \code{dsLinear} containing the date or datetime value.
##' @param stageIDName The variable name indicating the stage. In a typical interrupted time series, these values are \code{1} before the interruption and \code{2} after.
##' @return Returns a \code{data.frame} with additional variables: \code{CycleTally}, \code{ProportionThroughCycle}, \code{ProportionID}, and \code{TerminalPointInCycle}.
##' @examples
##' a <- 32+323
##' 
AugmentYearDataWithMonthResolution <- function( dsLinear, dateName, stageIDName ) {
  yearOfEvent <- lubridate::year(dsLinear[, dateName])

  minYearOfEvent <- min(yearOfEvent)
  dsLinear$CycleTally <- (yearOfEvent - minYearOfEvent)
  monthsThroughTheYear <- lubridate::month(dsLinear[, dateName]) - .5
  monthsInTheYear <- 12L
  dsLinear$ProportionThroughCycle <- monthsThroughTheYear /  monthsInTheYear
  dsLinear$ProportionID <- rank(dsLinear$ProportionThroughCycle, ties.method="max") / max(dsLinear$CycleTally + 1)
  dsLinear$StartingPointInCycle <- (dsLinear$ProportionID==min(dsLinear$ProportionID))
  dsLinear$TerminalPointInCycle <- (dsLinear$ProportionID==max(dsLinear$ProportionID))
#   dsLinear$StageProgress <- dsLinear$StageID + ifelse(dsLinear$StartingPointInCycle | dsLinear$TerminalPointInCycle, 0, 0.5)
  
  
  SummarizeWithinStage <- function( df ) {
    minValue <- min(df[, dateName])
#     maxValue <- max(df[, dateName])
#     isBetween <- ( (min(df[, dateName]) < df[, dateName]) & (df[, dateName] < max(df[, dateName])))
    isMin <-  (min(df[, dateName]) < df[, dateName])
#     
    return( df$StageID + isMin*0.5 )
  }
  dsLinear$StageProgress <- unlist(plyr::dlply(dsLinear, "StageID", SummarizeWithinStage))
  return( dsLinear )
}
AugmentYearDataWithSecondResolution <- function( dsLinear, dateName, stageIDName ) {
  yearOfEvent <- lubridate::year(dsLinear[, dateName])
  firstOfYear <- base::ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT")
  lastOfYear <- firstOfYear + lubridate::years(1)  #ISOdate(year=yearOfEvent + 1, month=1, day=1, tz="GMT") 
  
  minYearOfEvent <- min(yearOfEvent)
  dsLinear$CycleTally <- (yearOfEvent - minYearOfEvent)
  secondsThroughTheYear <- as.integer(base::difftime(time1=dsLinear[, dateName], firstOfYear, units="sec")) - .5
  secondsInTheYear <- as.integer(base::difftime(lastOfYear, firstOfYear, units="sec"))
  dsLinear$ProportionThroughCycle <- secondsThroughTheYear /  secondsInTheYear
  dsLinear$ProportionID <- rank(dsLinear$ProportionThroughCycle, ties.method="max") / max(dsLinear$CycleTally + 1)
  dsLinear$TerminalPointInCycle <- (dsLinear$ProportionID==max(dsLinear$ProportionID))  
#   dsLinear <- plyr::ddply(dsLinear, 
#                     "CycleTally", 
#                     transform, 
#                     TerminalPointInCycle=(rank(ProportionThroughCycle)==max(rank(ProportionThroughCycle))))  

  dsLinear$DV <- NULL
  return( dsLinear )
}
# 
# dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
# sapply(dsLinear, class)
# dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# # # dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# # # dsLinear$Date <- as.POSIXct(dsLinear$Date, tz="GMT")
# # # dsLinear <- AugmentYearDataWithSecondResolution(dsLinear=dsLinear, dateName="Date")
# 
# 
# head(dsLinear, 80)
# SummarizeWithinStage <- function( df ) {
#   minValue <- min(df$Date)
#   maxValue <- max(df$Date)
#   stageWidth <- as.integer(difftime(maxValue, minValue, units="days"))
#   stageProportion <- as.integer(difftime(df$Date, minValue, units="days"))
#   
#   stageProgress <- stageProportion / stageWidth 
#   
#   return( df$StageID + (stageProportion / stageWidth) )
# }

# SummarizeWithinStage <- function( df ) {
#   minValue <- min(df[, dateName])
#   maxValue <- max(df[, dateName])
#   stageWidth <- as.integer(difftime(maxValue, minValue, units="days"))
#   stageProportion <- as.integer(difftime(df$Date, minValue, units="days"))
#   
#   stageProgress <- stageProportion / stageWidth 
#   
#   return( df$StageID + (stageProportion / stageWidth) )
# }
# dsLinear$StageProgress <- unlist(plyr::dlply(dsLinear, "StageID", SummarizeWithinStage))

# head(dsLinear, 80)
# # dsLinear$DV <- dsLinear$BirthRate
# dsLinear$DV <- dsLinear$BirthRate
# l <- plyr::dlply(dsLinear, "StageID", SummarizeWithinStage)
# 
# length(unlist(l))
# 
# 
# minValue <- min(dsLinear$Date)
# maxValue <- max(dsLinear$Date)
# stageWidth <- as.integer(difftime(maxValue, minValue, units="days"))
# stageProportion <- as.integer(difftime(dsLinear$Date, minValue, units="days"))
# 
# stageProgress <- stageProportion / stageWidth 


# julian(dsLinear[, dateName])
# yday(dsLinear[, dateName])
# month(dsLinear[, dateName])
# leap_year(dsLinear[, dateName])


