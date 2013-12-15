##' @name AnnotateData
##' @export AnnotateData
##' 
##' @title Finds midpoints and bands for the within and between cycles.
##' 
##' @description Finds midpoints and bands for the within and between cycles.
##' 
##' @param ds The \code{data.frame} to containing the detailed data.
##' @param cycleTallyName The variable name indicating how many cycles have been completed.
##' @param stageIDName The variable name indicating the stage. In a typical interrupted time series, these values are \code{1} before the interruption and \code{2} after.
##' @param proportionThroughCycleName The variable name indicating how far the point is through a cycle.  For example, 0 degrees would be \code{0}, 180 degrees would be \code{0.5}, 359 degrees would be \code{0.9972}, and 360 degrees would be \code{0}.
##' @return Returns a \code{data.frame} with additional variables <<Say what they are>>.
##' @examples
##' a <- 32+323
##' 
AnnotateData <- function( ds, cycleTallyName="CycleTally", stageIDName="StageID", proportionThroughCycleName="ProportionThroughCycle" ) {

  
  return( ds )
}

# ds <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# ds$Date <- as.Date(ds$Date) 
# ds$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# ds$StageID <- ifelse(ds$Date < changeMonth, 1L, 2L)
# ds <- AugmentYearDataWithMonthResolution(ds=ds, dateName="Date")
# head(ds, 10)




