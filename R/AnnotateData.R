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
##' @param proportionIDName The variable name indicating the ordinal position through a cycle.  
##' @param terminalPointInCycleName The variable name indicating the last point within a given cycle.
##' @return Returns a \code{data.frame} with additional variables <<Say what they are>>.
##' @examples
##' a <- 32+323
##' 
AnnotateData <- function( ds, 
                          cycleTallyName="CycleTally", 
                          stageIDName="StageID", 
                          proportionThroughCycleName="ProportionThroughCycle",
                          proportionIDName="ProportionID",
                          terminalPointInCycleName="TerminalPointInCycle" ) {
  

  
  return( ds )
}

ds <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
ds$Date <- as.Date(ds$Date) 
ds$MonthID <- NULL
changeMonth <- as.Date("1996-02-15")
ds$StageID <- ifelse(ds$Date < changeMonth, 1L, 2L)
ds <- AugmentYearDataWithMonthResolution(ds=ds, dateName="Date")
head(ds, 20)

# (z <- zoo::zooreg(x=ds frequency=12))
(z <- zoo::zooreg(data=ds$BirthRate, frequency=12))


summary(z)
cycle(z)
index(z)
is.regular(z)
plot.zoo(z$BirthRate)
rollmean(z, k=12, 1)
rollapply(data=z, width=12, FUN=max)
rollapply(data=z, width=12, FUN=quantile, probs=c(.25, .75))



