##' @name AnnotateData
##' @export AnnotateData
##' 
##' @title Finds midpoints and bands for the within and between cycles.
##' 
##' @description Finds midpoints and bands for the within and between cycles.
##' 
##' @param dsLinear The \code{data.frame} to containing the detailed data.
##' @param dvName The name of the dependent variable.
##' @param centerFunction A function to calculate the center of a subsample.
##' @param spreadFunction A function to calculate the bands of a subsample.
##' @param cycleTallyName The variable name indicating how many cycles have been completed.
##' @param stageIDName The variable name indicating the stage. In a typical interrupted time series, these values are \code{1} before the interruption and \code{2} after.
##' @param proportionThroughCycleName The variable name indicating how far the point is through a cycle.  For example, 0 degrees would be \code{0}, 180 degrees would be \code{0.5}, 359 degrees would be \code{0.9972}, and 360 degrees would be \code{0}.
##' @param proportionIDName The variable name indicating the ordinal position through a cycle.  
##' @param terminalPointInCycleName The variable name indicating the last point within a given cycle.
##' @return Returns a \code{data.frame} with additional variables <<Say what they are>>.
##' @examples
##' a <- 32+323
##' 
AnnotateData <- function( dsLinear, 
                          dvName,
                          centerFunction,
                          spreadFunction,
                          cycleTallyName="CycleTally", 
                          stageIDName="StageID", 
                          proportionThroughCycleName="ProportionThroughCycle",
                          proportionIDName="ProportionID",
                          terminalPointInCycleName="TerminalPointInCycle" ) {
  
  pointsInCycle <- max(dsLinear[, proportionIDName])
  testit::assert("The should be at least one point in a cycle", max(pointsInCycle)>=1)
  dsLinear$DV <- dsLinear[, dvName]
  z <- zoo::zooreg(data=dsLinear$DV, frequency=pointsInCycle)
  rollingBounds <- zoo::rollapply(data=z, width=pointsInCycle, FUN=spreadFunction)
  
  dsLinear$RollingLower <- NA
  dsLinear$RollingCenter <- NA
  dsLinear$RollingUpper <- NA
  dsLinear$RollingLower[-seq_len(pointsInCycle-1) ] <- rollingBounds[, 1]
  dsLinear$RollingCenter[-seq_len(pointsInCycle-1) ] <- zoo::rollapply(data=z, width=pointsInCycle, FUN=centerFunction)
  dsLinear$RollingUpper[-seq_len(pointsInCycle-1) ] <- rollingBounds[, 2]
  
  
  summarizePosition <- function( df ) {
    positionBounds <- hSpread(df$DV)
    #   print(positionBounds)
    data.frame(    
      PositionLower=positionBounds[1],
      PositionCenter=median(df$DV),
      PositionUpper=positionBounds[2]
    )
  }
  dsPositional <- plyr::ddply(dsLinear, .variables=c("StageID", "ProportionID"), .fun=summarizePosition)
  
  
  dsLinear$DV <- NULL
  return( list(dsLinear=dsLinear, dsPositional=dsPositional) )
}

# dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
# dsLinear <- Wats::AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# 
# 
# hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# dsCombined <- AnnotateData(dsLinear, dvName="BirthRate",centerFunction=median, spreadFunction=hSpread)
# sapply(dsCombined, head, 20)

# dsLinear$DV <- dsLinear$BirthRate
# 
# SummarizePosition <- function( df ) {
#   positionBounds <- hSpread(df$DV)
# #   print(positionBounds)
#   data.frame(    
#     PositionLower=positionBounds[1],
#     PositionCenter=median(df$DV),
#     PositionUpper=positionBounds[2]
#   )
# }
# plyr::ddply(dsLinear, .variables=c("StageID", "ProportionID"), SummarizePosition)



# 
# # (z <- zoo::zooreg(x=dsLinear frequency=12))
# (z <- zoo::zooreg(data=dsLinear$BirthRate, frequency=12))
# 
# 
# summary(z)
# cycle(z)
# index(z)
# is.regular(z)
# plot.zoo(z)
# rollmean(z, k=12, 1)
# rollapply(data=z, width=12, FUN=max)
# 
# # hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# 
# rollapply(data=z, width=12, FUN=hSpread)[, 1]
# 
# dsLinear$RollingCenter[-(1:11)]

