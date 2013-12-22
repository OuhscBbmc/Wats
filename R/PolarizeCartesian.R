##' @name PolarizeCartesian
##' @export
##' 
##' @title Manipulate Cartesian data to use in the WATS polar plot
##' 
##' @description Three operations are performed.  
##' First, within each stage, the first row is repeated at the end, to close the loop.  
##' Second, multiple points are interpolated (still in a Cartesian coordinates) so that the polar graph doesn't have sharp edges.  These sharp edges would be artifacts of the conversion, and not reflect the observed data.
##' Third, the Cartesian points are coverted to polar coordinates.
##' 
##' @param dsLinear The \code{data.frame} to containing the simple linear data.  There should be one record per observation.
##' @param dsStageCycle The \code{data.frame} to containing the reoccurring/periodic bands.  There should be one record per observation per stage.  If there are three stages, this \code{data.frame} should have three times as many rows as \code{dsLinear}.
##' @param yName The variable name containing the dependent/criterion variable.
##' @param stageIDName The variable name indicating which stage the record belongs to.  For example, before the first interruption, the \code{StageID} is \code{1}, and is \code{2} afterwards.
##' @param cycleTallyName The variable name indicating how many \emph{complete} cycles have occurred at that observation.
##' @param proportionThroughCycleName The variable name showing how far through a cycle the observation (or summarized observations) occurred.
##' @param periodicLowerName The variable name showing the lower bound of a stage's periodic estimate.
##' @param periodicCenterName The variable name showing the center estimate of a stage's periodic estimate.
##' @param periodicUpperName The variable name showing the upper bound of a stage's periodic estimate.
##' @param plottedPointCountPerCycle The number of points that are plotted per cycle.  If the polar graph has 'sharp corners', then increase this value.
##' @param graphFloor The value of the criterion/dependent variable at the center of the polar plot.
##' @return Returns a \code{data.frame}.
##' @keywords polar
##' @examples
##' 532 + 9/78

PolarizeCartesian <- function(dsLinear, dsStageCycle,
                      yName, stageIDName, 
                      cycleTallyName="CycleTally", 
                      proportionThroughCycleName="ProportionThroughCycle", 
                      periodicLowerName="PositionLower", periodicCenterName="PositionCenter", periodicUpperName="PositionUpper",
                      plottedPointCountPerCycle=120,
                      graphFloor=min(base::pretty(x=dsLinear[, yName]))) {
  #TODO: allow counter-clockwise and arbitrary angle for theta=0
  
  
#   print(dsLinear[, cycleTallyName])
#   print(dsLinear[, proportionThroughCycleName])
#   print(dsLinear[, yName])
  
  closeLoop <- function( d ) {
    d[nrow(d) + 1, ] <- d[1, ] #Within each stage, repeat the first row at the end of the stage's data.frame.
    d[nrow(d), proportionThroughCycleName] <- 1 + d[nrow(d), proportionThroughCycleName]
    return( d )
  }
  interpolateObserved <- function( d, pointsPerCycleCount ) {
    observed <- stats::approx(x=d[, cycleTallyName] + d[, proportionThroughCycleName], y=d[, yName], n=pointsPerCycleCount)
    
    base::data.frame(
      ObservedX = observed$x,
      ObservedY = observed$y
#       ObservedX = d[, cycleTallyName] + d[, proportionThroughCycleName],
#       ObservedY = d[, yName]
    )   
  }
  interpolateBand <- function( d, pointsPerCycleCount ) {
    lower <- stats::approx(x=d[, proportionThroughCycleName], y=d[, periodicLowerName], n=pointsPerCycleCount)
    center <- stats::approx(x=d[, proportionThroughCycleName], y=d[, periodicCenterName], n=pointsPerCycleCount)
    upper <- stats::approx(x=d[, proportionThroughCycleName], y=d[, periodicUpperName], n=pointsPerCycleCount)
    
    base::data.frame(
      LowerX = lower$x,
      LowerY = lower$y,
      CenterX = center$x,
      CenterY = center$y,
      UpperX = upper$x,
      UpperY = upper$y
    )   
  }
  polarizeObserved <- function( d, graphFloor=graphFloor ) {
    #After R 3.1.0 has been out for a while, consider using sinpi()`.
    base::data.frame(
      ObservedX = (d$ObservedY - graphFloor) * sin(2 * pi * d$ObservedX),
      ObservedY = (d$ObservedY - graphFloor) * cos(2 * pi * d$ObservedX),
      Theta = pi * 2 * d$ObservedX,
      Radius = d$ObservedY
    )
  }
  polarizeBand <- function( d, graphFloor=graphFloor ) {
    base::data.frame(
      PolarLowerX = (d$LowerY - graphFloor) * sin(2 * pi * d$LowerX),
      PolarLowerY = (d$LowerY - graphFloor) * cos(2 * pi * d$LowerX),  
      PolarCenterX = (d$CenterY - graphFloor) * sin(2 * pi * d$CenterX),
      PolarCenterY = (d$CenterY - graphFloor) * cos(2 * pi * d$CenterX) ,  
      PolarUpperX = (d$UpperY - graphFloor) * sin(2 * pi * d$UpperX),
      PolarUpperY = (d$UpperY - graphFloor) * cos(2 * pi * d$UpperX)  
    )
  }
  
  dsObservedInterpolated <- plyr::ddply(dsLinear, .variables=stageIDName, .fun=interpolateObserved, pointsPerCycleCount=plottedPointCountPerCycle)
  dsObservedPolar <- plyr::ddply(dsObservedInterpolated, .variables=stageIDName, .fun=polarizeObserved, graphFloor=graphFloor)

  dsStageCycleClosed <- plyr::ddply(dsStageCycle, .variables=stageIDName, .fun=closeLoop)
  dsStageCycleInterpolated <- plyr::ddply(dsStageCycleClosed, .variables=stageIDName, .fun=interpolateBand, pointsPerCycleCount=plottedPointCountPerCycle)
  dsStageCyclePolar <- plyr::ddply(dsStageCycleInterpolated, .variables=stageIDName, .fun=polarizeBand, graphFloor=graphFloor)
  
  
  return( list(dsObservedPolar=dsObservedPolar, dsStageCyclePolar=dsStageCyclePolar) )
}

# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
# dsLinear <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
# dsLinear <- Wats::AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# 
# hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# portfolio <- Wats::AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=hSpread)
# 
# rm(dsLinear)
# polarized <- PolarizeCartesian(portfolio$dsLinear, portfolio$dsStageCycle, yName="BirthRate", stageIDName="StageID")
# dsStageCyclePolar <- polarized$dsStageCyclePolar
# dsObservedPolar <- polarized$dsObservedPolar
# 
# ggplot2::ggplot(dsStageCyclePolar, ggplot2::aes(color=factor(StageID))) + 
#   ggplot2::geom_path(ggplot2::aes(x=PolarLowerX, y=PolarLowerY)) + #ggplot2::geom_point(ggplot2::aes(x=PolarLowerX, y=PolarLowerY)) + 
#   ggplot2::geom_path(ggplot2::aes(x=PolarCenterX, y=PolarCenterY)) + #ggplot2::geom_point(ggplot2::aes(x=PolarCenterX, y=PolarCenterY)) + 
#   ggplot2::geom_path(ggplot2::aes(x=PolarUpperX, y=PolarUpperY)) + #ggplot2::geom_point(ggplot2::aes(x=PolarUpperX, y=PolarUpperY)) + 
#   ggplot2::geom_path(ggplot2::aes(x=ObservedX, y=ObservedY), data=dsObservedPolar, size=2) + #ggplot2::geom_point(ggplot2::aes(x=PolarUpperX, y=PolarUpperY)) + 
#   ggthemes::scale_color_tableau() +
#   ggplot2::coord_fixed(ratio=1) +
#   ggplot2::guides(color=FALSE) +
#   ggthemes::theme_solarized_2(light=FALSE)



