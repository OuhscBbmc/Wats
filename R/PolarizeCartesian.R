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
                      proportionThroughCycleName="ProportionThroughCycleMean", 
                      periodicLowerName="PositionLower", periodicCenterName="PositionCenter", periodicUpperName="PositionUpper",
                      plottedPointCountPerCycle=120,
                      graphFloor=min(base::pretty(x=dsLinear[, yName]))) {
  #TODO: allow counter-clockwise and arbitrary angle for theta=0
  
  closeLoop <- function( d ) {
    d[nrow(d) + 1, ] <- d[1, ] #Within each stage, repeat the first row at the end of the stage's data.frame.
    d[nrow(d), proportionThroughCycleName] <- 1 + d[nrow(d), proportionThroughCycleName]
    return( d )
  }
  interpolate <- function( d, pointsPerCycleCount ) {
    lower <- stats::approx(x = d[, proportionThroughCycleName], y = d[, periodicLowerName], n = pointsPerCycleCount)
    center <- stats::approx(x = d[, proportionThroughCycleName], y = d[, periodicCenterName], n = pointsPerCycleCount)
    upper <- stats::approx(x = d[, proportionThroughCycleName], y = d[, periodicUpperName], n = pointsPerCycleCount)
    
    base::data.frame(
      LowerX = lower$x,
      LowerY = lower$y,
      CenterX = center$x,
      CenterY = center$y,
      UpperX = upper$x,
      UpperY = upper$y

    )   
  }
  Polarize <- function( d, graphFloor=0 ) {
    base::data.frame(
      PolarLowerX = (d$LowerY - graphFloor) * sinpi(2 * d$LowerX),
      PolarLowerY = (d$LowerY - graphFloor) * cospi(2 * d$LowerX),  
      PolarCenterX = (d$CenterY - graphFloor) * sinpi(2 * d$CenterX),
      PolarCenterY = (d$CenterY - graphFloor) * cospi(2 * d$CenterX) ,  
      PolarUpperX = (d$UpperY - graphFloor) * sinpi(2 * d$UpperX),
      PolarUpperY = (d$UpperY - graphFloor) * cospi(2 * d$UpperX)  
    )
  }
  
  dsStageCycleClosed <- plyr::ddply(dsStageCycle, .variables=stageIDName, .fun=closeLoop)
  dsStageCycleInterpolated <- plyr::ddply(dsStageCycleClosed, .variables="StageID", .fun=interpolate, pointsPerCycleCount=plottedPointCountPerCycle)
  dsStageCyclePolar <- plyr::ddply(dsStageCycleInterpolated, .variables="StageID", .fun=Polarize, graphFloor=graphFloor)
  
  return( dsStageCyclePolar )
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
# dsStageCyclePolar <- PolarizeCartesian(portfolio$dsLinear, portfolio$dsStageCycle, yName="BirthRate", stageIDName="StageID")

# ggplot2::ggplot(dsStageCyclePolar, ggplot2::aes(x=PolarLowerX, y=PolarLowerY, color=factor(StageID))) + ggplot2::geom_path() + ggplot2::geom_point() + ggthemes::theme_solarized_2()
# ggplot2::ggplot(dsStageCyclePolar, ggplot2::aes(x=PolarCenterX, y=PolarCenterY, color=factor(StageID))) + ggplot2::geom_path() + ggplot2::geom_point() + ggthemes::theme_solarized_2()


