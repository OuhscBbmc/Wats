##' @name PolarPeriodic
##' @export
##' 
##' @title Polar Plot with Periodic Elements
##' 
##' @description Shows the interrupted time series in Cartesian coordinates and its a periodic/cyclic components.
##' 
##' @param dsLinear The \code{data.frame} to containing the simple linear data.  There should be one record per observation.
##' @param dsStageCycle The \code{data.frame} to containing the reoccurring/periodic bands.  There should be one record per observation per stage.  If there are three stages, this \code{data.frame} should have three times as many rows as \code{dsLinear}.
##' @param dsStageCyclePolar The \code{data.frame} to containing the bands for a single period.  There should be one record per theta per stage.  If there are three stages, this \code{data.frame} should have three times as many rows as \code{dsLinear}.
##' @param xName The variable name containing the date.
##' @param yName The variable name containing the dependent/criterion variable.
##' @param stageIDName The variable name indicating which stage the record belongs to.  For example, before the first interruption, the \code{StageID} is \code{1}, and is \code{2} afterwards.
# 
# ##' @param periodicLowerName The variable name showing the lower bound of a stage's periodic estimate.
# ##' @param periodicUpperName The variable name showing the upper bound of a stage's periodic estimate.
##' @param paletteDark A vector of colors used for the dark/heavy graphical elements.  The vector should have one color for each \code{StageID} value.  If no vector is specified, a default will be chosen, based on the number of stages.
##' @param paletteLight A vector of colors used for the light graphical elements.  The vector should have one color for each \code{StageID} value.  If no vector is specified, a default will be chosen, based on the number of stages.
##' @param changePoints A vector of values indicate the interruptions between stages.  It typically works best as a \code{Date} or a \code{POSIXct} class.
##' @param changePointLabels The text plotted above each interruption.
##' @param drawPeriodicBand A boolean value indicating if the bands should be plotted (whose values are take from the \code{periodicLowerName} and \code{periodicUpperName}.
##' @param jaggedPointSize The size of the observed data points.
##' @param jaggedLineSize The size of the line connecting the observed data points.
##' 
##' @param bandAlphaDark The amount of transparency of the band appropriate for a stage's \emph{x} values.
##' @param bandAlphaLight The amount of transparency of the band comparison stages for a given \emph{x} value.
##' @param changeLineAlpha The amount of transparency marking each interruption.
##' @param changeLineSize The width of a line marking an interruption.
##' @param graphFloor The value of the criterion/dependent variable at the center of the polar plot.
##' @param graphCeiling The value of the criterion/dependent variable at the outside of the polar plot.
##' 
##' @param title The string describing the plot.
##' @param xTitle The string describing the \emph{x}-axis.
##' @param yTitle The string describing the \emph{y}-axis. 
##' 
##' @return Returns a \code{grid} graphical object (ie, a \href{http://stat.ethz.ch/R-manual/R-devel/library/grid/html/grid.grob.html}{\code{grob}}.)
##' @keywords polar
##' @examples
##' 32+7854

PolarPeriodic <- function(dsLinear, dsStageCycle, dsStageCyclePolar,
                          xName, yName, stageIDName, 
                          periodicLowerName="PositionLower", periodicUpperName="PositionUpper",
                          paletteDark=NULL, paletteLight=NULL, 
                          changePoints=NULL, changePointLabels=NULL,
                          drawPeriodicBand=TRUE,
                          jaggedPointSize=2, jaggedLineSize=.5, 
                          bandAlphaDark=.4, bandAlphaLight=.15, 
                          changeLineAlpha=.5, changeLineSize=3,
                          tickLocations=base::pretty(x=dsLinear[, yName]),
                          graphFloor=min(tickLocations),
                          graphCeiling=max(tickLocations),
                          title=NULL, xTitle=NULL, yTitle=NULL 
                          ) {
#   require(grid)T
  
  graphHeight <- graphCeiling - graphFloor
#   stages <- base::sort(base::unique(dsLinear[, stageIDName]))
#   stageCount <- length(stages)
#   testit::assert("The number of unique `StageID` values should be 1 greater than the number of `changePoints`.", stageCount==1+length(changePoints))
#   if( !is.null(changePoints) ) testit::assert("The number of `changePoints` should equal the number of `changeLabels`.", length(changePoints)==length(changePointLabels))
#   if( !is.null(paletteDark) ) testit::assert("The number of `paletteDark` colors should equal the number of unique `StageID` values.", stageCount==length(paletteDark))
#   if( !is.null(paletteLight) ) testit::assert("The number of `paletteLight` colors should equal the number of unique `StageID` values.", stageCount==length(paletteLight))

vpRange <- c(-graphHeight, graphHeight) * 1.02


# pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
# pushViewport(plotViewport(c(2, 2, 2, 2))) 
# pushViewport(dataViewport(xscale=vpRange, yscale=vpRange, name="plotRegion"))
# grid.lines(x=c(-2,2), y=c(0,0), gp=gpar(col="gray80"), default.units="native")
# grid.lines(x=c(0,0), y=c(-2,2), gp=gpar(col="gray80"), default.units="native")
# grid.circle(x=0, y=0, r=0:2, default.units="native", gp=gpar(col="gray80"))
  
#   return( g )
}

filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
dsLinear <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
dsLinear$Date <- as.Date(dsLinear$Date) 
dsLinear$MonthID <- NULL
changeMonth <- as.Date("1996-02-15")
dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
dsLinear <- Wats::AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")

hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
portfolio <- Wats::AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=hSpread)

dsStageCyclePolar <- PolarizeCartesian(portfolio$dsLinear, portfolio$dsStageCycle, yName="BirthRate", stageIDName="StageID")

# ggplot2::ggplot(dsStageCyclePolar, ggplot2::aes(x=PolarX, y=PolarY, color=factor(StageID))) + ggplot2::geom_path() + ggplot2::geom_point() #+ ggthemes::theme_few()
PolarPeriodic(dsLinear=portfolio$dsLinear, dsStageCycle=portfolio$dsStageCycle, dsStageCyclePolar=dsStageCyclePolar, yName="BirthRate")




#   g <- ggplot2::ggplot(dsStageCyclePolar, ggplot2::aes(x=PolarX, y=PolarY, color=factor(StageID)))
#   g <- g + ggplot2::geom_path()
#   g <- g + ggplot2::geom_point() #This are interpolation points, not real data points.  Comment out this line for production version.
#   g <- g + ggplot2::coord_fixed(ratio=1) 
# #   g <- g + ggthemes::theme_solid() 
#   g <- g + ggplot2::guides(color="none") 
#   g <- g + ggplot2::labs(x=NULL, y=NULL) 