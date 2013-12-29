\name{PolarPeriodic}
\alias{PolarPeriodic}
\title{Polar Plot with Periodic Elements}
\usage{
PolarPeriodic(dsLinear, dsStageCyclePolar, xName, yName, stageIDName,
  periodicLowerName = "PositionLower", periodicUpperName = "PositionUpper",
  paletteDark = NULL, paletteLight = NULL, changePoints = NULL,
  changePointLabels = NULL, drawObservedLine = TRUE,
  drawPeriodicBand = TRUE, drawStageLabels = FALSE,
  drawRadiusLabels = FALSE, jaggedPointSize = 2, jaggedLineSize = 1,
  bandAlphaDark = 0.4, bandAlphaLight = 0.15, colorLabels = "gray50",
  colorGridlines = "gray80", labelColor = "orange3",
  changeLineAlpha = 0.5, changeLineSize = 3,
  tickLocations = base::pretty(x = dsLinear[, yName]),
  graphFloor = min(tickLocations), graphCeiling = max(tickLocations),
  cardinalLabels = NULL, originLabel = paste0("The origin represents ",
  graphFloor, ";\\nthe perimeter represents ", graphCeiling, "."),
  plotMargins = c(3.5, 2, 0.5, 2))
}
\arguments{
  \item{dsLinear}{The \code{data.frame} to containing the
  simple linear data.  There should be one record per
  observation.}

  \item{dsStageCyclePolar}{The \code{data.frame} to
  containing the bands for a single period.  There should
  be one record per theta per stage.  If there are three
  stages, this \code{data.frame} should have three times as
  many rows as \code{dsLinear}.}

  \item{xName}{The variable name containing the date.}

  \item{yName}{The variable name containing the
  dependent/criterion variable.}

  \item{stageIDName}{The variable name indicating which
  stage the record belongs to.  For example, before the
  first interruption, the \code{StageID} is \code{1}, and
  is \code{2} afterwards. #}

  \item{periodicLowerName}{The variable name showing the
  lower bound of a stage's periodic estimate. #}

  \item{periodicUpperName}{The variable name showing the
  upper bound of a stage's periodic estimate.}

  \item{paletteDark}{A vector of colors used for the
  dark/heavy graphical elements.  The vector should have
  one color for each \code{StageID} value.  If no vector is
  specified, a default will be chosen, based on the number
  of stages.}

  \item{paletteLight}{A vector of colors used for the light
  graphical elements.  The vector should have one color for
  each \code{StageID} value.  If no vector is specified, a
  default will be chosen, based on the number of stages.}

  \item{changePoints}{A vector of values indicate the
  interruptions between stages.  It typically works best as
  a \code{Date} or a \code{POSIXct} class.}

  \item{changePointLabels}{The text plotted above each
  interruption.}

  \item{drawObservedLine}{A boolean value indicating if the
  longitudinal observed line should be plotted (whose
  values are take from \code{dsLinear}).}

  \item{drawPeriodicBand}{A boolean value indicating if the
  bands should be plotted (whose values are take from the
  \code{periodicLowerName} and \code{periodicUpperName}
  fields).}

  \item{drawStageLabels}{A boolean value indicating if the
  stage labels should be plotted (whose values are take
  from \code{dsLinear}).}

  \item{drawRadiusLabels}{A boolean value indicating if the
  gridline/radius labels should be plotted (whose values
  are take from \code{tickLocations}).}

  \item{jaggedPointSize}{The size of the observed data
  points.}

  \item{jaggedLineSize}{The size of the line connecting the
  observed data points.}

  \item{bandAlphaDark}{The amount of transparency of the
  band appropriate for a stage's \emph{x} values.}

  \item{bandAlphaLight}{The amount of transparency of the
  band comparison stages for a given \emph{x} value.}

  \item{changeLineAlpha}{The amount of transparency marking
  each interruption.}

  \item{colorLabels}{The color for \code{cardinalLabels}
  and \code{originLabel}.}

  \item{colorGridlines}{The color for the gridlines.}

  \item{labelColor}{The color of the text labels imposed on
  the line.}

  \item{changeLineSize}{The width of a line marking an
  interruption.}

  \item{tickLocations}{The desired locations for ticks
  showing the value of the criterion/dependent variable.}

  \item{graphFloor}{The value of the criterion/dependent
  variable at the center of the polar plot.}

  \item{graphCeiling}{The value of the criterion/dependent
  variable at the outside of the polar plot.}

  \item{cardinalLabels}{The four labels placed where
  `North', `East', `South', and `West' typically are.}

  \item{originLabel}{Explains what the criterion variable's
  value is at the origin.  Use \code{NULL} if no
  explanation is desired.}

  \item{plotMargins}{A vector of four \code{numeric}
  values, specifying the number of lines in the bottom,
  left, top and right margins.}
}
\value{
Returns a \code{grid} graphical object (ie, a
\href{http://stat.ethz.ch/R-manual/R-devel/library/grid/html/grid.grob.html}{\code{grob}}.)
}
\description{
Shows the interrupted time series in Cartesian coordinates
and its a periodic/cyclic components.
}
\examples{
require(grid)
require(Wats)
dsLinear <- CountyMonthBirthRate2005Version
dsLinear <- dsLinear[dsLinear$CountyName=="oklahoma", ]
dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")

hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
portfolio <- AnnotateData(
  dsLinear = dsLinear,
  dvName = "BirthRate",
  centerFunction = median,
  spreadFunction = hSpread
)
rm(dsLinear)

polarized <- PolarizeCartesian(
  portfolio$dsLinear,
  portfolio$dsStageCycle,
  yName = "BirthRate",
  stageIDName = "StageID"
)

grid.newpage()
PolarPeriodic(
  dsLinear = polarized$dsObservedPolar,
  dsStageCyclePolar = polarized$dsStageCyclePolar,
  yName = "Radius",
  stageIDName = "StageID",
  cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1")
)

grid.newpage()
PolarPeriodic(
  dsLinear = polarized$dsObservedPolar,
  dsStageCyclePolar = polarized$dsStageCyclePolar,
  yName = "Radius",
  stageIDName = "StageID",
  drawPeriodicBand = FALSE
)

grid.newpage()
PolarPeriodic(
  dsLinear = polarized$dsObservedPolar,
  dsStageCyclePolar = polarized$dsStageCyclePolar,
  yName = "Radius",
  stageIDName = "StageID",
  drawObservedLine = FALSE,
  cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1")
)
}
\keyword{polar}

