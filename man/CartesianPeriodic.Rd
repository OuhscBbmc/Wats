\name{CartesianPeriodic}
\alias{CartesianPeriodic}
\title{Linear Plot with Periodic Elements}
\usage{
CartesianPeriodic(dsLinear, dsPeriodic, xName, yName, stageIDName,
  periodicLowerName = "PositionLower", periodicUpperName = "PositionUpper",
  paletteDark = NULL, paletteLight = NULL, changePoints = NULL,
  changePointLabels = NULL, drawPeriodicBand = TRUE, jaggedPointSize = 2,
  jaggedLineSize = 0.5, bandAlphaDark = 0.4, bandAlphaLight = 0.15,
  changeLineAlpha = 0.5, changeLineSize = 3, title = NULL,
  xTitle = NULL, yTitle = NULL)
}
\arguments{
  \item{dsLinear}{The \code{data.frame} to containing the
  simple linear data.  There should be one record per
  observation.}

  \item{dsPeriodic}{The \code{data.frame} to containing the
  reoccurring/periodic bands.  There should be one record
  per observation per stage.  If there are three stages,
  this \code{data.frame} should have three times as many
  rows as \code{dsLinear}.}

  \item{xName}{The variable name containing the date.}

  \item{yName}{The variable name containing the
  dependent/criterion variable.}

  \item{stageIDName}{The variable name indicating which
  stage the record belongs to.  For example, before the
  first interruption, the \code{StageID} is \code{1}, and
  is \code{2} afterwards.}

  \item{periodicLowerName}{The variable name showing the
  lower bound of a stage's periodic estimate.}

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

  \item{drawPeriodicBand}{A boolean value indicating if the
  bands should be plotted (whose values are take from the
  \code{periodicLowerName} and \code{periodicUpperName}.}

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

  \item{changeLineSize}{The width of a line marking an
  interruption.}

  \item{title}{The string describing the plot.}

  \item{xTitle}{The string describing the \emph{x}-axis.}

  \item{yTitle}{The string describing the \emph{y}-axis.}
}
\value{
Returns a \code{ggplot2} graphing object
}
\description{
Shows the interrupted time series in Cartesian coordinates
and its a periodic/cyclic components.
}
\examples{
require(Wats) #Load the package
changeMonth <- base::as.Date("1996-02-15")
dsLinear <- CountyMonthBirthRate2005Version
dsLinear <- dsLinear[dsLinear$CountyName=="oklahoma", ]
dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
portfolio <- AnnotateData(
    dsLinear,
    dvName = "BirthRate",
    centerFunction = median,
    spreadFunction = hSpread
)

CartesianPeriodic(
  portfolio$dsLinear,
  portfolio$dsPeriodic,
  xName = "Date",
  yName = "BirthRate",
  stageIDName = "StageID",
  changePoints = changeMonth,
  changePointLabels = "Bombing Effect"
)
}
\keyword{Cartesian}

