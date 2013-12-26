\name{AnnotateData}
\alias{AnnotateData}
\title{Finds midpoints and bands for the within and between cycles.}
\usage{
AnnotateData(dsLinear, dvName, centerFunction, spreadFunction,
  cycleTallyName = "CycleTally", stageIDName = "StageID",
  stageProgressName = "StageProgress",
  proportionThroughCycleName = "ProportionThroughCycle",
  proportionIDName = "ProportionID",
  terminalPointInCycleName = "TerminalPointInCycle")
}
\arguments{
  \item{dsLinear}{The \code{data.frame} to containing the
  detailed data.}

  \item{dvName}{The name of the dependent/criterion
  variable.}

  \item{centerFunction}{A function to calculate the center
  of a subsample.}

  \item{spreadFunction}{A function to calculate the bands
  of a subsample.}

  \item{cycleTallyName}{The variable name indicating how
  many cycles have been completed.}

  \item{stageIDName}{The variable name indicating the
  stage. In a typical interrupted time series, these values
  are \code{1} before the interruption and \code{2} after.}

  \item{stageProgressName}{The variable name indicating the
  stage in a decimal form.  This is mostly for internal
  uses.}

  \item{proportionThroughCycleName}{The variable name
  indicating how far the point is through a cycle.  For
  example, 0 degrees would be \code{0}, 180 degrees would
  be \code{0.5}, 359 degrees would be \code{0.9972}, and
  360 degrees would be \code{0}.}

  \item{proportionIDName}{The variable name indicating the
  ordinal position through a cycle.}

  \item{terminalPointInCycleName}{The variable name
  indicating the last point within a given cycle.}
}
\value{
Returns a \code{data.frame} with additional variables <<Say
what they are>>.
}
\description{
Finds midpoints and bands for the within and between
cycles.  This the second of two functions that needs to be
called to produce WATS Plots.  \code{AugmentZZZ} is the
first.
}
\examples{
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

head(portfolio$dsStageCycle)
head(portfolio$dsLinear)
head(portfolio$dsPeriodic)
}

