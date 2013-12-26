\name{AugmentCycleData}
\alias{AugmentCycleData}
\alias{AugmentYearDataWithMonthResolution}
\alias{AugmentYearDataWithSecondResolution}
\title{Calculates variables necessary for WATS Plots}
\usage{
AugmentYearDataWithMonthResolution( dsLinear, dateName, stageIDName )
AugmentYearDataWithSecondResolution( dsLinear, dateName, stageIDName )
}
\arguments{
  \item{dsLinear}{The \code{data.frame} to containing the
  detailed data.}

  \item{dateName}{The variable name in \code{dsLinear}
  containing the date or datetime value.}

  \item{stageIDName}{The variable name indicating the
  stage. In a typical interrupted time series, these values
  are \code{1} before the interruption and \code{2} after.}
}
\value{
Returns a \code{data.frame} with additional variables:
\code{CycleTally}, \code{ProportionThroughCycle},
\code{ProportionID}, and \code{TerminalPointInCycle}.
}
\description{
Calculates variables necessary for WATS Plots.  This the
first of two functions that needs to be called to produce
WATS Plots.  \code{AnnotateData} is the second.
}
\examples{
require(Wats)
dsLinear <- CountyMonthBirthRate2005Version
dsLinear <- dsLinear[dsLinear$CountyName=="oklahoma", ]
dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
head(dsLinear)
}

