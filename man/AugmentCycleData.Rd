\name{AugmentCycleData}
\alias{AugmentCycleData}
\alias{AugmentYearDataWithMonthResolution}
\alias{AugmentYearDataWithSecondResolution}
\title{Calculates variables necessary for WATS Plots}
\usage{
AugmentYearDataWithMonthResolution(ds, dateName)
}
\arguments{
  \item{ds}{The \code{data.frame} to containing the
  detailed data.}

  \item{dateName}{The variable name in \code{ds} containing
  the date or datetime value.}
}
\value{
Returns a \code{data.frame} with two additional variables:
\code{CycleIndex} and \code{ProportionThroughCycle}.
}
\description{
Calculates variables necessary for WATS Plots
}
\examples{
a <- 32+323
}

