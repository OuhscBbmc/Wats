\name{AugmentCycleData}
\alias{AugmentCycleData}
\alias{AugmentYearDataWithMonthResolution}
\alias{AugmentYearDataWithSecondResolution}
\title{Shows the interrupted time series in Cartesian coordinates}
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
Shows the interrupted time series in Cartesian coordinates.
}
\examples{
a <- 32+323
}
\author{
Will Beasley
}

