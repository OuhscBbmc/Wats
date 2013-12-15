\name{AugmentCycleData}
\alias{AugmentCycleData}
\alias{AugmentYearDataWithMonthResolution}
\alias{AugmentYearDataWithSecondResolution}
\title{Calculates variables necessary for WATS Plots}
\usage{
AugmentYearDataWithMonthResolution(dsLinear, dateName)
}
\arguments{
  \item{dsLinear}{The \code{data.frame} to containing the
  detailed data.}

  \item{dateName}{The variable name in \code{dsLinear}
  containing the date or datetime value.}
}
\value{
Returns a \code{data.frame} with additional variables:
\code{CycleTally}, \code{ProportionThroughCycle},
\code{ProportionID}, and \code{TerminalPointInCycle}.
}
\description{
Calculates variables necessary for WATS Plots
}
\examples{
a <- 32+323
}

