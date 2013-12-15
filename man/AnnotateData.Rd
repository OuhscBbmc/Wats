\name{AnnotateData}
\alias{AnnotateData}
\title{Finds midpoints and bands for the within and between cycles.}
\usage{
AnnotateData(ds, cycleTallyName = "CycleTally", stageIDName = "StageID",
  proportionThroughCycleName = "ProportionThroughCycle",
  proportionIDName = "ProportionID",
  terminalPointInCycleName = "TerminalPointInCycle")
}
\arguments{
  \item{ds}{The \code{data.frame} to containing the
  detailed data.}

  \item{cycleTallyName}{The variable name indicating how
  many cycles have been completed.}

  \item{stageIDName}{The variable name indicating the
  stage. In a typical interrupted time series, these values
  are \code{1} before the interruption and \code{2} after.}

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
cycles.
}
\examples{
a <- 32+323
}

