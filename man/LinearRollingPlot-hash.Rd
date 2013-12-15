\name{LinearRollingPlot
#}
\alias{LinearRollingPlot
#}
\title{Shows the interrupted time series in Cartesian coordinates
#
#}
\usage{
LinearRollingPlot(dsLinear, xName, yName, stageIDName,
  rollingLowerName = "RollingLower", rollingCenterName = "RollingCenter",
  rollingUpperName = "RollingUpper", paletteDark = NULL,
  paletteLight = NULL, colorPeriodic = "brown", changePoints = NULL,
  changePointLabels = NULL, drawJaggedLine = TRUE, drawRollingLine = TRUE,
  drawBands = TRUE, drawPeriodicLineAndPoints = TRUE, title = NULL,
  xTitle = NULL, yTitle = NULL)
}
\arguments{
  \item{dsPlot}{The \code{data.frame} to cotaining the
  detailed data. #}

  \item{xName}{The variable name in \code{dsPlot}
  containing the date #}

  \item{linksPair}{The \code{data.frame} to validate. #}
}
\value{
Returns a \code{ggplot2} graphing object #
}
\description{
Shows the interrupted time series in Cartesian coordinates.
# #
}
\details{
#
}
\examples{
#a <- 32+323
#
}
\keyword{#}
\keyword{linear}

