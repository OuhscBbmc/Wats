\name{LinearRollingPlot}
\alias{LinearRollingPlot}
\title{Linear Plot with Rolling Summaries}
\usage{
LinearRollingPlot(dsLinear, xName, yName, stageIDName,
  rollingLowerName = "RollingLower", rollingCenterName = "RollingCenter",
  rollingUpperName = "RollingUpper", paletteDark = NULL,
  paletteLight = NULL, colorPeriodic = "brown", changePoints = NULL,
  changePointLabels = NULL, drawJaggedLine = TRUE, drawRollingLine = TRUE,
  drawRollingBands = TRUE, drawPeriodicLineAndPoints = TRUE,
  jaggedPointSize = 4, jaggedLineSize = 0.5, rollingLineSize = 1,
  periodicPointSize = 4, periodicLineSize = 0.5, bandAlpha = 0.3,
  changeLineAlpha = 0.5, changeLineSize = 5, title = NULL,
  xTitle = NULL, yTitle = NULL)
}
\arguments{
  \item{dsLinear}{The \code{data.frame} to cotaining the
  data.}

  \item{xName}{The variable name containing the date.}

  \item{yName}{The variable name containing the
  dependent/criterion variable.}

  \item{stageIDName}{The variable name indicating which
  stage the record belongs to.  For example, before the
  first interruption, the \code{StageID} is \code{1}, and
  is \code{2} afterwards.}

  \item{rollingLowerName}{The variable name showing the
  lower bound of the rolling estimate.}

  \item{rollingCenterName}{The variable name showing the
  rolling estimate.}

  \item{rollingUpperName}{The variable name showing the
  upper bound of the rolling estimate.}

  \item{paletteDark}{A vector of colors used for the
  dark/heavy graphical elements.  The vector should have
  one color for each \code{StageID} value.  If no vector is
  specified, a default will be chosen, based on the number
  of stages.}

  \item{paletteLight}{A vector of colors used for the light
  graphical elements.  The vector should have one color for
  each \code{StageID} value.  If no vector is specified, a
  default will be chosen, based on the number of stages.}

  \item{colorPeriodic}{The color of the `slowest' trend
  line, which plots only one value per cycle.}

  \item{changePoints}{A vector of values indicate the
  interruptions between stages.  It typically works best as
  a \code{Date} or a \code{POSIXct} class.}

  \item{changePointLabels}{The text plotted above each
  interruption.}

  \item{drawJaggedLine}{A boolean value indicating if a
  line should be plotted that connects the observed data
  points.}

  \item{drawRollingLine}{A boolean value indicating if a
  line should be plotted that connects the rolling
  estimates specified by \code{rollingCenterName}.}

  \item{drawRollingBands}{A boolean value indicating if a
  band should be plotted that envelopes the rolling
  estimates (whose values are take from the
  \code{rollingLowerName} and \code{rollingUpperName}.}

  \item{drawPeriodicLineAndPoints}{A boolean value
  indicating if the periodic line and points should be
  plotted.}

  \item{jaggedPointSize}{The size of the observed data
  points.}

  \item{jaggedLineSize}{The size of the line connecting the
  observed data points.}

  \item{rollingLineSize}{The size of the line connecting
  the rolling estimates.}

  \item{periodicPointSize}{The size of the periodic
  estimates.}

  \item{periodicLineSize}{The size of the line connecting
  the periodic estimates.}

  \item{bandAlpha}{The amount of transparency of the
  rolling estimate band.}

  \item{changeLineAlpha}{The amount of transparency marking
  each interruption.}

  \item{changeLineSize}{The width of a line marking an
  interruption.}

  \item{title}{The string describing the plot.}

  \item{xTitle}{TThe string describing the \emph{x}-axis.}

  \item{yTitle}{TThe string describing the \emph{y}-axis.}
}
\value{
Returns a \code{ggplot2} graphing object
}
\description{
Shows the interrupted time series in Cartesian coordinates
without a periodic/cyclic components.
}
\examples{
a <- 32+323
}
\keyword{linear}

