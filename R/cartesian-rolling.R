#' @name cartesian_rolling
#' @export
#' @title Linear Plot with Rolling Summaries
#'
#' @description Shows the interrupted time series in Cartesian coordinates without a periodic/cyclic components.
#'
#' @param ds_linear The [data.frame] to containing the data.
#' @param xName The variable name containing the date.
#' @param yName The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.  For example, before the first interruption, the `StageID` is `1`, and is `2` afterwards.
#' @param rollingLowerName The variable name showing the lower bound of the rolling estimate.
#' @param rollingCenterName The variable name showing the rolling estimate.
#' @param rollingUpperName The variable name showing the upper bound of the rolling estimate.
#' @param paletteDark A vector of colors used for the dark/heavy graphical elements.  The vector should have one color for each `StageID` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param paletteLight A vector of colors used for the light graphical elements.  The vector should have one color for each `StageID` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param colorSparse The color of the `slowest' trend line, which plots only one value per cycle.
#' @param changePoints A vector of values indicate the interruptions between stages.  It typically works best as a Date or a POSIXct class.
#' @param changePointLabels The text plotted above each interruption.
#' @param drawJaggedLine A boolean value indicating if a line should be plotted that connects the observed data points.
#' @param drawRollingLine A boolean value indicating if a line should be plotted that connects the rolling estimates specified by `rollingCenterName`.
#' @param drawRollingBand A boolean value indicating if a band should be plotted that envelopes the rolling estimates (whose values are take from the `rollingLowerName` and `rollingUpperName`.
#' @param drawSparseLineAndPoints A boolean value indicating if the sparse line and points should be plotted.
#'
#' @param jaggedPointSize The size of the observed data points.
#' @param jaggedLineSize The size of the line connecting the observed data points.
#' @param rollingLineSize The size of the line connecting the rolling estimates.
#' @param sparsePointSize The size of the sparse estimates.
#' @param sparseLineSize The size of the line connecting the sparse estimates.
#'
#' @param bandAlpha The amount of transparency of the rolling estimate band.
#' @param changeLineAlpha The amount of transparency marking each interruption.
#' @param changeLineSize The width of a line marking an interruption.
#'
#' @param title The string describing the plot.
#' @param xTitle The string describing the *x*-axis.
#' @param yTitle The string describing the *y*-axis.
#'
#' @return Returns a ggplot2 graphing object
#' @keywords Cartesian
#' @examples
#' library(Wats) #Load the package
#' changeMonth <- base::as.Date("1996-02-15")
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$CountyName=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, dateName="Date")
#' hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
#' portfolio <- annotate_data(
#'     ds_linear,
#'     dv_name = "BirthRate",
#'     center_function = median,
#'     spread_function = hSpread
#' )
#'
#' cartesian_rolling(
#'     portfolio$ds_linear,
#'     xName = "Date",
#'     yName = "BirthRate",
#'     stage_id_name = "StageID",
#'     changePoints = changeMonth,
#'     changePointLabels = "Bombing Effect"
#' )

cartesian_rolling <- function(ds_linear, xName, yName, stage_id_name,
                              rollingLowerName="RollingLower", rollingCenterName="RollingCenter", rollingUpperName="RollingUpper",
                              paletteDark=NULL, paletteLight=NULL, colorSparse=grDevices::adjustcolor("tan1", .5),
                              changePoints=NULL, changePointLabels=NULL,
                              drawJaggedLine=TRUE, drawRollingLine=TRUE, drawRollingBand=TRUE, drawSparseLineAndPoints=TRUE,
                              jaggedPointSize=2, jaggedLineSize=.5, rollingLineSize=1, sparsePointSize=4, sparseLineSize=.5,
                              bandAlpha=.4, changeLineAlpha=.5, changeLineSize=3,
                              title=NULL, xTitle=NULL, yTitle=NULL ) {

  stages <- base::sort(base::unique(ds_linear[[stage_id_name]]))
  stageCount <- length(stages)
  testit::assert("The number of unique `StageID` values should be 1 greater than the number of `changePoints`.", stageCount==1+length(changePoints))
  if (!is.null(changePoints)) testit::assert("The number of `changePoints` should equal the number of `changeLabels`.", length(changePoints)==length(changePointLabels))
  if (!is.null(paletteDark))  testit::assert("The number of `paletteDark` colors should equal the number of unique `StageID` values.", stageCount==length(paletteDark))
  if (!is.null(paletteLight)) testit::assert("The number of `paletteLight` colors should equal the number of unique `StageID` values.", stageCount==length(paletteLight))

  p <- ggplot2::ggplot(ds_linear, ggplot2::aes_string(x=xName, y=yName, color=stage_id_name))

  if (is.null(paletteDark)) {
    if (length(stages) <= 4L) paletteDark <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(2,4,6,8)] #There's not a risk of defining more colors than levels
    else paletteDark <- colorspace::rainbow_hcl(n=length(stages), l=40)
  }
  if (is.null(paletteLight)) {
    if (length(stages) <= 4L) paletteLight <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(1,3,5,7)] #There's not a risk of defining more colors than levels
    else paletteLight <- colorspace::rainbow_hcl(n=length(stages), l=70)
  }

  for (stage in stages) {
    dsStage <- ds_linear[stage <= ds_linear$StageProgress & ds_linear$StageProgress <= (stage+1), ]

    if (drawJaggedLine)
      p <- p + ggplot2::geom_line(size=jaggedLineSize, color=paletteDark[stage], data=dsStage)
    if (drawRollingLine)
      p <- p + ggplot2::geom_line(ggplot2::aes_string(y=rollingCenterName), data=dsStage, size=rollingLineSize, color=paletteDark[stage], na.rm=TRUE)
    if (drawRollingBand)
      p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin=rollingLowerName, ymax=rollingUpperName), data=dsStage, fill=paletteDark[stage], color=NA, alpha=bandAlpha, na.rm=TRUE)

    p <- p + ggplot2::geom_point(shape=1, color=paletteDark[stage], data=dsStage, size=jaggedPointSize)
  }

  if (drawSparseLineAndPoints) {
    p <- p + ggplot2::geom_line(data=ds_linear[ds_linear$TerminalPointInCycle,], ggplot2::aes_string(y=rollingCenterName), size=sparseLineSize, color=colorSparse)
    p <- p + ggplot2::geom_point(data=ds_linear[ds_linear$TerminalPointInCycle,], ggplot2::aes_string(y=rollingCenterName), size=sparsePointSize, shape=3, color=colorSparse)
  }

  if (!is.null(changePoints)) {
    for (i in seq_along(changePoints))  {
      p <- p + ggplot2::geom_vline(xintercept=as.integer(changePoints[i]), color=paletteLight[i+1], alpha=changeLineAlpha, size=changeLineSize)
      p <- p + ggplot2::annotate("text", x=changePoints[i], y=Inf, vjust=1.1, color=paletteLight[i+1], label=changePointLabels[i])
    }
  }

  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::labs(title=title, x=xTitle, y=yTitle)

  return( p )
}
