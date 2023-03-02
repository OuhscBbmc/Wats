#' @name polar_periodic
#' @export
#' @title Polar Plot with Periodic Elements
#'
#' @description Shows the interrupted time series in Cartesian coordinates and its a periodic/cyclic components.
#'
#' @param ds_linear The [data.frame] to containing the simple linear data.  There should be one record per observation.
#' @param dsStageCyclePolar The [data.frame] to containing the bands for a single period.  There should be one record per theta per stage.  If there are three stages, this [data.frame] should have three times as many rows as `ds_linear`.
#' @param x_name The variable name containing the date.
#' @param y_name The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.  For example, before the first interruption, the `StageID` is `1`, and is `2` afterwards.

#' @param periodic_lower_name The variable name showing the lower bound of a stage's periodic estimate.
#' @param periodic_upper_name The variable name showing the upper bound of a stage's periodic estimate.
#' @param palette_dark A vector of colors used for the dark/heavy graphical elements.  The vector should have one color for each `StageID` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param palette_light A vector of colors used for the light graphical elements.  The vector should have one color for each `StageID` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param change_points A vector of values indicate the interruptions between stages.  It typically works best as a Date or a POSIXct class.
#' @param change_point_labels The text plotted above each interruption.
#' @param drawObservedLine A boolean value indicating if the longitudinal observed line should be plotted (whose values are take from `ds_linear`).
#' @param draw_periodic_band A boolean value indicating if the bands should be plotted (whose values are take from the `periodic_lower_name` and `periodic_upper_name` fields).
#' @param drawStageLabels A boolean value indicating if the stage labels should be plotted (whose values are take from `ds_linear`).
#' @param drawRadiusLabels A boolean value indicating if the gridline/radius labels should be plotted (whose values are take from `tickLocations`).
#' @param jagged_point_size The size of the observed data points.
#' @param jagged_line_size The size of the line connecting the observed data points.
#'
#' @param band_alpha_dark The amount of transparency of the band appropriate for a stage's \emph{x} values.
#' @param band_alpha_light The amount of transparency of the band comparison stages for a given \emph{x} value.
#' @param change_line_alpha The amount of transparency marking each interruption.
#' @param colorLabels The color for `cardinalLabels` and `originLabel`.
#' @param colorGridlines The color for the gridlines.
#' @param labelColor The color of the text labels imposed on the line.
#' @param change_line_size The width of a line marking an interruption.
#' @param tickLocations The desired locations for ticks showing the value of the criterion/dependent variable.
#' @param graphFloor The value of the criterion/dependent variable at the center of the polar plot.
#' @param graphCeiling The value of the criterion/dependent variable at the outside of the polar plot.
#'
#' @param cardinalLabels The four labels placed  where `North', `East', `South', and `West' typically are.
#' @param originLabel Explains what the criterion variable's value is at the origin.  Use `NULL` if no explanation is desired.
#' @param plotMargins A vector of four `numeric` values, specifying the number of lines in the bottom, left, top and right margins.
#'
#' @return Returns a grid graphical object (ie, a [grid::grob()].)
#' @keywords polar
#' @examples
#' library(grid)
#' library(Wats)
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$CountyName=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
#'
#' hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
#' portfolio <- annotate_data(
#'   ds_linear = ds_linear,
#'   dv_name = "BirthRate",
#'   center_function = median,
#'   spread_function = hSpread
#' )
#' rm(ds_linear)
#'
#' polarized <- polarize_cartesian(
#'   portfolio$ds_linear,
#'   portfolio$dsStageCycle,
#'   y_name = "BirthRate",
#'   stage_id_name = "StageID"
#' )
#'
#' grid.newpage()
#' polar_periodic(
#'   ds_linear = polarized$dsObservedPolar,
#'   dsStageCyclePolar = polarized$dsStageCyclePolar,
#'   y_name = "Radius",
#'   stage_id_name = "StageID",
#'   cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1")
#' )
#'
#' grid.newpage()
#' polar_periodic(
#'   ds_linear = polarized$dsObservedPolar,
#'   dsStageCyclePolar = polarized$dsStageCyclePolar,
#'   y_name = "Radius",
#'   stage_id_name = "StageID",
#'   draw_periodic_band = FALSE
#' )
#'
#' grid.newpage()
#' polar_periodic(
#'   ds_linear = polarized$dsObservedPolar,
#'   dsStageCyclePolar = polarized$dsStageCyclePolar,
#'   y_name = "Radius",
#'   stage_id_name = "StageID",
#'   drawObservedLine = FALSE,
#'   cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1")
#' )

polar_periodic <- function(ds_linear, dsStageCyclePolar,
                          x_name, y_name, stage_id_name,
                          periodic_lower_name = "PositionLower", periodic_upper_name = "PositionUpper",
                          palette_dark = NULL, palette_light = NULL,
                          change_points = NULL, change_point_labels = NULL,
                          drawObservedLine = TRUE, draw_periodic_band = TRUE,
                          drawStageLabels = FALSE, drawRadiusLabels = FALSE,
                          jagged_point_size = 2, jagged_line_size = 1,
                          band_alpha_dark = .4, band_alpha_light = .15,
                          colorLabels = "gray50", colorGridlines = "gray80", labelColor="orange3",
                          change_line_alpha = .5, change_line_size = 3,
                          tickLocations = base::pretty(x = ds_linear[[y_name]]),
                          graphFloor = min(tickLocations),
                          graphCeiling = max(tickLocations),
                          cardinalLabels = NULL, originLabel = paste0("The origin represents ", graphFloor, ";\nthe perimeter represents ", graphCeiling, "."),
                          plotMargins = c(3.5, 2, .5, 2)
                          ) {

  testit::assert("The `dsStageCyclePolar` must have a valid column called `PolarLowerX`.  Typically this is generated by `Wats::polarize_cartesian()`.", !is.null(dsStageCyclePolar$PolarLowerX))
  testit::assert("The `dsStageCyclePolar` must have a valid column called `PolarLowerY`.  Typically this is generated by `Wats::polarize_cartesian()`.", !is.null(dsStageCyclePolar$PolarLowerY))
  testit::assert("The `dsStageCyclePolar` must have a valid column called `PolarUpperX`.  Typically this is generated by `Wats::polarize_cartesian()`.", !is.null(dsStageCyclePolar$PolarUpperX))
  testit::assert("The `dsStageCyclePolar` must have a valid column called `PolarUpperY`.  Typically this is generated by `Wats::polarize_cartesian()`.", !is.null(dsStageCyclePolar$PolarUpperY))

  tickLocationsPolar <- tickLocations - min(tickLocations)

  graphRadius <- graphCeiling - graphFloor
  vpRange <- c(-graphRadius, graphRadius) * 1.02
  stages <- base::sort(base::unique(ds_linear[[stage_id_name]]))
  stageCount <- length(stages)
  #     testit::assert("The number of unique `StageID` values should be 1 greater than the number of `change_points`.", stageCount==1+length(change_points))
  if (!is.null(change_points )) testit::assert("The number of `change_points` should equal the number of `changeLabels`.", length(change_points)==length(change_point_labels))
  if (!is.null(palette_dark  )) testit::assert("The number of `palette_dark` colors should equal the number of unique `StageID` values.", stageCount==length(palette_dark))
  if (!is.null(palette_light )) testit::assert("The number of `palette_light` colors should equal the number of unique `StageID` values.", stageCount==length(palette_light))

  if (is.null(palette_dark)) {
    if (length(stages) <= 4L) palette_dark <- RColorBrewer::brewer.pal(n=10L, name="Paired")[c(2L,4L,6L,8L)] #There's not a risk of defining more colors than levels
    else palette_dark <- colorspace::rainbow_hcl(n=length(stages), l=40)
  }
  if (is.null(palette_light)) {
    if (length(stages) <= 4L) palette_light <- RColorBrewer::brewer.pal(n=10L, name="Paired")[c(1L,3L,5L,7L)] #There's not a risk of defining more colors than levels
    else palette_light <- colorspace::rainbow_hcl(n=length(stages), l=70)
  }
#   grid.rect() #For exploring nested viewports
  grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1, ncol=1, respect=TRUE), gp=grid::gpar(cex=0.6, fill=NA)))
#   grid.rect() #For exploring nested viewports
  grid::pushViewport(grid::viewport(layout.pos.col=1, layout.pos.row=1)) #This simple viewport is very important for the respected aspect ratio of 1.
#   grid.rect() #For exploring nested viewports
  grid::grid.text(originLabel, x=0, y=0, hjust=-.1, vjust=-.2, gp=grid::gpar(cex=1.5, col=colorLabels, lineheight=.8), default.units="npc")
  grid::pushViewport(grid::plotViewport(margins=plotMargins))
#   grid.rect() #For exploring nested viewports
  grid::pushViewport(grid::dataViewport(xscale=vpRange, yscale=vpRange, name="plotRegion"))
#   grid.rect() #For exploring nested viewports

  grid::grid.lines(x=c(-graphRadius,graphRadius), y=c(0,0), gp=grid::gpar(col=colorGridlines, lty=3), default.units="native")
  grid::grid.lines(x=c(0,0), y=c(-graphRadius,graphRadius), gp=grid::gpar(col=colorGridlines, lty=3), default.units="native")
  grid::grid.circle(x=0, y=0, r=tickLocationsPolar, default.units="native", gp=grid::gpar(col=colorGridlines))
  if (drawRadiusLabels) {
#     grid::grid.text(tickLocations, x=tickLocationsPolar, y=0, default.units="native",
#                     gp=grid::gpar(col=colorGridlines), just=c(-.1, 1.1))
    grid::grid.text(tickLocations, x=tickLocationsPolar/sqrt(2), y=-tickLocationsPolar/sqrt(2), default.units="native",
                    gp=grid::gpar(col=colorLabels), just=c(-.05, 1.05))
  }
  grid::grid.text(cardinalLabels, x=c(0, graphRadius, 0, -graphRadius), y=c(graphRadius, 0, -graphRadius, 0), gp=grid::gpar(cex=2, col=colorLabels), default.units="native")

#   lg <- grid::polylineGrob(x=dsStageCyclePolar$PolarLowerX, y=dsStageCyclePolar$PolarLowerY, id=dsStageCyclePolar$StageID, gp=grid::gpar(col=palette_dark, lwd=2), default.units="native", name="l") #summary(lg) #lg$gp
#   grid::grid.draw(lg)
#   cg <- grid::polylineGrob(x=dsStageCyclePolar$PolarCenterX, y=dsStageCyclePolar$PolarCenterY, id=dsStageCyclePolar$StageID, gp=grid::gpar(col=palette_dark, lwd=2), default.units="native", name="l") #summary(lg) #lg$gp
#   grid::grid.draw(cg)
#   ug <- grid::polylineGrob(x=dsStageCyclePolar$PolarUpperX, y=dsStageCyclePolar$PolarUpperY, id=dsStageCyclePolar$StageID, gp=grid::gpar(col=palette_dark, lwd=2), default.units="native", name="l") #summary(lg) #lg$gp
#   grid::grid.draw(ug)

  if (draw_periodic_band) {
    for (stageID in stages) {
      lowerX <- dsStageCyclePolar$PolarLowerX[dsStageCyclePolar$StageID == stageID]
      lowerY <- dsStageCyclePolar$PolarLowerY[dsStageCyclePolar$StageID == stageID]
      upperX <- dsStageCyclePolar$PolarUpperX[dsStageCyclePolar$StageID == stageID]
      upperY <- dsStageCyclePolar$PolarUpperY[dsStageCyclePolar$StageID == stageID]

      x <- c(lowerX, rev(upperX))
      y <- c(lowerY, rev(upperY))
      grid::grid.polygon(x=x, y=y, default.units="native", gp=grid::gpar(fill=palette_dark[stageID], col="transparent", alpha=band_alpha_dark))
    }
  }

  if (drawObservedLine) {
#     gObserved <- grid::polylineGrob(x=ds_linear$ObservedX, y=ds_linear$ObservedY, id=ds_linear$StageID,
#                                     gp=grid::gpar(col=palette_dark, lwd=jagged_line_size),
#                                     default.units="native", name="l")
#     grid::grid.draw(gObserved)
    for (stage in stages) {
      dsStage <- ds_linear[stage <= ds_linear$StageProgress & ds_linear$StageProgress <= (stage+1), ]

      gObserved <- grid::polylineGrob(x=dsStage$ObservedX, y=dsStage$ObservedY,
                                      gp=grid::gpar(col=palette_dark[stage], lwd=jagged_line_size),
                                      default.units="native", name="l")
      grid::grid.draw(gObserved)

    }
  }

  if (drawStageLabels) {
    gLabelStart <- grid::textGrob(label=ds_linear$LabelStageStart, x=ds_linear$ObservedX, y=ds_linear$ObservedY,
                                  gp=grid::gpar(col=labelColor, lwd=jagged_line_size),
                                  default.units="native", name="l")
    grid::grid.draw(gLabelStart)
    gLabelEnd <- grid::textGrob(label=ds_linear$LabelStageEnd, x=ds_linear$ObservedX, y=ds_linear$ObservedY,
                                gp=grid::gpar(col=labelColor, lwd=jagged_line_size),
                                default.units="native", name="l")
    grid::grid.draw(gLabelEnd)
  }
  grid::upViewport(n=4)
}

# library(grid)
# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$CountyName=="oklahoma", ]
# ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
#
# hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# portfolio <- annotate_data(ds_linear, dv_name="BirthRate", center_function=median, spread_function=hSpread)
# rm(ds_linear)
#
# polarized <- polarize_cartesian(portfolio$ds_linear, portfolio$dsStageCycle, y_name="BirthRate", stage_id_name="StageID", plottedPointCountPerCycle=3600)
#
# grid.newpage()
# polar_periodic(ds_linear=polarized$dsObservedPolar, polarized$dsStageCyclePolar, drawRadiusLabels=TRUE, drawStageLabels=TRUE, y_name="Radius", stage_id_name="StageID", draw_periodic_band=FALSE)

# grid.newpage()
# polar_periodic(ds_linear=polarized$dsObservedPolar, polarized$dsStageCyclePolar, y_name="Radius", stage_id_name="StageID", draw_periodic_band=FALSE)

# grid.newpage()
# polar_periodic(ds_linear=polarized$dsObservedPolar, polarized$dsStageCyclePolar, y_name="Radius", stage_id_name="StageID", cardinalLabels=c("Jan1", "Apr1", "July1", "Oct1"))

# #
# grid.newpage()
# polar_periodic(ds_linear=polarized$dsObservedPolar, polarized$dsStageCyclePolar, y_name="Radius", stage_id_name="StageID", drawObservedLine=FALSE, cardinalLabels=c("Jan1", "Apr1", "July1", "Oct1"))
