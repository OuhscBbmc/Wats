#' @name polar_periodic
#' @export
#' @title Polar Plot with Periodic Elements
#'
#' @description Shows the interrupted time series in Cartesian coordinates and its a periodic/cyclic components.
#'
#' @param ds_linear The [data.frame] to containing the simple linear data.  There should be one record per observation.
#' @param ds_stage_cycle_polar The [data.frame] to containing the bands for a single period.  There should be one record per theta per stage.  If there are three stages, this [data.frame] should have three times as many rows as `ds_linear`.
#' @param x_name The variable name containing the date.
#' @param y_name The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.  For example, before the first interruption, the `stage_id` is `1`, and is `2` afterwards.

#' @param periodic_lower_name The variable name showing the lower bound of a stage's periodic estimate.
#' @param periodic_upper_name The variable name showing the upper bound of a stage's periodic estimate.
#' @param palette_dark A vector of colors used for the dark/heavy graphical elements.  The vector should have one color for each `stage_id` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param palette_light A vector of colors used for the light graphical elements.  The vector should have one color for each `stage_id` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param change_points A vector of values indicate the interruptions between stages.  It typically works best as a Date or a POSIXct class.
#' @param change_point_labels The text plotted above each interruption.
#' @param draw_observed_line A boolean value indicating if the longitudinal observed line should be plotted (whose values are take from `ds_linear`).
#' @param draw_periodic_band A boolean value indicating if the bands should be plotted (whose values are take from the `periodic_lower_name` and `periodic_upper_name` fields).
#' @param draw_stage_labels A boolean value indicating if the stage labels should be plotted (whose values are take from `ds_linear`).
#' @param draw_radius_labels A boolean value indicating if the gridline/radius labels should be plotted (whose values are take from `tick_locations`).
#' @param jagged_point_size The size of the observed data points.
#' @param jagged_line_size The size of the line connecting the observed data points.
#'
#' @param band_alpha_dark The amount of transparency of the band appropriate for a stage's \emph{x} values.
#' @param band_alpha_light The amount of transparency of the band comparison stages for a given \emph{x} value.
#' @param change_line_alpha The amount of transparency marking each interruption.
#' @param color_labels The color for `cardinal_labels` and `origin_label`.
#' @param color_gridlines The color for the gridlines.
#' @param label_color The color of the text labels imposed on the line.
#' @param change_line_size The width of a line marking an interruption.
#' @param tick_locations The desired locations for ticks showing the value of the criterion/dependent variable.
#' @param graph_floor The value of the criterion/dependent variable at the center of the polar plot.
#' @param graph_ceiling The value of the criterion/dependent variable at the outside of the polar plot.
#'
#' @param cardinal_labels The four labels placed  where `North', `East', `South', and `West' typically are.
#' @param origin_label Explains what the criterion variable's value is at the origin.  Use `NULL` if no explanation is desired.
#' @param plot_margins A vector of four `numeric` values, specifying the number of lines in the bottom, left, top and right margins.
#'
#' @return Returns a grid graphical object (ie, a [grid::grob()].)
#' @keywords polar
#' @examples
#' library(grid)
#' library(Wats)
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear = ds_linear, date_name="date")
#'
#' h_spread <- function( scores ) { quantile(x = scores, probs = c(.25, .75)) }
#' portfolio <- annotate_data(
#'   ds_linear = ds_linear,
#'   dv_name = "birth_rate",
#'   center_function = median,
#'   spread_function = h_spread
#' )
#' rm(ds_linear)
#'
#' polarized <- polarize_cartesian(
#'   portfolio$ds_linear,
#'   portfolio$ds_stage_cycle,
#'   y_name = "birth_rate",
#'   stage_id_name = "stage_id"
#' )
#'
#' grid.newpage()
#' polar_periodic(
#'   ds_linear = polarized$ds_observed_polar,
#'   ds_stage_cycle_polar = polarized$ds_stage_cycle_polar,
#'   y_name = "radius",
#'   stage_id_name = "stage_id",
#'   cardinal_labels = c("Jan1", "Apr1", "July1", "Oct1")
#' )
#'
#' grid.newpage()
#' polar_periodic(
#'   ds_linear = polarized$ds_observed_polar,
#'   ds_stage_cycle_polar = polarized$ds_stage_cycle_polar,
#'   y_name = "radius",
#'   stage_id_name = "stage_id",
#'   draw_periodic_band = FALSE
#' )
#'
#' grid.newpage()
#' polar_periodic(
#'   ds_linear = polarized$ds_observed_polar,
#'   ds_stage_cycle_polar = polarized$ds_stage_cycle_polar,
#'   y_name = "radius",
#'   stage_id_name = "stage_id",
#'   draw_observed_line = FALSE,
#'   cardinal_labels = c("Jan1", "Apr1", "July1", "Oct1")
#' )

polar_periodic <- function(
  ds_linear,
  ds_stage_cycle_polar,
  x_name,
  y_name,
  stage_id_name,
  periodic_lower_name    = "position_lower",
  periodic_upper_name    = "position_upper",
  palette_dark           = NULL,
  palette_light          = NULL,
  change_points          = NULL,
  change_point_labels    = NULL,
  draw_observed_line     = TRUE,
  draw_periodic_band     = TRUE,
  draw_stage_labels      = FALSE,
  draw_radius_labels     = FALSE,
  jagged_point_size      = 2,
  jagged_line_size       = 1,
  band_alpha_dark        = .4,
  band_alpha_light       = .15,
  color_labels           = "gray50",
  color_gridlines        = "gray80",
  label_color            = "orange3",
  change_line_alpha      = .5,
  change_line_size       = 3,
  tick_locations         = base::pretty(x = ds_linear[[y_name]]),
  graph_floor            = min(tick_locations),
  graph_ceiling          = max(tick_locations),
  cardinal_labels        = NULL,
  origin_label           = paste0("The origin represents ", graph_floor, ";\nthe perimeter represents ", graph_ceiling, "."),
  plot_margins           = c(3.5, 2, .5, 2)
) {

  testit::assert(
    "The `ds_stage_cycle_polar` must have a valid column called `polar_lower_x`.  Typically this is generated by `Wats::polarize_cartesian()`.",
    !is.null(ds_stage_cycle_polar$polar_lower_x)
  )
  testit::assert(
    "The `ds_stage_cycle_polar` must have a valid column called `polar_lower_y`.  Typically this is generated by `Wats::polarize_cartesian()`.",
    !is.null(ds_stage_cycle_polar$polar_lower_y)
  )
  testit::assert(
    "The `ds_stage_cycle_polar` must have a valid column called `polar_upper_x`.  Typically this is generated by `Wats::polarize_cartesian()`.",
    !is.null(ds_stage_cycle_polar$polar_upper_x)
  )
  testit::assert(
    "The `ds_stage_cycle_polar` must have a valid column called `polar_upper_y`.  Typically this is generated by `Wats::polarize_cartesian()`.",
    !is.null(ds_stage_cycle_polar$polar_upper_y)
  )

  tick_locations_polar <- tick_locations - min(tick_locations)

  graph_radius <- graph_ceiling - graph_floor
  vp_range     <- c(-graph_radius, graph_radius) * 1.02
  stages       <- base::sort(base::unique(ds_linear[[stage_id_name]]))
  stage_count  <- length(stages)
  #     testit::assert("The number of unique `stage_id` values should be 1 greater than the number of `change_points`.", stage_count==1+length(change_points))

  if (!is.null(change_points )) {
    testit::assert(
      "The number of `change_points` should equal the number of `changeLabels`.",
      length(change_points) == length(change_point_labels)
    )
  }
  if (!is.null(palette_dark  )) {
    testit::assert(
      "The number of `palette_dark` colors should equal the number of unique `stage_id` values.",
      stage_count == length(palette_dark)
    )
  }
  if (!is.null(palette_light )) {
    testit::assert(
      "The number of `palette_light` colors should equal the number of unique `stage_id` values.",
      stage_count == length(palette_light)
    )
  }

  if (is.null(palette_dark)) {
    if (length(stages) <= 4L) palette_dark <- RColorBrewer::brewer.pal(n = 10L, name="Paired")[c(2L,4L,6L,8L)] #There's not a risk of defining more colors than levels
    else palette_dark <- colorspace::rainbow_hcl(n = length(stages), l = 40)
  }
  if (is.null(palette_light)) {
    if (length(stages) <= 4L) palette_light <- RColorBrewer::brewer.pal(n = 10L, name="Paired")[c(1L,3L,5L,7L)] #There's not a risk of defining more colors than levels
    else palette_light <- colorspace::rainbow_hcl(n = length(stages), l = 70)
  }
#   grid.rect() #For exploring nested viewports
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 1, respect = TRUE), gp = grid::gpar(cex = 0.6, fill = NA)))
#   grid.rect() #For exploring nested viewports
  grid::pushViewport(grid::viewport(layout.pos.col = 1, layout.pos.row = 1)) #This simple viewport is very important for the respected aspect ratio of 1.
#   grid.rect() #For exploring nested viewports
  grid::grid.text(origin_label, x = 0, y = 0, hjust=-.1, vjust=-.2, gp = grid::gpar(cex = 1.5, col = color_labels, lineheight=.8), default.units="npc")
  grid::pushViewport(grid::plotViewport(margins = plot_margins))
#   grid.rect() #For exploring nested viewports
  grid::pushViewport(grid::dataViewport(xscale = vp_range, yscale = vp_range, name="plot_region"))
#   grid.rect() #For exploring nested viewports

  grid::grid.lines(x = c(-graph_radius,graph_radius), y = c(0,0), gp = grid::gpar(col = color_gridlines, lty = 3), default.units="native")
  grid::grid.lines(x = c(0,0), y = c(-graph_radius,graph_radius), gp = grid::gpar(col = color_gridlines, lty = 3), default.units="native")
  grid::grid.circle(x = 0, y = 0, r = tick_locations_polar, default.units="native", gp = grid::gpar(col = color_gridlines))
  if (draw_radius_labels) {
#     grid::grid.text(tick_locations, x = tick_locations_polar, y = 0, default.units="native",
#                     gp = grid::gpar(col = color_gridlines), just = c(-.1, 1.1))
    grid::grid.text(tick_locations, x = tick_locations_polar/sqrt(2), y=-tick_locations_polar/sqrt(2), default.units="native",
                    gp = grid::gpar(col = color_labels), just = c(-.05, 1.05))
  }
  grid::grid.text(cardinal_labels, x = c(0, graph_radius, 0, -graph_radius), y = c(graph_radius, 0, -graph_radius, 0), gp = grid::gpar(cex = 2, col = color_labels), default.units="native")

#   lg <- grid::polylineGrob(x = ds_stage_cycle_polar$polar_lower_x, y = ds_stage_cycle_polar$polar_lower_y, id = ds_stage_cycle_polar$stage_id, gp = grid::gpar(col = palette_dark, lwd = 2), default.units="native", name="l") #summary(lg) #lg$gp
#   grid::grid.draw(lg)
#   cg <- grid::polylineGrob(x = ds_stage_cycle_polar$polar_center_x, y = ds_stage_cycle_polar$polar_center_y, id = ds_stage_cycle_polar$stage_id, gp = grid::gpar(col = palette_dark, lwd = 2), default.units="native", name="l") #summary(lg) #lg$gp
#   grid::grid.draw(cg)
#   ug <- grid::polylineGrob(x = ds_stage_cycle_polar$polar_upper_x, y = ds_stage_cycle_polar$polar_upper_y, id = ds_stage_cycle_polar$stage_id, gp = grid::gpar(col = palette_dark, lwd = 2), default.units="native", name="l") #summary(lg) #lg$gp
#   grid::grid.draw(ug)

  if (draw_periodic_band) {
    for (stageID in stages) {
      lower_x <- ds_stage_cycle_polar$polar_lower_x[ds_stage_cycle_polar$stage_id == stageID]
      lower_y <- ds_stage_cycle_polar$polar_lower_y[ds_stage_cycle_polar$stage_id == stageID]
      upper_x <- ds_stage_cycle_polar$polar_upper_x[ds_stage_cycle_polar$stage_id == stageID]
      upper_y <- ds_stage_cycle_polar$polar_upper_y[ds_stage_cycle_polar$stage_id == stageID]

      x <- c(lower_x, rev(upper_x))
      y <- c(lower_y, rev(upper_y))
      grid::grid.polygon(x = x, y = y, default.units="native", gp = grid::gpar(fill = palette_dark[stageID], col="transparent", alpha = band_alpha_dark))
    }
  }

  if (draw_observed_line) {
    for (stage in stages) {
      ds_stage <- ds_linear[stage <= ds_linear$stage_progress & ds_linear$stage_progress <= (stage+1), ]

      g_observed <-
        grid::polylineGrob(
          x             = ds_stage$observed_x,
          y             = ds_stage$observed_y,
          gp            = grid::gpar(col = palette_dark[stage], lwd = jagged_line_size),
          name          = "l",
          default.units = "native"
        )
      grid::grid.draw(g_observed)

    }
  }

  if (draw_stage_labels) {
    g_label_start <-
      grid::textGrob(
        label         = ds_linear$label_stage_start,
        x             = ds_linear$observed_x,
        y             = ds_linear$observed_y,
        gp            = grid::gpar(col = label_color, lwd = jagged_line_size),
        default.units = "native",
        name          = "l"
      )
    grid::grid.draw(g_label_start)
    g_label_end <-
      grid::textGrob(
        label         = ds_linear$label_stage_end,
        x             = ds_linear$observed_x,
        y             = ds_linear$observed_y,
        gp            = grid::gpar(col = label_color, lwd = jagged_line_size),
        default.units = "native",
        name          = "l"
      )
    grid::grid.draw(g_label_end)
  }
  grid::upViewport(n = 4)
}

# library(grid)
# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
# ds_linear <- augment_year_data_with_month_resolution(ds_linear = ds_linear, date_name="date")
#
# h_spread <- function( scores ) { quantile(x = scores, probs = c(.25, .75)) }
# portfolio <- annotate_data(ds_linear, dv_name="birth_rate", center_function = median, spread_function = h_spread)
# rm(ds_linear)
#
# polarized <- polarize_cartesian(portfolio$ds_linear, portfolio$ds_stage_cycle, y_name="birth_rate", stage_id_name="stage_id", plotted_point_count_per_cycle = 3600)
#
# grid.newpage()
# polar_periodic(ds_linear = polarized$ds_observed_polar, polarized$ds_stage_cycle_polar, draw_radius_labels = TRUE, draw_stage_labels = TRUE, y_name="radius", stage_id_name="stage_id", draw_periodic_band = FALSE)

# grid.newpage()
# polar_periodic(ds_linear = polarized$ds_observed_polar, polarized$ds_stage_cycle_polar, y_name="radius", stage_id_name="stage_id", draw_periodic_band = FALSE)

# grid.newpage()
# polar_periodic(ds_linear = polarized$ds_observed_polar, polarized$ds_stage_cycle_polar, y_name="radius", stage_id_name="stage_id", cardinal_labels = c("Jan1", "Apr1", "July1", "Oct1"))

# #
# grid.newpage()
# polar_periodic(ds_linear = polarized$ds_observed_polar, polarized$ds_stage_cycle_polar, y_name="radius", stage_id_name="stage_id", draw_observed_line = FALSE, cardinal_labels = c("Jan1", "Apr1", "July1", "Oct1"))
