#' @name cartesian_rolling
#' @export
#' @title Linear Plot with Rolling Summaries
#'
#' @description Shows the interrupted time series in Cartesian coordinates without a periodic/cyclic components.
#'
#' @param ds_linear The [data.frame] to containing the data.
#' @param x_name The variable name containing the date.
#' @param y_name The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.  For example, before the first interruption, the `stage_id` is `1`, and is `2` afterwards.
#' @param rolling_lower_name The variable name showing the lower bound of the rolling estimate.
#' @param rolling_center_name The variable name showing the rolling estimate.
#' @param rolling_upper_name The variable name showing the upper bound of the rolling estimate.
#' @param palette_dark A vector of colors used for the dark/heavy graphical elements.  The vector should have one color for each `stage_id` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param palette_light A vector of colors used for the light graphical elements.  The vector should have one color for each `stage_id` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param color_sparse The color of the `slowest' trend line, which plots only one value per cycle.
#' @param change_points A vector of values indicate the interruptions between stages.  It typically works best as a Date or a POSIXct class.
#' @param change_point_labels The text plotted above each interruption.
#' @param draw_jagged_line A boolean value indicating if a line should be plotted that connects the observed data points.
#' @param draw_rolling_line A boolean value indicating if a line should be plotted that connects the rolling estimates specified by `rolling_center_name`.
#' @param draw_rolling_band A boolean value indicating if a band should be plotted that envelopes the rolling estimates (whose values are take from the `rolling_lower_name` and `rolling_upper_name`.
#' @param draw_sparse_line_and_points A boolean value indicating if the sparse line and points should be plotted.
#'
#' @param jagged_point_size The size of the observed data points.
#' @param jagged_line_size The size of the line connecting the observed data points.
#' @param rolling_line_size The size of the line connecting the rolling estimates.
#' @param sparse_point_size The size of the sparse estimates.
#' @param sparse_line_size The size of the line connecting the sparse estimates.
#'
#' @param band_alpha The amount of transparency of the rolling estimate band.
#' @param change_line_alpha The amount of transparency marking each interruption.
#' @param change_line_size The width of a line marking an interruption.
#'
#' @param title The string describing the plot.
#' @param x_title The string describing the *x*-axis.
#' @param y_title The string describing the *y*-axis.
#'
#' @return Returns a ggplot2 graphing object
#' @keywords Cartesian
#' @examples
#' library(Wats) #Load the package
#' changeMonth <- base::as.Date("1996-02-15")
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
#' hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
#' portfolio <- annotate_data(
#'     ds_linear,
#'     dv_name = "birth_rate",
#'     center_function = median,
#'     spread_function = hSpread
#' )
#'
#' cartesian_rolling(
#'     portfolio$ds_linear,
#'     x_name = "date",
#'     y_name = "birth_rate",
#'     stage_id_name = "stage_id",
#'     change_points = changeMonth,
#'     change_point_labels = "Bombing Effect"
#' )

cartesian_rolling <- function(ds_linear, x_name, y_name, stage_id_name,
                              rolling_lower_name="rolling_lower", rolling_center_name="rolling_center", rolling_upper_name="rolling_upper",
                              palette_dark=NULL, palette_light=NULL, color_sparse=grDevices::adjustcolor("tan1", .5),
                              change_points=NULL, change_point_labels=NULL,
                              draw_jagged_line=TRUE, draw_rolling_line=TRUE, draw_rolling_band=TRUE, draw_sparse_line_and_points=TRUE,
                              jagged_point_size=2, jagged_line_size=.5, rolling_line_size=1, sparse_point_size=4, sparse_line_size=.5,
                              band_alpha=.4, change_line_alpha=.5, change_line_size=3,
                              title=NULL, x_title=NULL, y_title=NULL ) {

  stages <- base::sort(base::unique(ds_linear[[stage_id_name]]))
  stage_count <- length(stages)
  testit::assert("The number of unique `stage_id` values should be 1 greater than the number of `change_points`.", stage_count==1+length(change_points))
  if (!is.null(change_points)) testit::assert("The number of `change_points` should equal the number of `changeLabels`.", length(change_points)==length(change_point_labels))
  if (!is.null(palette_dark))  testit::assert("The number of `palette_dark` colors should equal the number of unique `stage_id` values.", stage_count==length(palette_dark))
  if (!is.null(palette_light)) testit::assert("The number of `palette_light` colors should equal the number of unique `stage_id` values.", stage_count==length(palette_light))

  p <- ggplot2::ggplot(ds_linear, ggplot2::aes_string(x=x_name, y=y_name, color=stage_id_name))

  if (is.null(palette_dark)) {
    if (length(stages) <= 4L) palette_dark <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(2,4,6,8)] #There's not a risk of defining more colors than levels
    else palette_dark <- colorspace::rainbow_hcl(n=length(stages), l=40)
  }
  if (is.null(palette_light)) {
    if (length(stages) <= 4L) palette_light <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(1,3,5,7)] #There's not a risk of defining more colors than levels
    else palette_light <- colorspace::rainbow_hcl(n=length(stages), l=70)
  }

  for (stage in stages) {
    ds_stage <- ds_linear[stage <= ds_linear$stage_progress & ds_linear$stage_progress <= (stage+1), ]

    if (draw_jagged_line)
      p <- p + ggplot2::geom_line(size=jagged_line_size, color=palette_dark[stage], data=ds_stage)
    if (draw_rolling_line)
      p <- p + ggplot2::geom_line(ggplot2::aes_string(y=rolling_center_name), data=ds_stage, size=rolling_line_size, color=palette_dark[stage], na.rm=TRUE)
    if (draw_rolling_band)
      p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin=rolling_lower_name, ymax=rolling_upper_name), data=ds_stage, fill=palette_dark[stage], color=NA, alpha=band_alpha, na.rm=TRUE)

    p <- p + ggplot2::geom_point(shape=1, color=palette_dark[stage], data=ds_stage, size=jagged_point_size)
  }

  if (draw_sparse_line_and_points) {
    p <- p + ggplot2::geom_line(data=ds_linear[ds_linear$terminal_point_in_cycle,], ggplot2::aes_string(y=rolling_center_name), size=sparse_line_size, color=color_sparse)
    p <- p + ggplot2::geom_point(data=ds_linear[ds_linear$terminal_point_in_cycle,], ggplot2::aes_string(y=rolling_center_name), size=sparse_point_size, shape=3, color=color_sparse)
  }

  if (!is.null(change_points)) {
    for (i in seq_along(change_points))  {
      p <- p + ggplot2::geom_vline(xintercept=as.integer(change_points[i]), color=palette_light[i+1], alpha=change_line_alpha, size=change_line_size)
      p <- p + ggplot2::annotate("text", x=change_points[i], y=Inf, vjust=1.1, color=palette_light[i+1], label=change_point_labels[i])
    }
  }

  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::labs(title=title, x=x_title, y=y_title)

  return( p )
}
