#' @name cartesian_periodic
#' @export
#' @title Linear Plot with Periodic Elements
#'
#' @description Shows the interrupted time series in Cartesian coordinates and its a periodic/cyclic components.
#'
#' @param ds_linear The [data.frame] to containing the simple linear data.  There should be one record per observation.
#' @param ds_periodic The [data.frame] to containing the reoccurring/periodic bands.  There should be one record per observation per stage.  If there are three stages, this [data.frame] should have three times as many rows as `ds_linear`.
#' @param x_name The variable name containing the date.
#' @param y_name The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.  For example, before the first interruption, the `stage_id` is `1`, and is `2` afterwards.
#' @param periodic_lower_name The variable name showing the lower bound of a stage's periodic estimate.
#' @param periodic_upper_name The variable name showing the upper bound of a stage's periodic estimate.
#' @param palette_dark A vector of colors used for the dark/heavy graphical elements.  The vector should have one color for each `stage_id` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param palette_light A vector of colors used for the light graphical elements.  The vector should have one color for each `stage_id` value.  If no vector is specified, a default will be chosen, based on the number of stages.
#' @param change_points A vector of values indicate the interruptions between stages.  It typically works best as a Date or a POSIXct class.
#' @param change_point_labels The text plotted above each interruption.
#' @param draw_periodic_band A boolean value indicating if the bands should be plotted (whose values are take from the `periodic_lower_name` and `periodic_upper_name`.
#' @param jagged_point_size The size of the observed data points.
#' @param jagged_line_size The size of the line connecting the observed data points.
#'
#' @param band_alpha_dark The amount of transparency of the band appropriate for a stage's *x* values.
#' @param band_alpha_light The amount of transparency of the band comparison stages for a given *x* value.
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
#' change_month <- base::as.Date("1996-02-15")
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
#' h_spread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
#' portfolio <- annotate_data(
#'     ds_linear,
#'     dv_name = "birth_rate",
#'     center_function = median,
#'     spread_function = h_spread
#' )
#'
#' cartesian_periodic(
#'   portfolio$ds_linear,
#'   portfolio$ds_periodic,
#'   x_name = "date",
#'   y_name = "birth_rate",
#'   stage_id_name = "stage_id",
#'   change_points = change_month,
#'   change_point_labels = "Bombing Effect"
#' )



cartesian_periodic <- function(ds_linear, ds_periodic,
                              x_name, y_name, stage_id_name,
                              periodic_lower_name="position_lower", periodic_upper_name="position_upper",
                              palette_dark=NULL, palette_light=NULL,
                              change_points=NULL, change_point_labels=NULL,
                              draw_periodic_band=TRUE,
                              jagged_point_size=2, jagged_line_size=.5,
                              band_alpha_dark=.4, band_alpha_light=.15,
                              change_line_alpha=.5, change_line_size=3,
                              title=NULL, x_title=NULL, y_title=NULL ) {

  stages <- base::sort(base::unique(ds_linear[[stage_id_name]]))
  stage_count <- length(stages)
  testit::assert("The number of unique `stage_id` values should be 1 greater than the number of `change_points`.", stage_count==1+length(change_points))
  if (!is.null(change_points)) testit::assert("The number of `change_points` should equal the number of `changeLabels`.", length(change_points)==length(change_point_labels))
  if (!is.null(palette_dark))  testit::assert("The number of `palette_dark` colors should equal the number of unique `stage_id` values.", stage_count==length(palette_dark))
  if (!is.null(palette_light)) testit::assert("The number of `palette_light` colors should equal the number of unique `stage_id` values.", stage_count==length(palette_light))

  p <- ggplot2::ggplot(ds_linear, ggplot2::aes_string(x=x_name, y=y_name))

  if (is.null(palette_dark)) {
    if (length(stages) <= 4L) palette_dark <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(2,4,6,8)] #There's not a risk of defining more colors than levels
    else palette_dark <- colorspace::rainbow_hcl(n=length(stages), l=40)
  }
  if (is.null(palette_light)) {
    if (length(stages) <= 4L) palette_light <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(1,3,5,7)] #There's not a risk of defining more colors than levels
    else palette_light <- colorspace::rainbow_hcl(n=length(stages), l=70)
  }

  for (stage in stages) {
    ds_stage_linear <- ds_linear[stage <= ds_linear$stage_progress & ds_linear$stage_progress <= (stage+1), ]

    if (draw_periodic_band) {
      for (stage_inner in stages) {
        ds_stage_periodic <- ds_periodic[(stage <= ds_periodic$stage_progress) & (ds_periodic$stage_progress <= (stage+1)) & (ds_periodic$stage_id_band == stage_inner), ]
        ribbon_alpha <- ifelse(stage==stage_inner, band_alpha_dark, band_alpha_light)
        #p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin=periodic_lower_name, ymax=periodic_upper_name, y=NULL), data=ds_stage_periodic,
        #                     fill=palette_dark[stage_inner], color=NA, alpha=ribbon_alpha, na.rm=TRUE)

        p <-
          p +
          ggplot2::geom_ribbon(
            ggplot2::aes_string(y=NULL, ymin=periodic_lower_name, ymax=periodic_upper_name),
            data  = ds_stage_periodic,
            fill  = palette_dark[stage_inner],
            color = NA,
            alpha = ribbon_alpha,
            na.rm = TRUE
          )
      }
    }

    p <- p + ggplot2::geom_line(size=jagged_line_size, color=palette_dark[stage], data=ds_stage_linear)
    p <- p + ggplot2::geom_point(shape=1, color=palette_light[stage], data=ds_stage_linear, size=jagged_point_size)
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

# ds_linear <- county_month_birth_rate_2005_version
# ds_linear[ds_linear$county_name=="oklahoma", ]
# ds_linear <- Wats::augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
#
# h_spread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# portfolio <- Wats::annotate_data(ds_linear, dv_name="birth_rate", center_function=median, spread_function=h_spread)
#
# cartesian_periodic(portfolio$ds_linear, portfolio$ds_periodic, x_name="date", y_name="birth_rate", stage_id_name="stage_id", change_points=change_month, change_point_labels="Bombing Effect",
#                    draw_periodic_band=FALSE)
# cartesian_periodic(portfolio$ds_linear, portfolio$ds_periodic, x_name="date", y_name="birth_rate", stage_id_name="stage_id", change_points=change_month, change_point_labels="Bombing Effect")
