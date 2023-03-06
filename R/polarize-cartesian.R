#' @name polarize_cartesian
#' @export
#' @title Manipulate Cartesian data to use in the WATS polar plot
#'
#' @description Three operations are performed.
#' First, within each stage, the first row is repeated at the end, to close the loop.
#' Second, multiple points are interpolated (still in a Cartesian coordinates)
#' so that the polar graph doesn't have sharp edges.
#' These sharp edges would be artifacts of the conversion, and not reflect the observed data.
#' Third, the Cartesian points are converted to polar coordinates.
#'
#' @param ds_linear The [data.frame] to containing the simple linear data.
#' There should be one record per observation.
#' @param ds_stage_cycle The [data.frame] to containing the reoccurring/periodic bands.
#' There should be one record per observation per stage.
#' If there are three stages, this [tibble::tibble] should have three times as many rows as `ds_linear`.
#' @param y_name The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.
#' For example, before the first interruption, the `stage_id` is "1", and is "2" afterwards.
#' @param cycle_tally_name The variable name indicating how many *complete* cycles have occurred at that observation.
#' @param proportion_through_cycle_name The variable name showing how far
#' through a cycle the observation (or summarized observations) occurred.
#' @param periodic_lower_name The variable name showing the lower bound of a stage's periodic estimate.
#' @param periodic_center_name The variable name showing the center estimate of a stage's periodic estimate.
#' @param periodic_upper_name The variable name showing the upper bound of a stage's periodic estimate.
#' @param plotted_point_count_per_cycle The number of points that are plotted per cycle.
#' If the polar graph has 'sharp corners', then increase this value.
#' @param graph_floor The value of the criterion/dependent variable at the center of the polar plot.
#' @return Returns a [tibble::tibble].
#' @keywords polar
#' @examples
#' library(Wats)
#' ds_linear <-
#'   Wats::county_month_birth_rate_2005_version |>
#'   dplyr::filter(county_name == "oklahoma") |>
#'   augment_year_data_with_month_resolution(date_name = "date")
#'
#' h_spread <- function( scores ) { quantile(x = scores, probs = c(.25, .75)) }
#' portfolio <- annotate_data(
#'   ds_linear        = ds_linear,
#'   dv_name          = "birth_rate",
#'   center_function  = median,
#'   spread_function  = h_spread
#' )
#' rm(ds_linear)
#'
#' polarized <- polarize_cartesian(
#'   ds_linear      = portfolio$ds_linear,
#'   ds_stage_cycle = portfolio$ds_stage_cycle,
#'   y_name         = "birth_rate",
#'   stage_id_name  = "stage_id"
#' )
#'
#' library(ggplot2)
#' polarized$ds_stage_cycle_polar |>
#'   ggplot(aes(color = factor(stage_id))) +
#'   geom_path(aes(x = polar_lower_x , y = polar_lower_y ), linetype = 2) +
#'   geom_path(aes(x = polar_center_x, y = polar_center_y), linewidth = 2) +
#'   geom_path(aes(x = polar_upper_x , y = polar_upper_y ), linetype = 2) +
#'   geom_path(aes(x = observed_x    , y = observed_y    ), data = polarized$ds_observed_polar) +
#'   coord_fixed(ratio = 1) +
#'   guides(color = NULL)

# For a more polished graph, see polar_periodic().
polarize_cartesian <- function(
  ds_linear,
  ds_stage_cycle,
  y_name,
  stage_id_name,
  cycle_tally_name                = "cycle_tally",
  proportion_through_cycle_name   = "proportion_through_cycle",
  periodic_lower_name             = "position_lower",
  periodic_center_name            = "PositionCenter",
  periodic_upper_name             = "position_upper",
  plotted_point_count_per_cycle   = 120,
  graph_floor                     = min(base::pretty(ds_linear[[y_name]]))
) {
  # TODO: allow counter-clockwise and arbitrary angle for theta = 0
  . <- NULL # avoid "Undefined global functions or variables"

  close_loop <- function(d) {
    d[nrow(d) + 1, ] <- d[1, ] #Within each stage, repeat the first row at the end of the stage's data.frame.
    d[nrow(d), proportion_through_cycle_name] <- 1 + d[nrow(d), proportion_through_cycle_name]
    d
  }

  interpolate_observed <- function(d, points_per_cycle_count) {
    observed <-
      stats::approx(
        x = d[[cycle_tally_name]] + d[[proportion_through_cycle_name]],
        y = d[[y_name]],
        n = points_per_cycle_count
      )
    stage_progress <-
      stats::approx(
        x = unique(d[[stage_id_name]]) + 0:1,
        n = points_per_cycle_count + 1
      )

    tibble::tibble(
      observed_x     = observed$x,
      observed_y     = observed$y,
      stage_progress = stage_progress$y[seq_len(points_per_cycle_count)] #Which chops off the last value.
    )
  }

  interpolate_band <- function(d, points_per_cycle_count) {
    lower  <- stats::approx(x = d[[proportion_through_cycle_name]], y = d[[periodic_lower_name ]], n = points_per_cycle_count)
    center <- stats::approx(x = d[[proportion_through_cycle_name]], y = d[[periodic_center_name]], n = points_per_cycle_count)
    upper  <- stats::approx(x = d[[proportion_through_cycle_name]], y = d[[periodic_upper_name ]], n = points_per_cycle_count)

    tibble::tibble(
      lower_x  = lower$x,
      lower_y  = lower$y,
      center_x = center$x,
      center_y = center$y,
      upper_x  = upper$x,
      upper_y  = upper$y
    )
  }

  polarize_observed <- function(d, graph_floor = graph_floor) {
    #After R 3.1.0 has been out for a while, consider using sinpi()`.
    if (nrow(d) == 0L) {
      stage_start <- logical(0)
      stage_end   <- logical(0)
    } else {
      stage_start <- c(TRUE, rep(FALSE, times = nrow(d) - 1L))
      stage_end   <- c(rep(FALSE, times = nrow(d) - 1L), TRUE)
    }
    tibble::tibble(
      observed_x          = (d$observed_y - graph_floor) * sin(2 * pi * d$observed_x),
      observed_y          = (d$observed_y - graph_floor) * cos(2 * pi * d$observed_x),
      theta               = pi * 2 * d$observed_x,
      radius              = d$observed_y,
      stage_progress      = d$stage_progress,
      stage_start         = stage_start,
      stage_end           = stage_end,
      label_stage_start   = dplyr::if_else(stage_start, paste0(d$stage_id, "S"), ""),
      label_stage_end     = dplyr::if_else(stage_end  , paste0(d$stage_id, "E"), "")
    )
  }

  polarize_band <- function(d, graph_floor = graph_floor) {
    if (nrow(d) == 0L) {
      stage_start <- logical(0)
      stage_end   <- logical(0)
    } else {
      stage_start <- c(TRUE, rep(FALSE, times = nrow(d) - 1L))
      stage_end   <- c(rep(FALSE, times = nrow(d) - 1L), TRUE)
    }

    tibble::tibble(
      polar_lower_x     = (d$lower_y  - graph_floor) * sin(2 * pi * d$lower_x),
      polar_lower_y     = (d$lower_y  - graph_floor) * cos(2 * pi * d$lower_x),
      polar_center_x    = (d$center_y - graph_floor) * sin(2 * pi * d$center_x),
      polar_center_y    = (d$center_y - graph_floor) * cos(2 * pi * d$center_x),
      polar_upper_x     = (d$upper_y  - graph_floor) * sin(2 * pi * d$upper_x),
      polar_upper_y     = (d$upper_y  - graph_floor) * cos(2 * pi * d$upper_x),
      stage_start       = stage_start,
      stage_end         = stage_end,
      label_stage_start = dplyr::if_else(stage_start, paste0(d$stage_id, "S"), ""),
      label_stage_end   = dplyr::if_else(stage_end  , paste0(d$stage_id, "E"), "")
    )
  }

  ds_observed_interpolated <-
    ds_linear |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      interpolate_observed(., points_per_cycle_count = plotted_point_count_per_cycle)
    ) |>
    dplyr::ungroup()

  ds_observed_polar <-
    ds_observed_interpolated |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      polarize_observed(., graph_floor = graph_floor)
    ) |>
    dplyr::ungroup()

  ds_stage_cycle_closed <-
    ds_stage_cycle |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      close_loop(.)
    ) |>
    dplyr::ungroup()

  ds_stage_cycle_interpolated <-
    ds_stage_cycle_closed |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      interpolate_band(., points_per_cycle_count = plotted_point_count_per_cycle)
    ) |>
    dplyr::ungroup()

  ds_stage_cycle_polar <-
    ds_stage_cycle_interpolated |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      polarize_band(., graph_floor = graph_floor)
    ) |>
    dplyr::ungroup()

  list(
    ds_observed_polar    = ds_observed_polar,
    ds_stage_cycle_polar = ds_stage_cycle_polar,
    graph_floor          = graph_floor
  )
}
