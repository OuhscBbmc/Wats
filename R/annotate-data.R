#' @name annotate_data
#' @export annotate_data
#' @title Finds midpoints and bands for the within and between cycles.
#'
#' @description Finds midpoints and bands for the within and between cycles.
#' This the second of two functions
#' that needs to be called to produce WATS Plots.  `AugmentZZZ` is the first.
#'
#' @param ds_linear The [data.frame] to containing the detailed data.
#' @param dv_name The name of the dependent/criterion variable.
#' @param center_function A function to calculate the center of a subsample.
#' @param spread_function A function to calculate the bands of a subsample.
#' @param cycle_tally_name The variable name indicating how many cycles have been completed.
#' @param stage_id_name The variable name indicating the stage.
#' In a typical interrupted time series, these values are
#' `1` before the interruption and `2` after.
#' @param stage_progress_name The variable name indicating the stage in a decimal form.
#' This is mostly for internal uses.
#' @param proportion_through_cycle_name The variable name indicating how far the point
#' is through a cycle.  For example, 0 degrees would be `0`,
#' 180 degrees would be `0.5`, 359 degrees would be `0.9972`, and
#' 360 degrees would be `0`.
#' @param proportion_id_name The variable name indicating the ordinal position through a cycle.
#' @param terminal_point_in_cycle_name The variable name indicating the last point
#' within a given cycle.
#' @return Returns a [tibble::tibble()] with additional variables.  TODO: say what the variables are.
#' @examples
#' library(Wats)
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear = ds_linear, date_name="date")
#'
#' h_spread <- function( scores ) { return( quantile(x = scores, probs = c(.25, .75)) ) }
#' portfolio <- annotate_data(
#'   ds_linear = ds_linear,
#'   dv_name = "birth_rate",
#'   center_function = median,
#'   spread_function = h_spread
#' )
#'
#' head(portfolio$ds_stage_cycle)
#' head(portfolio$ds_linear)
#' head(portfolio$ds_periodic)

#' @importFrom rlang .data
annotate_data <- function(
  ds_linear,
  dv_name,
  center_function,
  spread_function,
  cycle_tally_name               = "cycle_tally",
  stage_id_name                  = "stage_id",
  stage_progress_name            = "stage_progress",
  proportion_through_cycle_name  = "proportion_through_cycle",
  proportion_id_name             = "proportion_id",
  terminal_point_in_cycle_name   = "terminal_point_in_cycle"
) {

  points_in_cycle <- max(ds_linear[[proportion_id_name]])
  testit::assert("The should be at least one point in a cycle", max(points_in_cycle)>=1)

  z <- zoo::zooreg(data = ds_linear[[dv_name]], frequency = points_in_cycle)
  rolling_bounds <- zoo::rollapply(data = z, width = points_in_cycle, FUN = spread_function)

  ds_linear$rolling_lower  <- NA
  ds_linear$rolling_center <- NA
  ds_linear$rolling_upper  <- NA
  ds_linear$rolling_lower[ -seq_len(points_in_cycle-1) ] <- rolling_bounds[, 1]
  ds_linear$rolling_center[-seq_len(points_in_cycle-1) ] <- zoo::rollapply(data = z, width = points_in_cycle, FUN = center_function)
  ds_linear$rolling_upper[ -seq_len(points_in_cycle-1) ] <- rolling_bounds[, 2]

  ds_stage_cycle <-
    ds_linear |>
    dplyr::group_by(!! rlang::ensym(stage_id_name), !! rlang::ensym(proportion_id_name)) |>
    dplyr::summarize(
      proportion_through_cycle  = mean(.data$proportion_through_cycle, na.rm = TRUE),
      position_lower            = spread_function(!! rlang::ensym(dv_name))[1],
      PositionCenter            = center_function(!! rlang::ensym(dv_name)),
      position_upper            = spread_function(!! rlang::ensym(dv_name))[2],
    ) |>
    dplyr::ungroup()

  ds_linear_temp <- ds_linear[, c("date", stage_id_name, proportion_id_name, stage_progress_name)]
  colnames(ds_linear_temp)[colnames(ds_linear_temp)==stage_id_name] <- "stage_id_time" #Make sure `stage_id_time` matches the two calls below.

  ds_stage_cycle_temp <- ds_stage_cycle
  colnames(ds_stage_cycle_temp)[colnames(ds_stage_cycle_temp)==stage_id_name] <- "stage_id_band" #Make sure `stage_id_band` matches the calls below.

  ds_periodic <-
    ds_linear_temp |>
    dplyr::left_join(ds_stage_cycle_temp, by = proportion_id_name, multiple = "all") |>
    dplyr::arrange(.data$date, .data$stage_id_time, .data$stage_id_band)

  # ds_periodic <- ds_periodic[order(ds_periodic$date, ds_periodic$stage_id_time, ds_periodic$stage_id_band), ]

  return( list(ds_linear = ds_linear, ds_stage_cycle = ds_stage_cycle, ds_periodic = ds_periodic) )
}

# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
# ds_linear <- augment_year_data_with_month_resolution(ds_linear = ds_linear, date_name="date")
#
# h_spread <- function( scores ) { return( quantile(x = scores, probs = c(.25, .75)) ) }
# portfolio <- annotate_data(ds_linear, dv_name="birth_rate", center_function = median, spread_function = h_spread)
#
# head(portfolio$ds_stage_cycle)
# head(portfolio$ds_linear)
# head(portfolio$ds_periodic)
#
# portfolio <- annotate_data(ds_linear, dv_name="birth_rate", center_function = mean, spread_function = h_spread)
#
# head(portfolio$ds_stage_cycle)
# head(portfolio$ds_linear)
# head(portfolio$ds_periodic)
