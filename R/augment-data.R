#' @name augment_cycle_data
#' @aliases augment_year_data_with_month_resolution augment_year_data_with_second_resolution
#' @export augment_year_data_with_month_resolution augment_year_data_with_second_resolution
#' @usage augment_year_data_with_month_resolution(ds_linear, date_name)
#' augment_year_data_with_second_resolution(ds_linear, date_name)
#'
#' @title Calculates variables necessary for WATS Plots
#'
#' @description Calculates variables necessary for WATS Plots.  This the first of two functions
#' that needs to be called to produce WATS Plots.  [annotate_data()] is the second.
#'
#' @param ds_linear The [data.frame] to containing the detailed data.
#' @param date_name The variable name in `ds_linear` containing the date or datetime value.
# @param stage_id_name The variable name indicating the stage. In a typical interrupted time series, these values are "1" before the interruption and "2" after.
#' @return Returns a [tibble::tibble] with additional variables:
#' `cycle_tally`, `proportion_through_cycle`, `proportion_id`, and `terminal_point_in_cycle`.
#' @examples
#' library(Wats)
#' ds_linear <-
#'   Wats::county_month_birth_rate_2005_version |>
#'   dplyr::filter(county_name == "oklahoma") |>
#'   augment_year_data_with_month_resolution(date_name = "date")
#'
#' head(ds_linear)
#'
#' @importFrom rlang .data
augment_year_data_with_month_resolution <- function(ds_linear, date_name) {
  is_min <- NULL # avoid "Undefined global functions or variables"

  year_of_event <- lubridate::year(ds_linear[[date_name]])

  min_year_of_event <- base::min(year_of_event)
  ds_linear$cycle_tally <- (year_of_event - min_year_of_event)
  months_through_the_year <- lubridate::month(ds_linear[[date_name]]) - .5
  months_in_the_year <- 12L
  ds_linear$proportion_through_cycle <- months_through_the_year /  months_in_the_year
  ds_linear$proportion_id <- base::rank(ds_linear$proportion_through_cycle, ties.method="max") / base::max(ds_linear$cycle_tally + 1)
  ds_linear$starting_point_in_cycle <- (ds_linear$proportion_id==base::min(ds_linear$proportion_id))
  ds_linear$terminal_point_in_cycle <- (ds_linear$proportion_id==base::max(ds_linear$proportion_id))

  ds_linear |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$stage_id) |>
    dplyr::mutate(
      is_min = (base::min(!! rlang::ensym(date_name)) < !! rlang::ensym(date_name)),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      stage_progress = .data$stage_id + .data$is_min*0.5,
    ) |>
    dplyr::select(
      -is_min,
    )
}
augment_year_data_with_second_resolution <- function(ds_linear, date_name) {
  is_min <- NULL # avoid "Undefined global functions or variables"

  year_of_event <- lubridate::year(ds_linear[[date_name]])
  first_of_year <- base::ISOdate(year = year_of_event, month = 1, day = 1, tz = "GMT")
  last_of_year <- first_of_year + lubridate::years(1)

  min_year_of_event <- min(year_of_event)
  ds_linear$cycle_tally <- (year_of_event - min_year_of_event)
  seconds_through_the_year <- base::as.integer(base::difftime(time1 = ds_linear[[date_name]], first_of_year, units="sec")) - .5
  seconds_in_the_year <- base::as.integer(base::difftime(last_of_year, first_of_year, units="sec"))
  ds_linear$proportion_through_cycle <- seconds_through_the_year /  seconds_in_the_year

  ds_linear <-
    ds_linear |>
    dplyr::group_by(.data$cycle_tally) |>
    dplyr::mutate(
      proportion_id          = base::rank(.data$proportion_through_cycle, ties.method="max"),
      starting_point_in_cycle  = (.data$proportion_id == base::min(.data$proportion_id)),
      terminal_point_in_cycle  = (.data$proportion_id == base::max(.data$proportion_id)),
    ) |>
    dplyr::ungroup()

  ds_linear |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$stage_id) |>
    dplyr::mutate(
      is_min = (base::min(!! rlang::ensym(date_name)) < !! rlang::ensym(date_name)),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      stage_progress = .data$stage_id + .data$is_min*0.5,
    ) |>
    dplyr::select(
      -is_min,
    )
}
