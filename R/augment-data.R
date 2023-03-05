#' @name augment_cycle_data
#' @aliases augment_year_data_with_month_resolution augment_year_data_with_second_resolution
#' @export augment_year_data_with_month_resolution augment_year_data_with_second_resolution
#' @usage augment_year_data_with_month_resolution( ds_linear, date_name )
#' augment_year_data_with_second_resolution( ds_linear, date_name )
#'
#' @title Calculates variables necessary for WATS Plots
#'
#' @description Calculates variables necessary for WATS Plots.  This the first of two functions
#' that needs to be called to produce WATS Plots.  [annotate_data()] is the second.
#'
#' @param ds_linear The [data.frame] to containing the detailed data.
#' @param date_name The variable name in `ds_linear` containing the date or datetime value.
# @param stage_id_name The variable name indicating the stage. In a typical interrupted time series, these values are `1` before the interruption and `2` after.
#' @return Returns a [data.frame] with additional variables: `cycle_tally`, `proportion_through_cycle`, `proportion_id`, and `terminal_point_in_cycle`.
#' @examples
#' library(Wats)
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
#' head(ds_linear)
#'
#' @importFrom rlang .data
augment_year_data_with_month_resolution <- function( ds_linear, date_name ) {
  is_min <- NULL # avoid "Undefined global functions or variables"

  year_of_event <- lubridate::year(ds_linear[[date_name]])

  minyear_of_event <- base::min(year_of_event)
  ds_linear$cycle_tally <- (year_of_event - minyear_of_event)
  months_through_the_year <- lubridate::month(ds_linear[[date_name]]) - .5
  months_in_the_year <- 12L
  ds_linear$proportion_through_cycle <- months_through_the_year /  months_in_the_year
  ds_linear$proportion_id <- base::rank(ds_linear$proportion_through_cycle, ties.method="max") / base::max(ds_linear$cycle_tally + 1)
  ds_linear$starting_point_in_cycle <- (ds_linear$proportion_id==base::min(ds_linear$proportion_id))
  ds_linear$terminal_point_in_cycle <- (ds_linear$proportion_id==base::max(ds_linear$proportion_id))

  # SummarizeWithinStage <- function( d ) {
  #   is_min <- (base::min(d[[date_name]]) < d[[date_name]])
  #   return( d$stage_id + is_min*0.5 )
  # }
  #
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

  # ds_linear$stage_progress <- base::unlist(plyr::dlply(ds_linear, "stage_id", SummarizeWithinStage))
  # return( ds_linear )
}
augment_year_data_with_second_resolution <- function( ds_linear, date_name ) {
  is_min <- NULL # avoid "Undefined global functions or variables"

  year_of_event <- lubridate::year(ds_linear[[date_name]])
  first_of_year <- base::ISOdate(year=year_of_event, month=1, day=1, tz="GMT")
  last_of_year <- first_of_year + lubridate::years(1)  #ISOdate(year=year_of_event + 1, month=1, day=1, tz="GMT")

  minyear_of_event <- min(year_of_event)
  ds_linear$cycle_tally <- (year_of_event - minyear_of_event)
  seconds_through_the_year <- base::as.integer(base::difftime(time1=ds_linear[[date_name]], first_of_year, units="sec")) - .5
  seconds_in_the_year <- base::as.integer(base::difftime(last_of_year, first_of_year, units="sec"))
  ds_linear$proportion_through_cycle <- seconds_through_the_year /  seconds_in_the_year

  # SummarizeWithinCycle <- function( d ) {
  #   d$proportion_id <- base::rank(d$proportion_through_cycle, ties.method="max")
  #   d$starting_point_in_cycle <- (d$proportion_id==base::min(d$proportion_id))
  #   d$terminal_point_in_cycle <- (d$proportion_id==base::max(d$proportion_id))
  #   return( d )
  # }
  # ds_linear <- plyr::ddply(ds_linear, .variables="cycle_tally", SummarizeWithinCycle) #base::transform,
#                           proportion_id)

  ds_linear <-
    ds_linear |>
    dplyr::group_by(.data$cycle_tally) |>
    dplyr::mutate(
      proportion_id          = base::rank(.data$proportion_through_cycle, ties.method="max"),
      starting_point_in_cycle  = (.data$proportion_id == base::min(.data$proportion_id)),
      terminal_point_in_cycle  = (.data$proportion_id == base::max(.data$proportion_id)),
    ) |>
    dplyr::ungroup()

  #ds_linear$proportion_id <- as.integer(round(rank(ds_linear$proportion_through_cycle, ties.method="max") / max(ds_linear$cycle_tally + 1)))
#   ds_linear$proportion_id <- rank(ds_linear$proportion_through_cycle, ties.method="max") / max(ds_linear$cycle_tally + 1)
#   ds_linear$starting_point_in_cycle <- (ds_linear$proportion_id==min(ds_linear$proportion_id))
#   ds_linear$terminal_point_in_cycle <- (ds_linear$proportion_id==max(ds_linear$proportion_id))
#   ds_linear <- plyr::ddply(ds_linear,
#                     "cycle_tally",
#                     transform,
#                     terminal_point_in_cycle=(rank(proportion_through_cycle)==max(rank(proportion_through_cycle))))
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
  #   SummarizeWithinStage <- function( d ) {
  #     #     minValue <- min(d[[date_name]])
  #     #     maxValue <- max(d[[date_name]])
  #     #     isBetween <- ( (min(d[[date_name]]) < d[[date_name]]) & (d[[date_name]] < max(d[[date_name]])))
  #     is_min <-  (base::min(d[[date_name]]) < d[[date_name]])
  #     return( d$stage_id + is_min*0.5 )
  #   }
  #   ds_linear$stage_progress <- base::unlist(plyr::dlply(ds_linear, "stage_id", SummarizeWithinStage))
  # #   ds_linear$stage_progress <- plyr::daply(ds_linear, "stage_id", SummarizeWithinStage)
  #   return( ds_linear )
}

# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
# # ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
# ds_linear
#
# ds_linear$date <- as.POSIXct(ds_linear$date, tz="GMT")
# ds_linear <- augment_year_data_with_second_resolution(ds_linear=ds_linear, date_name="date")