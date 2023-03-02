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
#' @return Returns a [data.frame] with additional variables: `CycleTally`, `ProportionThroughCycle`, `ProportionID`, and `TerminalPointInCycle`.
#' @examples
#' library(Wats)
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$CountyName=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
#' head(ds_linear)
#'
#' @importFrom rlang .data
augment_year_data_with_month_resolution <- function( ds_linear, date_name ) {
  isMin <- NULL # avoid "Undefined global functions or variables"

  yearOfEvent <- lubridate::year(ds_linear[[date_name]])

  minYearOfEvent <- base::min(yearOfEvent)
  ds_linear$CycleTally <- (yearOfEvent - minYearOfEvent)
  monthsThroughTheYear <- lubridate::month(ds_linear[[date_name]]) - .5
  monthsInTheYear <- 12L
  ds_linear$ProportionThroughCycle <- monthsThroughTheYear /  monthsInTheYear
  ds_linear$ProportionID <- base::rank(ds_linear$ProportionThroughCycle, ties.method="max") / base::max(ds_linear$CycleTally + 1)
  ds_linear$StartingPointInCycle <- (ds_linear$ProportionID==base::min(ds_linear$ProportionID))
  ds_linear$TerminalPointInCycle <- (ds_linear$ProportionID==base::max(ds_linear$ProportionID))

  # SummarizeWithinStage <- function( d ) {
  #   isMin <- (base::min(d[[date_name]]) < d[[date_name]])
  #   return( d$StageID + isMin*0.5 )
  # }
  #
  ds_linear |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$StageID) |>
    dplyr::mutate(
      isMin = (base::min(!! rlang::ensym(date_name)) < !! rlang::ensym(date_name)),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      StageProgress = .data$StageID + .data$isMin*0.5,
    ) |>
    dplyr::select(
      -isMin,
    )

  # ds_linear$StageProgress <- base::unlist(plyr::dlply(ds_linear, "StageID", SummarizeWithinStage))
  # return( ds_linear )
}
augment_year_data_with_second_resolution <- function( ds_linear, date_name ) {
  isMin <- NULL # avoid "Undefined global functions or variables"

  yearOfEvent <- lubridate::year(ds_linear[[date_name]])
  firstOfYear <- base::ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT")
  lastOfYear <- firstOfYear + lubridate::years(1)  #ISOdate(year=yearOfEvent + 1, month=1, day=1, tz="GMT")

  minYearOfEvent <- min(yearOfEvent)
  ds_linear$CycleTally <- (yearOfEvent - minYearOfEvent)
  secondsThroughTheYear <- base::as.integer(base::difftime(time1=ds_linear[[date_name]], firstOfYear, units="sec")) - .5
  secondsInTheYear <- base::as.integer(base::difftime(lastOfYear, firstOfYear, units="sec"))
  ds_linear$ProportionThroughCycle <- secondsThroughTheYear /  secondsInTheYear

  # SummarizeWithinCycle <- function( d ) {
  #   d$ProportionID <- base::rank(d$ProportionThroughCycle, ties.method="max")
  #   d$StartingPointInCycle <- (d$ProportionID==base::min(d$ProportionID))
  #   d$TerminalPointInCycle <- (d$ProportionID==base::max(d$ProportionID))
  #   return( d )
  # }
  # ds_linear <- plyr::ddply(ds_linear, .variables="CycleTally", SummarizeWithinCycle) #base::transform,
#                           ProportionID)

  ds_linear <-
    ds_linear |>
    dplyr::group_by(.data$CycleTally) |>
    dplyr::mutate(
      ProportionID          = base::rank(.data$ProportionThroughCycle, ties.method="max"),
      StartingPointInCycle  = (.data$ProportionID == base::min(.data$ProportionID)),
      TerminalPointInCycle  = (.data$ProportionID == base::max(.data$ProportionID)),
    ) |>
    dplyr::ungroup()

  #ds_linear$ProportionID <- as.integer(round(rank(ds_linear$ProportionThroughCycle, ties.method="max") / max(ds_linear$CycleTally + 1)))
#   ds_linear$ProportionID <- rank(ds_linear$ProportionThroughCycle, ties.method="max") / max(ds_linear$CycleTally + 1)
#   ds_linear$StartingPointInCycle <- (ds_linear$ProportionID==min(ds_linear$ProportionID))
#   ds_linear$TerminalPointInCycle <- (ds_linear$ProportionID==max(ds_linear$ProportionID))
#   ds_linear <- plyr::ddply(ds_linear,
#                     "CycleTally",
#                     transform,
#                     TerminalPointInCycle=(rank(ProportionThroughCycle)==max(rank(ProportionThroughCycle))))
  ds_linear |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$StageID) |>
    dplyr::mutate(
      isMin = (base::min(!! rlang::ensym(date_name)) < !! rlang::ensym(date_name)),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      StageProgress = .data$StageID + .data$isMin*0.5,
    ) |>
    dplyr::select(
      -isMin,
    )
  #   SummarizeWithinStage <- function( d ) {
  #     #     minValue <- min(d[[date_name]])
  #     #     maxValue <- max(d[[date_name]])
  #     #     isBetween <- ( (min(d[[date_name]]) < d[[date_name]]) & (d[[date_name]] < max(d[[date_name]])))
  #     isMin <-  (base::min(d[[date_name]]) < d[[date_name]])
  #     return( d$StageID + isMin*0.5 )
  #   }
  #   ds_linear$StageProgress <- base::unlist(plyr::dlply(ds_linear, "StageID", SummarizeWithinStage))
  # #   ds_linear$StageProgress <- plyr::daply(ds_linear, "StageID", SummarizeWithinStage)
  #   return( ds_linear )
}

# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$CountyName=="oklahoma", ]
# # ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
# ds_linear
#
# ds_linear$Date <- as.POSIXct(ds_linear$Date, tz="GMT")
# ds_linear <- augment_year_data_with_second_resolution(ds_linear=ds_linear, date_name="Date")
