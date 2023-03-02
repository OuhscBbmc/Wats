#' @name augment_cycle_data
#' @aliases augment_year_data_with_month_resolution augment_year_data_with_second_resolution
#' @export augment_year_data_with_month_resolution augment_year_data_with_second_resolution
#' @usage augment_year_data_with_month_resolution( dsLinear, dateName )
#' augment_year_data_with_second_resolution( dsLinear, dateName )
#'
#' @title Calculates variables necessary for WATS Plots
#'
#' @description Calculates variables necessary for WATS Plots.  This the first of two functions
#' that needs to be called to produce WATS Plots.  [annotate_data()] is the second.
#'
#' @param dsLinear The [data.frame] to containing the detailed data.
#' @param dateName The variable name in `dsLinear` containing the date or datetime value.
# @param stageIDName The variable name indicating the stage. In a typical interrupted time series, these values are `1` before the interruption and `2` after.
#' @return Returns a [data.frame] with additional variables: `CycleTally`, `ProportionThroughCycle`, `ProportionID`, and `TerminalPointInCycle`.
#' @examples
#' library(Wats)
#' dsLinear <- county_month_birth_rate_2005_version
#' dsLinear <- dsLinear[dsLinear$CountyName=="oklahoma", ]
#' dsLinear <- augment_year_data_with_month_resolution(dsLinear=dsLinear, dateName="Date")
#' head(dsLinear)
#'
#' @importFrom rlang .data
augment_year_data_with_month_resolution <- function( dsLinear, dateName ) {
  isMin <- NULL # avoid "Undefined global functions or variables"

  yearOfEvent <- lubridate::year(dsLinear[[dateName]])

  minYearOfEvent <- base::min(yearOfEvent)
  dsLinear$CycleTally <- (yearOfEvent - minYearOfEvent)
  monthsThroughTheYear <- lubridate::month(dsLinear[[dateName]]) - .5
  monthsInTheYear <- 12L
  dsLinear$ProportionThroughCycle <- monthsThroughTheYear /  monthsInTheYear
  dsLinear$ProportionID <- base::rank(dsLinear$ProportionThroughCycle, ties.method="max") / base::max(dsLinear$CycleTally + 1)
  dsLinear$StartingPointInCycle <- (dsLinear$ProportionID==base::min(dsLinear$ProportionID))
  dsLinear$TerminalPointInCycle <- (dsLinear$ProportionID==base::max(dsLinear$ProportionID))

  # SummarizeWithinStage <- function( d ) {
  #   isMin <- (base::min(d[[dateName]]) < d[[dateName]])
  #   return( d$StageID + isMin*0.5 )
  # }
  #
  dsLinear |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$StageID) |>
    dplyr::mutate(
      isMin = (base::min(!! rlang::ensym(dateName)) < !! rlang::ensym(dateName)),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      StageProgress = .data$StageID + .data$isMin*0.5,
    ) |>
    dplyr::select(
      -isMin,
    )

  # dsLinear$StageProgress <- base::unlist(plyr::dlply(dsLinear, "StageID", SummarizeWithinStage))
  # return( dsLinear )
}
augment_year_data_with_second_resolution <- function( dsLinear, dateName ) {
  isMin <- NULL # avoid "Undefined global functions or variables"

  yearOfEvent <- lubridate::year(dsLinear[[dateName]])
  firstOfYear <- base::ISOdate(year=yearOfEvent, month=1, day=1, tz="GMT")
  lastOfYear <- firstOfYear + lubridate::years(1)  #ISOdate(year=yearOfEvent + 1, month=1, day=1, tz="GMT")

  minYearOfEvent <- min(yearOfEvent)
  dsLinear$CycleTally <- (yearOfEvent - minYearOfEvent)
  secondsThroughTheYear <- base::as.integer(base::difftime(time1=dsLinear[[dateName]], firstOfYear, units="sec")) - .5
  secondsInTheYear <- base::as.integer(base::difftime(lastOfYear, firstOfYear, units="sec"))
  dsLinear$ProportionThroughCycle <- secondsThroughTheYear /  secondsInTheYear

  # SummarizeWithinCycle <- function( d ) {
  #   d$ProportionID <- base::rank(d$ProportionThroughCycle, ties.method="max")
  #   d$StartingPointInCycle <- (d$ProportionID==base::min(d$ProportionID))
  #   d$TerminalPointInCycle <- (d$ProportionID==base::max(d$ProportionID))
  #   return( d )
  # }
  # dsLinear <- plyr::ddply(dsLinear, .variables="CycleTally", SummarizeWithinCycle) #base::transform,
#                           ProportionID)

  dsLinear <-
    dsLinear |>
    dplyr::group_by(.data$CycleTally) |>
    dplyr::mutate(
      ProportionID          = base::rank(.data$ProportionThroughCycle, ties.method="max"),
      StartingPointInCycle  = (.data$ProportionID == base::min(.data$ProportionID)),
      TerminalPointInCycle  = (.data$ProportionID == base::max(.data$ProportionID)),
    ) |>
    dplyr::ungroup()

  #dsLinear$ProportionID <- as.integer(round(rank(dsLinear$ProportionThroughCycle, ties.method="max") / max(dsLinear$CycleTally + 1)))
#   dsLinear$ProportionID <- rank(dsLinear$ProportionThroughCycle, ties.method="max") / max(dsLinear$CycleTally + 1)
#   dsLinear$StartingPointInCycle <- (dsLinear$ProportionID==min(dsLinear$ProportionID))
#   dsLinear$TerminalPointInCycle <- (dsLinear$ProportionID==max(dsLinear$ProportionID))
#   dsLinear <- plyr::ddply(dsLinear,
#                     "CycleTally",
#                     transform,
#                     TerminalPointInCycle=(rank(ProportionThroughCycle)==max(rank(ProportionThroughCycle))))
  dsLinear |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$StageID) |>
    dplyr::mutate(
      isMin = (base::min(!! rlang::ensym(dateName)) < !! rlang::ensym(dateName)),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      StageProgress = .data$StageID + .data$isMin*0.5,
    ) |>
    dplyr::select(
      -isMin,
    )
  #   SummarizeWithinStage <- function( d ) {
  #     #     minValue <- min(d[[dateName]])
  #     #     maxValue <- max(d[[dateName]])
  #     #     isBetween <- ( (min(d[[dateName]]) < d[[dateName]]) & (d[[dateName]] < max(d[[dateName]])))
  #     isMin <-  (base::min(d[[dateName]]) < d[[dateName]])
  #     return( d$StageID + isMin*0.5 )
  #   }
  #   dsLinear$StageProgress <- base::unlist(plyr::dlply(dsLinear, "StageID", SummarizeWithinStage))
  # #   dsLinear$StageProgress <- plyr::daply(dsLinear, "StageID", SummarizeWithinStage)
  #   return( dsLinear )
}

# library(Wats)
# dsLinear <- county_month_birth_rate_2005_version
# dsLinear <- dsLinear[dsLinear$CountyName=="oklahoma", ]
# # dsLinear <- augment_year_data_with_month_resolution(dsLinear=dsLinear, dateName="Date")
# dsLinear
#
# dsLinear$Date <- as.POSIXct(dsLinear$Date, tz="GMT")
# dsLinear <- augment_year_data_with_second_resolution(dsLinear=dsLinear, dateName="Date")
