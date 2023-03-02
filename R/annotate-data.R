#' @name annotate_data
#' @export annotate_data
#' @title Finds midpoints and bands for the within and between cycles.
#'
#' @description Finds midpoints and bands for the within and between cycles.  This the second of two functions
#' that needs to be called to produce WATS Plots.  `AugmentZZZ` is the first.
#'
#' @param ds_linear The [data.frame] to containing the detailed data.
#' @param dv_name The name of the dependent/criterion variable.
#' @param center_function A function to calculate the center of a subsample.
#' @param spread_function A function to calculate the bands of a subsample.
#' @param cycle_tally_name The variable name indicating how many cycles have been completed.
#' @param stage_id_name The variable name indicating the stage. In a typical interrupted time series, these values are \code{1} before the interruption and \code{2} after.
#' @param stage_progress_name The variable name indicating the stage in a decimal form.  This is mostly for internal uses.
#' @param proportion_through_cycle_name The variable name indicating how far the point is through a cycle.  For example, 0 degrees would be \code{0}, 180 degrees would be \code{0.5}, 359 degrees would be \code{0.9972}, and 360 degrees would be \code{0}.
#' @param proportion_id_name The variable name indicating the ordinal position through a cycle.
#' @param terminal_point_in_cycle_name The variable name indicating the last point within a given cycle.
#' @return Returns a `data.frame` with additional variables <<Say what they are>>.
#' @examples
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
#'
#' head(portfolio$ds_stage_cycle)
#' head(portfolio$ds_linear)
#' head(portfolio$ds_periodic)

#' @importFrom rlang .data
annotate_data <- function( ds_linear,
                          dv_name,
                          center_function,
                          spread_function,
                          cycle_tally_name="CycleTally",
                          stage_id_name="StageID",
                          stage_progress_name="StageProgress",
                          proportion_through_cycle_name="ProportionThroughCycle",
                          proportion_id_name="ProportionID",
                          terminal_point_in_cycle_name="TerminalPointInCycle" ) {

  pointsInCycle <- max(ds_linear[[proportion_id_name]])
  testit::assert("The should be at least one point in a cycle", max(pointsInCycle)>=1)

  z <- zoo::zooreg(data=ds_linear[[dv_name]], frequency=pointsInCycle)
  rollingBounds <- zoo::rollapply(data=z, width=pointsInCycle, FUN=spread_function)

  ds_linear$RollingLower <- NA
  ds_linear$RollingCenter <- NA
  ds_linear$RollingUpper <- NA
  ds_linear$RollingLower[-seq_len(pointsInCycle-1) ] <- rollingBounds[, 1]
  ds_linear$RollingCenter[-seq_len(pointsInCycle-1) ] <- zoo::rollapply(data=z, width=pointsInCycle, FUN=center_function)
  ds_linear$RollingUpper[-seq_len(pointsInCycle-1) ] <- rollingBounds[, 2]

  # summarizeStageCycle <- function( d ) {
  #   positionBounds <- spread_function(d[[dv_name]])
  #   #   print(positionBounds)
  #   data.frame(
  #     ProportionThroughCycle = mean(d$ProportionThroughCycle, na.rm=TRUE),
  #     PositionLower = positionBounds[1],
  #     PositionCenter = center_function(d[[dv_name]]),
  #     PositionUpper = positionBounds[2]
  #   )
  # }
  # ds_stage_cycle2 <- plyr::ddply(ds_linear, .variables=c(stage_id_name, proportion_id_name), .fun=summarizeStageCycle)

  ds_stage_cycle <-
    ds_linear |>
    dplyr::group_by(!! rlang::ensym(stage_id_name), !! rlang::ensym(proportion_id_name)) |>
    dplyr::summarize(
      ProportionThroughCycle  = mean(.data$ProportionThroughCycle, na.rm = TRUE),
      PositionLower           = spread_function(!! rlang::ensym(dv_name))[1],
      PositionCenter          = center_function(!! rlang::ensym(dv_name)),
      PositionUpper           = spread_function(!! rlang::ensym(dv_name))[2],
    ) |>
    dplyr::ungroup()

  dsLinearTemp <- ds_linear[, c("Date", stage_id_name, proportion_id_name, stage_progress_name)]
  colnames(dsLinearTemp)[colnames(dsLinearTemp)==stage_id_name] <- "StageIDTime" #Make sure `StageIDTime` matches the two calls below.

  ds_stage_cycleTemp <- ds_stage_cycle
  colnames(ds_stage_cycleTemp)[colnames(ds_stage_cycleTemp)==stage_id_name] <- "StageIDBand" #Make sure `StageIDBand` matches the calls below.

  # dsPeriodic2 <- merge(x=dsLinearTemp, y=ds_stage_cycleTemp, by=c(proportion_id_name), all.x=TRUE, all.y=TRUE)
  ds_periodic <-
    dsLinearTemp |>
    dplyr::left_join(ds_stage_cycleTemp, by=proportion_id_name, multiple = "all") |>
    dplyr::arrange(.data$Date, .data$StageIDTime, .data$StageIDBand)

  # ds_periodic <- ds_periodic[order(ds_periodic$Date, ds_periodic$StageIDTime, ds_periodic$StageIDBand), ]

  return( list(ds_linear=ds_linear, ds_stage_cycle=ds_stage_cycle, ds_periodic=ds_periodic) )
}

# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$CountyName=="oklahoma", ]
# ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
#
# hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# portfolio <- annotate_data(ds_linear, dv_name="BirthRate", center_function=median, spread_function=hSpread)
#
# head(portfolio$ds_stage_cycle)
# head(portfolio$ds_linear)
# head(portfolio$ds_periodic)
#
# portfolio <- annotate_data(ds_linear, dv_name="BirthRate", center_function=mean, spread_function=hSpread)
#
# head(portfolio$ds_stage_cycle)
# head(portfolio$ds_linear)
# head(portfolio$ds_periodic)
