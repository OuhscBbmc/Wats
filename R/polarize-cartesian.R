#' @name polarize_cartesian
#' @export
#' @title Manipulate Cartesian data to use in the WATS polar plot
#'
#' @description Three operations are performed.
#' First, within each stage, the first row is repeated at the end, to close the loop.
#' Second, multiple points are interpolated (still in a Cartesian coordinates) so that the polar graph doesn't have sharp edges.  These sharp edges would be artifacts of the conversion, and not reflect the observed data.
#' Third, the Cartesian points are converted to polar coordinates.
#'
#' @param ds_linear The [data.frame] to containing the simple linear data.  There should be one record per observation.
#' @param ds_stage_cycle The [data.frame] to containing the reoccurring/periodic bands.  There should be one record per observation per stage.  If there are three stages, this [data.frame] should have three times as many rows as `ds_linear`.
#' @param y_name The variable name containing the dependent/criterion variable.
#' @param stage_id_name The variable name indicating which stage the record belongs to.  For example, before the first interruption, the `stage_id` is `1`, and is `2` afterwards.
#' @param cycle_tally_name The variable name indicating how many \emph{complete} cycles have occurred at that observation.
#' @param proportion_through_cycle_name The variable name showing how far through a cycle the observation (or summarized observations) occurred.
#' @param periodic_lower_name The variable name showing the lower bound of a stage's periodic estimate.
#' @param periodic_center_name The variable name showing the center estimate of a stage's periodic estimate.
#' @param periodic_upper_name The variable name showing the upper bound of a stage's periodic estimate.
#' @param plotted_point_count_per_cycle The number of points that are plotted per cycle.  If the polar graph has 'sharp corners', then increase this value.
#' @param graph_floor The value of the criterion/dependent variable at the center of the polar plot.
#' @return Returns a [data.frame].
#' @keywords polar
#' @examples
#' library(Wats)
#' ds_linear <- county_month_birth_rate_2005_version
#' ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
#' ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
#'
#' hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
#' portfolio <- annotate_data(
#'   ds_linear = ds_linear,
#'   dv_name = "birth_rate",
#'   center_function = median,
#'   spread_function = hSpread
#' )
#' rm(ds_linear)
#'
#' polarized <- polarize_cartesian(
#'   ds_linear = portfolio$ds_linear,
#'   ds_stage_cycle = portfolio$ds_stage_cycle,
#'   y_name = "birth_rate",
#'   stage_id_name = "stage_id"
#' )
#'
#' library(ggplot2)
#' polarized$ds_stage_cycle_polar |>
#'   ggplot(aes(color=factor(stage_id))) +
#'   geom_path(aes(x=PolarLowerX, y=PolarLowerY), linetype=2) +
#'   geom_path(aes(x=PolarCenterX, y=PolarCenterY), linewidth=2) +
#'   geom_path(aes(x=PolarUpperX, y=PolarUpperY), linetype=2) +
#'   geom_path(aes(x=ObservedX, y=ObservedY), data=polarized$dsObservedPolar) +
#'   coord_fixed(ratio=1) +
#'   guides(color=NULL)

#For a more polished graph, see polar_periodic().
polarize_cartesian <- function(ds_linear, ds_stage_cycle,
                      y_name, stage_id_name,
                      cycle_tally_name="CycleTally",
                      proportion_through_cycle_name="ProportionThroughCycle",
                      periodic_lower_name="PositionLower", periodic_center_name="PositionCenter", periodic_upper_name="PositionUpper",
                      plotted_point_count_per_cycle=120,
                      graph_floor=min(base::pretty(x=ds_linear[[y_name]]))) {
  #TODO: allow counter-clockwise and arbitrary angle for theta=0
#
  . <- NULL # avoid "Undefined global functions or variables"
#   print(ds_linear[[cycle_tally_name]])
#   print(ds_linear[[proportion_through_cycle_name]])
#   print(ds_linear[[y_name]])

  closeLoop <- function(d) {
    d[nrow(d) + 1, ] <- d[1, ] #Within each stage, repeat the first row at the end of the stage's data.frame.
    d[nrow(d), proportion_through_cycle_name] <- 1 + d[nrow(d), proportion_through_cycle_name]
    return( d )
  }
  interpolateObserved <- function(d, pointsPerCycleCount) {
    observed <-
      stats::approx(
        x = d[[cycle_tally_name]] + d[[proportion_through_cycle_name]],
        y = d[[y_name]],
        n = pointsPerCycleCount
      )
    stageProgress <-
      stats::approx(
        x = unique(d[[stage_id_name]]) + 0:1,
        n = pointsPerCycleCount + 1
      )
    # browser()
    base::data.frame(
      ObservedX = observed$x,
      ObservedY = observed$y,
      StageProgress = stageProgress$y[seq_len(pointsPerCycleCount)] #Which chops off the last value.
    )
  }
  interpolateBand <- function(d, pointsPerCycleCount) {
    lower <- stats::approx(x=d[[proportion_through_cycle_name]], y=d[[periodic_lower_name]], n=pointsPerCycleCount)
    center <- stats::approx(x=d[[proportion_through_cycle_name]], y=d[[periodic_center_name]], n=pointsPerCycleCount)
    upper <- stats::approx(x=d[[proportion_through_cycle_name]], y=d[[periodic_upper_name]], n=pointsPerCycleCount)

    base::data.frame(
      LowerX = lower$x,
      LowerY = lower$y,
      CenterX = center$x,
      CenterY = center$y,
      UpperX = upper$x,
      UpperY = upper$y
    )
  }
  polarizeObserved <- function(d, graph_floor=graph_floor) {
    #After R 3.1.0 has been out for a while, consider using sinpi()`.
    if (nrow(d) == 0L) {
      stageStart <- logical(0)
      stageEnd <- logical(0)
    } else {
      stageStart <- c(TRUE, rep(FALSE, times=nrow(d)-1))
      stageEnd <- c(rep(FALSE, times=nrow(d)-1), TRUE)
    }
    base::data.frame(
      ObservedX = (d$ObservedY - graph_floor) * sin(2 * pi * d$ObservedX),
      ObservedY = (d$ObservedY - graph_floor) * cos(2 * pi * d$ObservedX),
      Theta = pi * 2 * d$ObservedX,
      Radius = d$ObservedY,
      StageProgress = d$StageProgress,
      StageStart = stageStart,
      StageEnd = stageEnd,
      LabelStageStart = ifelse(stageStart, paste0(d$stage_id, "S"), ""),
      LabelStageEnd = ifelse(stageEnd, paste0(d$stage_id, "E"), ""),
      stringsAsFactors = FALSE
    )
  }
  polarizeBand <- function(d, graph_floor = graph_floor) {
    if (nrow(d) == 0L) {
      stageStart <- logical(0)
      stageEnd <- logical(0)
    } else {
      stageStart <- c(TRUE, rep(FALSE, times=nrow(d)-1))
      stageEnd <- c(rep(FALSE, times=nrow(d)-1), TRUE)
    }

    base::data.frame(
      PolarLowerX = (d$LowerY - graph_floor) * sin(2 * pi * d$LowerX),
      PolarLowerY = (d$LowerY - graph_floor) * cos(2 * pi * d$LowerX),
      PolarCenterX = (d$CenterY - graph_floor) * sin(2 * pi * d$CenterX),
      PolarCenterY = (d$CenterY - graph_floor) * cos(2 * pi * d$CenterX),
      PolarUpperX = (d$UpperY - graph_floor) * sin(2 * pi * d$UpperX),
      PolarUpperY = (d$UpperY - graph_floor) * cos(2 * pi * d$UpperX),
#       StageProgress = d$StageProgress,
      StageStart = stageStart,
      StageEnd = stageEnd,
      LabelStageStart = ifelse(stageStart, paste0(d$stage_id, "S"), ""),
      LabelStageEnd = ifelse(stageEnd, paste0(d$stage_id, "E"), ""),
      stringsAsFactors = FALSE
    )
  }

  dsObservedInterpolated <-
    ds_linear |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      interpolateObserved(., pointsPerCycleCount=plotted_point_count_per_cycle)
    ) |>
    dplyr::ungroup()

  dsObservedPolar <-
    dsObservedInterpolated |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      polarizeObserved(., graph_floor=graph_floor)
    ) |>
    dplyr::ungroup()

  ds_stage_cycleClosed <-
    ds_stage_cycle |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      closeLoop(.)
    ) |>
    dplyr::ungroup()

  ds_stage_cycleInterpolated <-
    ds_stage_cycleClosed |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      interpolateBand(., pointsPerCycleCount = plotted_point_count_per_cycle)
    ) |>
    dplyr::ungroup()

  ds_stage_cycle_polar <-
    ds_stage_cycleInterpolated |>
    dplyr::group_by(!! rlang::ensym(stage_id_name)) |>
    dplyr::do(
      polarizeBand(., graph_floor = graph_floor)
    ) |>
    dplyr::ungroup()

  # dsObservedInterpolated <- plyr::ddply(ds_linear, .variables=stage_id_name, .fun=interpolateObserved, pointsPerCycleCount=plotted_point_count_per_cycle)
  # dsObservedPolar <- plyr::ddply(dsObservedInterpolated, .variables=stage_id_name, .fun=polarizeObserved, graph_floor=graph_floor)
  #
  # ds_stage_cycleClosed <- plyr::ddply(ds_stage_cycle, .variables=stage_id_name, .fun=closeLoop)
  # ds_stage_cycleInterpolated <- plyr::ddply(ds_stage_cycleClosed, .variables=stage_id_name, .fun=interpolateBand, pointsPerCycleCount=plotted_point_count_per_cycle)
  # ds_stage_cycle_polar <- plyr::ddply(ds_stage_cycleInterpolated, .variables=stage_id_name, .fun=polarizeBand, graph_floor=graph_floor)

  return( list(dsObservedPolar=dsObservedPolar, ds_stage_cycle_polar=ds_stage_cycle_polar, graph_floor=graph_floor) )
}

# library(Wats)
# ds_linear <- county_month_birth_rate_2005_version
# ds_linear <- ds_linear[ds_linear$county_name=="oklahoma", ]
# ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
#
# hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# portfolio <- annotate_data(ds_linear, dv_name="birth_rate", center_function=median, spread_function=hSpread)
# rm(ds_linear)
#
# polarized <- polarize_cartesian(portfolio$ds_linear, portfolio$ds_stage_cycle, y_name="birth_rate", stage_id_name="stage_id")
#
# library(ggplot2)
# ggplot(polarized$ds_stage_cycle_polar, aes(color=factor(stage_id))) +
#   geom_path(aes(x=PolarLowerX, y=PolarLowerY), linetype=2) +
#   geom_path(aes(x=PolarCenterX, y=PolarCenterY), size=2) +
#   geom_path(aes(x=PolarUpperX, y=PolarUpperY), linetype=2) +
#   geom_path(aes(x=ObservedX, y=ObservedY), data=polarized$dsObservedPolar) +
#   coord_fixed(ratio=1) +
#   guides(color=FALSE)
#
# #For a more polished graph, see polar_periodic().
