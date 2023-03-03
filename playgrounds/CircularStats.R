rm(list=ls(all=TRUE))
library(Wats)
library(grid)
library(ggplot2)
library(boot)
library(circular)

ds_linear <- county_month_birth_rate_2014_version[county_month_birth_rate_2014_version$county_name=="oklahoma", ]

ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
# base::pretty(x=ds_linear$birth_rate)

hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
seSpread <- function( scores ) { return( mean(scores) + c(-1, 1)*sd(scores)/sqrt(length(scores)) ) }
bootSpread <- function( scores, conf=.68 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  distribution <- boot(data=scores, plugin, R=99)
  ci <- boot.ci(distribution, type = c("bca"), conf=conf)
  return( ci$bca[4:5] )
}
# b <- bootSpread(ds_linear$birth_rate)

# portfolioCartesian <- annotate_data(ds_linear, dv_name="birth_rate",center_function=median,
#                                    spread_function=bootSpread)
#                                    #spread_function=seSpread)
#
# portfolioPolar <- polarize_cartesian(portfolioCartesian$ds_linear, portfolioCartesian$ds_stage_cycle, y_name="birth_rate", stage_id_name="stage_id", plotted_point_count_per_cycle=7200)


tsData <- stats::ts(
  data = ds_linear$birth_rate,
  start = as.integer(ds_linear[1, c("year", "month")]),
  end = as.integer(ds_linear[nrow(ds_linear), c("year", "month")]),
  frequency = 12)
m <- decompose(tsData)
plot(m)
