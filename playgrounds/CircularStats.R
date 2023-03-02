rm(list=ls(all=TRUE))
library(Wats)
library(grid)
library(ggplot2)
library(boot)
library(circular)

ds_linear <- county_month_birth_rate_2014_version[county_month_birth_rate_2014_version$CountyName=="oklahoma", ]

ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
# base::pretty(x=ds_linear$BirthRate)

hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
seSpread <- function( scores ) { return( mean(scores) + c(-1, 1)*sd(scores)/sqrt(length(scores)) ) }
bootSpread <- function( scores, conf=.68 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  distribution <- boot(data=scores, plugin, R=99)
  ci <- boot.ci(distribution, type = c("bca"), conf=conf)
  return( ci$bca[4:5] )
}
# b <- bootSpread(ds_linear$BirthRate)

# portfolioCartesian <- annotate_data(ds_linear, dv_name="BirthRate",center_function=median,
#                                    spread_function=bootSpread)
#                                    #spread_function=seSpread)
#
# portfolioPolar <- polarize_cartesian(portfolioCartesian$ds_linear, portfolioCartesian$dsStageCycle, yName="BirthRate", stage_id_name="StageID", plottedPointCountPerCycle=7200)


tsData <- stats::ts(
  data = ds_linear$BirthRate,
  start = as.integer(ds_linear[1, c("Year", "Month")]),
  end = as.integer(ds_linear[nrow(ds_linear), c("Year", "Month")]),
  frequency = 12)
m <- decompose(tsData)
plot(m)
