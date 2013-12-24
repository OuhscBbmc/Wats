rm(list=ls(all=TRUE))
library(Wats)
library(grid)
library(ggplot2) 
library(boot) 
library(circular) 

dsLinear <- CountyMonthBirthRate[CountyMonthBirthRate$CountyName=="oklahoma", ]

dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# base::pretty(x=dsLinear$BirthRate)

hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
seSpread <- function( scores) { return( mean(scores) + c(-1, 1)*sd(scores)/sqrt(length(scores)) ) }
bootSpread <- function( scores, conf=.66 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  dist <- boot(data=scores, plugin, R=99)
  ci <- boot.ci(dist, type = c("bca"), conf=conf)
  return( ci$bca[4:5] )
}
# b <- bootSpread(dsLinear$BirthRate)

# portfolioCartesian <- AnnotateData(dsLinear, dvName="BirthRate",centerFunction=median, 
#                                    spreadFunction=bootSpread)
#                                    #spreadFunction=seSpread)
# 
# portfolioPolar <- PolarizeCartesian(portfolioCartesian$dsLinear, portfolioCartesian$dsStageCycle, yName="BirthRate", stageIDName="StageID", plottedPointCountPerCycle=7200)


tsData <- stats::ts(
  data = dsLinear$BirthRate, 
  start = as.integer(dsLinear[1, c("Year", "Month")]), 
  end = as.integer(dsLinear[nrow(dsLinear), c("Year", "Month")]),
  frequency = 12)
m <- decompose(tsData)
plot(m)
