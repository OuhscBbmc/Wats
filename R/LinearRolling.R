# 
# ##' @name LinearRollingPlot
# ##' @export
# ##' 
# ##' @title Shows the interrupted time series in Cartesian coordinates
# ##' 
# ##' @description Shows the interrupted time series in Cartesian coordinates.
# ##' 
# ##' @param dsPlot The \code{data.frame} to cotaining the detailed data.
# ##' @param xName The variable name in \code{dsPlot} containing the date
# ##' @param linksPair The \code{data.frame} to validate.
# ##' @return Returns a \code{ggplot2} graphing object
# ##' @keywords linear
# ##' @examples
# ##' a <- 32+323
# ##' 
LinearRollingPlot <- function(dsLinear, xName, yName, stageIDName, 
                              rollingLowerName="RollingLower", rollingCenterName="RollingCenter", rollingUpperName="RollingUpper",
                              title=NULL, xTitle=NULL, yTitle=NULL ) {
  p <- ggplot2::ggplot(dsLinear, ggplot2::aes_string(x=xName, y=yName, color=stageIDName))
  p <- p + ggplot2::geom_line(ggplot2::aes_string(y=rollingCenterName), size=1, na.rm=T)
  p <- p + ggplot2::geom_point(data=dsLinear[dsLinear$TerminalPointInCycle,], ggplot2::aes_string(y=rollingCenterName), size=4, shape=3)

#   p <- p + geom_ribbon(aes_string(ymin=rollingLowerName, ymax=rollingUpperName))
p <- p + ggplot2::geom_ribbon(data=dsLinear[1<=dsLinear$StageID & dsLinear$StageID<=2,], ggplot2::aes_string(ymin=rollingLowerName, ymax=rollingUpperName), fill="red", color=NA, alpha=.2)
p <- p + ggplot2::geom_ribbon(data=dsLinear[2<=dsLinear$StageID & dsLinear$StageID<=3,], ggplot2::aes_string(ymin=rollingLowerName, ymax=rollingUpperName), fill="blue", color=NA, alpha=.2)
#   p <- p + geom_ribbon(aes_string(ymin=rollingLowerName, ymax=rollingUpperName), fill="blue", color=NA )
#   p <- p + geom_point(shape=1)
  p <- p + ggplot2::geom_line(size=.5)
# #   p <- p + geom_line(data=ds[!is.na(ds$Rolling), ], aes(y=Rolling), size=2)
  p <- p + ggplot2::scale_color_continuous(guide=FALSE)#low=colorBefore, high=colorAfter, 
# #   p <- p + geom_vline(x=as.integer(changeMonth), color=colorAfter)
# #   p <- p + annotate("text", x=changeMonth, y=max(ds$BirthRate), color=colorAfter, label="Bombing Effect")
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::labs(title=title, x=xTitle, y=yTitle)
  
  return( p )
}

# dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
# dsLinear <- Wats::AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# 
# 
# hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# dsCombined <- Wats::AnnotateData(dsLinear, dvName="BirthRate",centerFunction=median, spreadFunction=hSpread)
# sapply(dsCombined, head, 20)
# 
# LinearRollingPlot(dsCombined$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID")













# # LinearPlot(dsBirthRate, "Date", "BirthRate", "StageID")
# 
# # p <- ggplot(ds, aes(x=Date, y=BirthRate, color=StageID))
# # p <- p + geom_line(data=dsFebruary, aes(y=Rolling), size=1, color=smoothedLinear)
# # p <- p + geom_point(data=dsFebruary, aes(y=Rolling), size=4, shape=3, color=smoothedLinear)
# # 
# # p <- p + geom_ribbon(data=dsStage1, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorBefore[2], color=NA )
# # p <- p + geom_ribbon(data=dsStage2, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorAfter[2], color=NA )
# # p <- p + geom_point(shape=1)
# # p <- p + geom_line(size=1)
# # p <- p + geom_line(data=ds[!is.na(ds$Rolling), ], aes(y=Rolling), size=2)
# # p <- p + scale_color_continuous(low=colorBefore, high=colorAfter, guide=FALSE)
# # p <- p + geom_vline(x=as.integer(changeMonth), color=colorAfter)
# # p <- p + annotate("text", x=changeMonth, y=max(ds$BirthRate), color=colorAfter, label="Bombing Effect")
# # p <- p + theme_minimal()
# # p <- p + labs(x="", y="General Fertility Rate")
# # p
