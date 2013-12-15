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
                              colorPalette=NULL,
                              title=NULL, xTitle=NULL, yTitle=NULL ) {
  
  stages <- sort(unique(dsLinear[, stageIDName]))
  p <- ggplot2::ggplot(dsLinear, ggplot2::aes_string(x=xName, y=yName, color=stageIDName))
  
#   stageColor <- c("red", "blue")
  if( missing(colorPalette) | is.null(colorPalette) ) {
    if( length(stages) <= 9L)
      colorPalette <- RColorBrewer::brewer.pal(n=9L, name="Pastel1") #There's not a risk of defining more colors than levels
    else
      colorPalette <- colorspace::rainbow_hcl(n=length(stages))
  }
  
  for( stage in stages) {
#     print(stage)
    dsStage <- dsLinear[stage<=dsLinear$StageProgress & dsLinear$StageProgress<=(stage+1),]
#     print(dsStage)
    p <- p + ggplot2::geom_line(ggplot2::aes_string(y=rollingCenterName), data=dsStage, size=1, color=colorPalette[stage], na.rm=T)
    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin=rollingLowerName, ymax=rollingUpperName), data=dsStage, fill=colorPalette[stage], color=NA, alpha=.2)
  }
  
  p <- p + ggplot2::geom_point(data=dsLinear[dsLinear$TerminalPointInCycle,], ggplot2::aes_string(y=rollingCenterName, color=stageIDName), size=4, shape=3)

# #   p <- p + geom_point(shape=1)
#   p <- p + ggplot2::geom_line(size=.5)
# # #   p <- p + geom_line(data=ds[!is.na(ds$Rolling), ], aes(y=Rolling), size=2)
#   p <- p + ggplot2::scale_color_continuous(guide=FALSE)#low=colorBefore, high=colorAfter, 
#   p <- p + ggplot2::geom_vline(x=as.integer(changeMonth), color=colorAfter)
# # #   p <- p + ggplot2::annotate("text", x=changeMonth, y=max(ds$BirthRate), color=colorAfter, label="Bombing Effect")
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::labs(title=title, x=xTitle, y=yTitle)
  
  return( p )
}


# 
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
# 
# 


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
