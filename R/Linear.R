# 
# ##' @name LinearPlot
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
# ##' @author Will Beasley
# ##' @keywords linear
# ##' @examples
# ##' a <- 32+323
# ##' 
# LinearPlot <- function(dsPlot, xName, yName, idName, title=NULL, xTitle=NULL, yTitle=NULL) {
#   p <- ggplot2::ggplot(dsPlot, aes_string(x=xName, y=yName, color=idName))
# #   # p <- p + geom_line(data=dsFebruary, aes(y=Rolling), size=1, color=smoothedLinear)
# #   # p <- p + geom_point(data=dsFebruary, aes(y=Rolling), size=4, shape=3, color=smoothedLinear)
# #   
# # #   p <- p + geom_ribbon(data=dsStage1, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorBefore[2], color=NA )
# # #   p <- p + geom_ribbon(data=dsStage2, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorAfter[2], color=NA )
# #   p <- p + geom_point(shape=1)
#   p <- p + geom_line(size=1)
# # #   p <- p + geom_line(data=ds[!is.na(ds$Rolling), ], aes(y=Rolling), size=2)
# # #   p <- p + scale_color_continuous(low=colorBefore, high=colorAfter, guide=FALSE)
# # #   p <- p + geom_vline(x=as.integer(changeMonth), color=colorAfter)
# # #   p <- p + annotate("text", x=changeMonth, y=max(ds$BirthRate), color=colorAfter, label="Bombing Effect")
# #   p <- p + theme_minimal()
# #   p <- p + labs(title=title, x=xTitle, y=yTitle)
#   
#   return( p )
# }
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
