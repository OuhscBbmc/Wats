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
                              paletteDark=NULL, paletteLight=NULL, colorPeriodic="brown",
                              changePoints=NULL, changePointLabels=NULL,
                              drawJaggedLine=TRUE, drawRollingLine=TRUE, drawBands=TRUE, drawPeriodicLineAndPoints=TRUE, 
                              title=NULL, xTitle=NULL, yTitle=NULL ) {
  
  stages <- sort(unique(dsLinear[, stageIDName]))
  p <- ggplot2::ggplot(dsLinear, ggplot2::aes_string(x=xName, y=yName, color=stageIDName))
  
  if( is.null(paletteDark) ) {
    if( length(stages) <= 4L)
      paletteDark <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(2,4,6,8)] #There's not a risk of defining more colors than levels
    else
      paletteDark <- colorspace::rainbow_hcl(n=length(stages))
  }  
  if( is.null(paletteLight) ) {
    if( length(stages) <= 4L)
      paletteLight <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(1,3,5,7)] #There's not a risk of defining more colors than levels
    else
      paletteLight <- colorspace::rainbow_hcl(n=length(stages))
  }  
    
  for( stage in stages) {
    dsStage <- dsLinear[stage<=dsLinear$StageProgress & dsLinear$StageProgress<=(stage+1),]
    if( drawJaggedLine )
      p <- p + ggplot2::geom_line(size=.5, color=paletteLight[stage], data=dsStage)
    if( drawRollingLine )
      p <- p + ggplot2::geom_line(ggplot2::aes_string(y=rollingCenterName), data=dsStage, size=1, color=paletteLight[stage], na.rm=T)
    if( drawBands )
      p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin=rollingLowerName, ymax=rollingUpperName), data=dsStage, fill=paletteLight[stage], color=NA, alpha=.2)
    
    p <- p + ggplot2::geom_point(shape=1, color=paletteLight[stage], data=dsStage)
  }
  
  if( drawPeriodicLineAndPoints ) {
    p <- p + ggplot2::geom_line(data=dsLinear[dsLinear$TerminalPointInCycle,], ggplot2::aes_string(y=rollingCenterName), size=.5, color=colorPeriodic)
    p <- p + ggplot2::geom_point(data=dsLinear[dsLinear$TerminalPointInCycle,], ggplot2::aes_string(y=rollingCenterName), size=4, shape=3, color=colorPeriodic)
  }  
  
#   p <- p + ggplot2::geom_vline(x=as.integer(changeMonth))
  if( !is.null(changePoints) ) {
    for( i in seq_along(changePoints) )  {
      p <- p + ggplot2::geom_vline(x=as.integer(changePoints[i]), color=paletteLight[i+1], alpha=.5, size=4)
      p <- p + ggplot2::annotate("text", x=changeMonth, y=max(dsLinear[, yName]), color=paletteDark[i+1], label=changePointLabels[i])
    }
  }

#   p <- p + ggplot2::geom_vline(x=as.integer(changeMonth), color="gray70")
# # #   p <- p + ggplot2::annotate("text", x=changeMonth, y=max(ds$BirthRate), color=colorAfter, label="Bombing Effect")
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::labs(title=title, x=xTitle, y=yTitle)
  
  return( p )
}


# dsLinear <- read.table(file="./inst/extdata/BirthRatesOk.txt", header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# dsLinear$StageID <- ifelse(dsLinear$Date < as.Date("1996-02-15"), 1L, 2L)
# dsLinear <- Wats::AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# 
# hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# dsCombined <- Wats::AnnotateData(dsLinear, dvName="BirthRate",centerFunction=median, spreadFunction=hSpread)
# # sapply(dsCombined, head, 20)
# 
# LinearRollingPlot(dsCombined$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", 
#                   changePoints=as.Date("1996-02-15"), changePointLabels="Bombing Effect")




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
