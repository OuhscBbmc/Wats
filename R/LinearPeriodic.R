LinearPeriodicPlot <- function(dsLinear, xName, yName, stageIDName, 
                              periodicLowerName="RollingLower", periodicUpperName="RollingUpper",
                              paletteDark=NULL, paletteLight=NULL, colorPeriodic=grDevices::adjustcolor("tan1", .5),
                              changePoints=NULL, changePointLabels=NULL,
                              jaggedPointSize=2, jaggedLineSize=.5, 
                              bandAlphaDark=.4, bandAlphaLight=.4, 
                              changeLineAlpha=.5, changeLineSize=3,
                              title=NULL, xTitle=NULL, yTitle=NULL ) {
  
  stages <- sort(unique(dsLinear[, stageIDName]))
  stageCount <- length(stages)
  testit::assert("The number of unique `StageID` values should be 1 greater than the number of `changePoints`.", stageCount==1+length(changePoints))
  if( !is.null(changePoints) ) testit::assert("The number of `changePoints` should equal the number of `changeLabels`.", length(changePoints)==length(changePointLabels))
  if( !is.null(paletteDark) ) testit::assert("The number of `paletteDark` colors should equal the number of unique `StageID` values.", stageCount==length(paletteDark))
  if( !is.null(paletteLight) ) testit::assert("The number of `paletteLight` colors should equal the number of unique `StageID` values.", stageCount==length(paletteLight))
  
  p <- ggplot2::ggplot(dsLinear, ggplot2::aes_string(x=xName, y=yName, color=stageIDName))
  
  if( is.null(paletteDark) ) {
    if( length(stages) <= 4L) paletteDark <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(2,4,6,8)] #There's not a risk of defining more colors than levels
    else paletteDark <- colorspace::rainbow_hcl(n=length(stages), l=40)
  }  
  if( is.null(paletteLight) ) {
    if( length(stages) <= 4L) paletteLight <- RColorBrewer::brewer.pal(n=10, name="Paired")[c(1,3,5,7)] #There's not a risk of defining more colors than levels
    else paletteLight <- colorspace::rainbow_hcl(n=length(stages), l=70)
  }  
    
  for( stage in stages) {
    dsStage <- dsLinear[stage<=dsLinear$StageProgress & dsLinear$StageProgress<=(stage+1), ]
    
    p <- p + ggplot2::geom_line(size=jaggedLineSize, color=paletteDark[stage], data=dsStage)
    
    p <- p + ggplot2::geom_point(shape=1, color=paletteLight[stage], data=dsStage, size=jaggedPointSize)
  }
  
  #Remove this line:
  p <- p + ggplot2::geom_point(shape=1, color=paletteLight[stage], data=dsStage, size=jaggedPointSize)
  
  

#   if( !is.null(changePoints) ) {
#     for( i in seq_along(changePoints) )  {
#       p <- p + ggplot2::geom_vline(x=as.integer(changePoints[i]), color=paletteLight[i+1], alpha=changeLineAlpha, size=changeLineSize)
#       p <- p + ggplot2::annotate("text", x=changePoints[i], y=Inf, vjust=1.1, color=paletteLight[i+1], label=changePointLabels[i])
#     }
#   }

  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::labs(title=title, x=xTitle, y=yTitle)
  
  return( p )
}

# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
# dsLinear <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
# changeMonth <- as.Date("1996-02-15")
# dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
# dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# 
# hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
# portfolio <- AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=hSpread)
# 
# LinearPeriodicPlot(portfolio$dsPeriodic, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, changePointLabels="Bombing Effect")

