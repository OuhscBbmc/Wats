rm(list=ls(all=TRUE))
vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }

fullSpread <- function( scores) { 
  return( range(scores) ) 
}
hSpread <- function( scores) { 
  return( quantile(x=scores, probs=c(.25, .75)) ) 
}
seSpread <- function( scores) { 
  return( mean(scores) + c(-1, 1) * sd(scores) / sqrt(length(scores)) ) 
}
bootSpread <- function( scores, conf=.66 ) {
  plugin <- function( d, i ) { mean(d[i]) }
  
  dist <- boot(data=scores, plugin, R=999)
  ci <- boot.ci(dist, type = c("bca"), conf=conf)
  return( ci$bca[4:5] ) #The fourth & fifth elements correspond to the lower & upper bound.
}
lightTheme <- ggplot2::theme(
  axis.title          = element_text(color="gray60", size=9),
  axis.text.x         = element_text(color="gray80", hjust=0),
  axis.text.y         = element_text(color="gray80"),
  axis.ticks.length   = grid::unit(0, "cm"), #g <- g + theme(axis.ticks=element_blank())
  axis.ticks.margin   = grid::unit(.00001, "cm"),
  panel.grid.minor.y  = element_line(color="gray99", size=.1),
  panel.grid.major    = element_line(color="gray95", size=.1),
  panel.margin        = grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin         = grid::unit(c(0, 0, 0, 0), "cm")
)

GraphCountyComparison <- function( rowLabel="", countyName="oklahoma", spreadFunction=hSpread, changeMonth=as.Date("1996-02-15") ) {
  dsLinear <- CountyMonthBirthRate2005Version[CountyMonthBirthRate2005Version$CountyName==countyName, ]
  
  dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
  
  portfolioCartesian <- AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=spreadFunction)
  
  portfolioPolar <- PolarizeCartesian(
    dsLinear = portfolioCartesian$dsLinear, 
    dsStageCycle = portfolioCartesian$dsStageCycle, 
    yName = "BirthRate", 
    stageIDName = "StageID", 
    plottedPointCountPerCycle = 7200
  )
  cartesianPeriodic <- CartesianPeriodic(
    portfolioCartesian$dsLinear, 
    portfolioCartesian$dsPeriodic, 
    xName = "Date", 
    yName = "BirthRate",
    stageIDName = "StageID", 
    changePoints = changeMonth, 
    changePointLabels = "",
#     yTitle = "General Fertility Rate",
    drawPeriodicBand = TRUE #The only difference from the simple linear graph above
  )
  
  pushViewport(viewport(
    layout=grid.layout(nrow=1, ncol=3, respect=F, widths=unit(c(2,1,3), c("line", "null", "null"))), 
    gp=gpar(cex=1, fill=NA)
  ))
#   vpLabel <- viewport(name="vpLabel", layout.pos.row=1)
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.rect(gp=gpar(fill="gray90", col=NA))
  grid.text(rowLabel, rot=90)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
  polarPeriodic <- PolarPeriodic(  
    dsLinear = portfolioPolar$dsObservedPolar, 
    dsStageCyclePolar = portfolioPolar$dsStageCyclePolar, 
    drawObservedLine = FALSE,
    yName = "Radius", 
    stageIDName = "StageID", 
#     cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1"), 
    originLabel=NULL
  )
  upViewport()
  
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=1, gp=gpar(cex=1)))
  print(cartesianPeriodic + lightTheme, vp=vpLayout(x=2, y=1)) #Print across both columns of the bottom row.
  upViewport()
  upViewport()
}

counties <- c("tulsa", "oklahoma", "cleveland", "comanche")
spreads <- c("hSpread", "fullSpread", "seSpread", "bootSpread")

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(counties), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(counties) ) {
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))
  GraphCountyComparison(countyName=counties[i], rowLabel=counties[i])
  upViewport()
}
upViewport()


grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(spreads), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(spreads) ) {
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))
  GraphCountyComparison(spreadFunction=get(spreads[i]), rowLabel=spreads[i])
  upViewport()
}
upViewport()
