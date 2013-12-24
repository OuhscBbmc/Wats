rm(list=ls(all=TRUE))
countyName <- "tulsa"
changeMonth <- as.Date("1996-02-15")

vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }

fullSpread <- function( scores) { 
  return( c(min(scores), max(scores)) ) 
}
hSpread <- function( scores) { 
  return( quantile(x=scores, probs=c(.25, .75)) ) 
}

dsLinear <- CountyMonthBirthRate2005Version[CountyMonthBirthRate2005Version$CountyName==countyName, ]

dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")

portfolioCartesian <- AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=hSpread)

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
  changePointLabels = "Bombing Effect",
  yTitle = "General Fertility Rate",
  drawPeriodicBand = TRUE #The only difference from the simple linear graph above
)

grid.newpage()
pushViewport(viewport(
  layout=grid.layout(nrow=1, ncol=2, respect=F, widths=unit(c(1,3), c("null", "null"))), 
  gp=gpar(cex=1, fill=NA)
))
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
polarPeriodic <- PolarPeriodic(  
  dsLinear = portfolioPolar$dsObservedPolar, 
  dsStageCyclePolar = portfolioPolar$dsStageCyclePolar, 
  drawObservedLine = FALSE,
  yName = "Radius", 
  stageIDName = "StageID", 
  cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1"), 
  originLabel=NULL
)
upViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=1, gp=gpar(cex=1)))
print(cartesianPeriodic, vp=vpLayout(x=2, y=1)) #Print across both columns of the bottom row.
upViewport()
upViewport()


# print(cartesianPeriodic, , vp=vpLayout(x=1:2, y=2))
# upViewport()

# viewport(layout=grid.layout(nrow=4, ncol=1, respect=TRUE), gp=gpar(cex=1, fill=NA))
