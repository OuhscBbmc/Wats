
## ----set_options, echo=FALSE, results='hide'-----------------------------
require(knitr)
opts_chunk$set(
    comment=NA, 
    tidy=FALSE,
    fig.width=6.5, 
    fig.height=1.6,
    fig.path='figure_rmd/',
    dpi=100
)

# options(markdown.HTML.header = system.file("misc", "vignette.css", package = "knitr"))
# options(markdown.HTML.header = system.file("misc", "vignette.css", package = "REDCapR"))
# options(markdown.HTML.header = file.path(devtools::inst("REDCapR"), "misc", "vignette.css"))

options(width=120) #So the output is 50% wider than the default.


## ----Definitions---------------------------------------------------------
library(Wats)
library(grid)
library(plyr) 
library(ggplot2) 
library(boot) 

changeMonth <- as.Date("1996-02-15") #as.Date("1995-04-19") + lubridate::weeks(39) = "1996-01-17"

vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }

fullSpread <- function( scores) { 
  return( range(scores) ) #A new function isn't necessary.  It's defined in order to be consistent.
}
hSpread <- function( scores) { 
  return( quantile(x=scores, probs=c(.25, .75)) ) 
}
seSpread <- function( scores) { 
  return( mean(scores) + c(-1, 1) * sd(scores) / sqrt(length(scores)) ) 
}
bootSpread <- function( scores, conf=.66 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  dist <- boot(data=scores, plugin, R=99) #999 for the publication
  ci <- boot.ci(dist, type = c("bca"), conf=conf)
  return( ci$bca[4:5] ) #The fourth & fifth elements correspond to the lower & upper bound.
}


## ----Figure2IndividualBasic----------------------------------------------
dsLinear <- CountyMonthBirthRate2005Version[CountyMonthBirthRate2005Version$CountyName=="oklahoma", ]

dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")

portfolioCartesian <- AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=hSpread)

CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date", 
  yName = "BirthRate", 
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  changePointLabels = "Bombing Effect"
)


## ----Figure2IndividualStylized-------------------------------------------
darkTheme <- ggplot2::theme(
  axis.title          = element_text(color="gray40", size=9),
  axis.text.x         = element_text(color="gray40", hjust=0),
  axis.text.y         = element_text(color="gray40"),
  axis.ticks.length   = grid::unit(0, "cm"),
  axis.ticks.margin   = grid::unit(.00001, "cm"),
  panel.grid.minor.y  = element_line(color="gray99", size=.1),
  panel.grid.major    = element_line(color="gray95", size=.1),
  panel.margin        = grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin         = grid::unit(c(0, 0, 0, 0), "cm")
)
lightTheme <- darkTheme + ggplot2::theme(
  axis.title          = element_text(color="gray80", size=9),
  axis.text.x         = element_text(color="gray80", hjust=0),
  axis.text.y         = element_text(color="gray80")
)
dateSequence <- seq.Date(from=as.Date("1990-01-01"), to=as.Date("1999-01-01"), by="years")
xScale       <- scale_x_date(breaks=dateSequence, labels=scales::date_format("%Y"))
xScaleBlank  <- scale_x_date(breaks=dateSequence, labels=NULL) #This keeps things proportional down the three frames.

topPanel <- CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date", 
  yName = "BirthRate", 
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  yTitle = "General Fertility Rate",
  changePointLabels = "Bombing Effect", 
  drawRollingBand = FALSE, 
  drawRollingLine = FALSE
)

middlePanel <- CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date", 
  yName = "BirthRate", 
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  yTitle = "General Fertility Rate",
  changePointLabels = "", 
  drawRollingBand = FALSE, 
  drawJaggedLine = FALSE
)

bottomPanel <- CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date", 
  yName = "BirthRate", 
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  yTitle = "General Fertility Rate", 
  changePointLabels = "", 
  drawJaggedLine = FALSE
)

topPanel <- topPanel + xScale + darkTheme
middlePanel <- middlePanel + xScale + darkTheme
bottomPanel <- bottomPanel + xScaleBlank + darkTheme


## ----Figure2Combined, fig.height=4.8-------------------------------------
#, out.height='300px', out.width='300px'
vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,1)))
print(topPanel, vp=vpLayout(1, 1))
print(middlePanel, vp=vpLayout(2, 1))
print(bottomPanel, vp=vpLayout(3, 1))
popViewport()


## ----Figure4Basic--------------------------------------------------------
cartesianPeriodicSimple <- CartesianPeriodic(
  portfolioCartesian$dsLinear, 
  portfolioCartesian$dsPeriodic, 
  xName = "Date", 
  yName = "BirthRate",
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  changePointLabels = "Bombing Effect",
  yTitle = "General Fertility Rate",
  drawPeriodicBand = FALSE
)
print(cartesianPeriodicSimple) #`print()` isn't necessary, but it makes my intention a little clearer.


## ----Figure4Stylized-----------------------------------------------------
print(cartesianPeriodicSimple + xScale + darkTheme) 


## ----Figure5Basic--------------------------------------------------------
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
print(cartesianPeriodic)


## ----Figure5Stylized-----------------------------------------------------
cartesianPeriodic <- cartesianPeriodic + xScale + darkTheme 
print(cartesianPeriodic)


## ----Figure6, fig.height=3, fig.width=3----------------------------------
portfolioPolar <- PolarizeCartesian( 
  dsLinear = portfolioCartesian$dsLinear, 
  dsStageCycle = portfolioCartesian$dsStageCycle, 
  yName = "BirthRate", 
  stageIDName = "StageID", 
  plottedPointCountPerCycle = 7200
)

windows.options(antialias = "cleartype")
grid.newpage()
PolarPeriodic(
  dsLinear = portfolioPolar$dsObservedPolar, 
  dsStageCycle = portfolioPolar$dsStageCyclePolar, 
  yName = "Radius",
  stageIDName = "StageID", 
  drawPeriodicBand = FALSE,
  cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1")
)



## ----Figure7, fig.height=6.5*2/3-----------------------------------------
portfolioPolar <- PolarizeCartesian(
  dsLinear = portfolioCartesian$dsLinear, 
  dsStageCycle = portfolioCartesian$dsStageCycle, 
  yName = "BirthRate", 
  stageIDName = "StageID", 
  plottedPointCountPerCycle = 7200
)

grid.newpage()
pushViewport(viewport(
  layout=grid.layout(
    nrow = 2, ncol = 2, respect = TRUE, 
    widths = unit(c(1,1), c("null", "null")), 
    heights = unit(c(1,.5), c("null", "null"))
  ), 
  gp = gpar(cex=1, fill=NA)
))

## Create top left panel
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
topLeftPanel <- PolarPeriodic(  
  dsLinear = portfolioPolar$dsObservedPolar, 
  dsStageCyclePolar = portfolioPolar$dsStageCyclePolar, 
  yName = "Radius", 
  stageIDName = "StageID", #graphCeiling=7, 
  cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1")
)
upViewport()

## Create top right panel
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
topRighttPanel <- PolarPeriodic(
  dsLinear = portfolioPolar$dsObservedPolar, 
  dsStageCyclePolar = portfolioPolar$dsStageCyclePolar, 
  yName = "Radius", 
  stageIDName = "StageID", #graphCeiling=7, 
  drawObservedLine = FALSE,
  cardinalLabels = c("Jan1", "Apr1", "July1", "Oct1"), 
  originLabel = NULL
)
upViewport()

## Create bottom panel
pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2, gp=gpar(cex=1)))
print(cartesianPeriodic, vp=vpLayout(x=1:2, y=2)) #Print across both columns of the bottom row.
upViewport()


## ----Figure8, fig.height=6.5---------------------------------------------
dsLinearAll <- AugmentYearDataWithMonthResolution(dsLinear=CountyMonthBirthRate2005Version, dateName="Date")

#Identify the average size of the fecund population
plyr::ddply(dsLinearAll, "CountyName", summarize, Mean=mean(FecundPopulation))


GraphRowComparison <- function( rowLabel="", countyName="oklahoma", spreadFunction=hSpread, changeMonth=as.Date("1996-02-15") ) {
  dsLinear <- CountyMonthBirthRate2005Version[CountyMonthBirthRate2005Version$CountyName==countyName, ]
  dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
  portfolioCartesian <- AnnotateData(dsLinear, dvName="BirthRate", centerFunction=median, spreadFunction=spreadFunction)
  portfolioPolar <- PolarizeCartesian(dsLinear=portfolioCartesian$dsLinear, dsStageCycle=portfolioCartesian$dsStageCycle, yName="BirthRate", stageIDName="StageID", plottedPointCountPerCycle=7200)
  cartesianPeriodic <- CartesianPeriodic(portfolioCartesian$dsLinear, portfolioCartesian$dsPeriodic, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, changePointLabels=""  )
  
  pushViewport(viewport(
    layout=grid.layout(nrow=1, ncol=3, respect=F, widths=unit(c(1.5,1,3), c("line", "null", "null"))), 
    gp=gpar(cex=1, fill=NA)
  ))
  pushViewport(viewport(layout.pos.col=1))
  grid.rect(gp=gpar(fill="gray90", col=NA))
  grid.text(rowLabel, rot=90)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2))
  polarPeriodic <- PolarPeriodic(dsLinear=portfolioPolar$dsObservedPolar, dsStageCyclePolar=portfolioPolar$dsStageCyclePolar, drawObservedLine=FALSE, yName="Radius", stageIDName="StageID", originLabel=NULL, plotMargins=c(0,0,0,0))
  popViewport()
  
  pushViewport(viewport(layout.pos.col=3))
  print(cartesianPeriodic + xScale + lightTheme, vp=vpLayout(x=1, y=1))
  popViewport()
  popViewport() #Finish the row
}

counties <- c("comanche", "cleveland", "oklahoma", "tulsa", "rogers")
countyNames <- c("Comanche", "Cleveland", "Oklahoma", "Tulsa", "Rogers")

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(counties), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(counties) ) {
  pushViewport(viewport(layout.pos.row=i))
  GraphRowComparison(countyName=counties[i], rowLabel=countyNames[i])
  popViewport()
}
popViewport()


## ----Figure8AllCounties, fig.height=6.5 * 12/5---------------------------
counties <- sort(unique(CountyMonthBirthRate2005Version$CountyName))
countyNames <- c("Canadian", "Cleveland", "Comanche", "Creek", "Logan", "McClain", "Oklahoma", "Osage", "Pottawatomie", "Rogers", "Tulsa", "Wagoner")

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(counties), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(counties) ) {
  pushViewport(viewport(layout.pos.row=i))
  GraphRowComparison(countyName=counties[i], rowLabel=countyNames[i])
  popViewport()
}
popViewport()


## ----Figure9, fig.height=6.5 * 4/5---------------------------------------
spreads <- c("hSpread", "fullSpread", "seSpread", "bootSpread")
spreadNames <- c("H-Spread", "Range", "+/-1 SE", "Bootstrap")
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(spreads), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(spreads) ) {
  pushViewport(viewport(layout.pos.row=i))
  GraphRowComparison(spreadFunction=get(spreads[i]), rowLabel=spreadNames[i])
  upViewport()
}
upViewport()


## ----session_info, echo=FALSE--------------------------------------------
cat("Report created by", Sys.info()["user"], "at", strftime(Sys.time(), "%c, %z"))
sessionInfo()


