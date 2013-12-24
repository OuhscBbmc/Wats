
## ----set_options, echo=FALSE, results='hide'-----------------------------
require(knitr)
opts_chunk$set(
    comment=NA, 
    tidy=FALSE,
    fig.width=6.5, 
    fig.height=1.6,
    fig.path='figure_rmd/',
    dpi=200
)

# options(markdown.HTML.header = system.file("misc", "vignette.css", package = "knitr"))
# options(markdown.HTML.header = system.file("misc", "vignette.css", package = "REDCapR"))
# options(markdown.HTML.header = file.path(devtools::inst("REDCapR"), "misc", "vignette.css"))

options(width=120) #So the output is 50% wider than the default.


## ----Figure2IndividualBasic----------------------------------------------
library(Wats)
library(grid)
library(ggplot2) 
library(boot) 
dsLinear <- CountyMonthBirthRate2005Version[CountyMonthBirthRate2005Version$CountyName=="oklahoma", ]
# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
# dsLinear <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
# dsLinear$Date <- as.Date(dsLinear$Date) 
# dsLinear$MonthID <- NULL
changeMonth <- as.Date("1996-02-15")
dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
# base::pretty(x=dsLinear$BirthRate)

hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
seSpread <- function( scores) { return( mean(scores) + c(-1, 1)*sd(scores)/sqrt(length(scores)) ) }
bootSpread <- function( scores, conf=.66 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  dist <- boot(data=scores, plugin, R=999)
  ci <- boot.ci(dist, type = c("bca"), conf=conf)
  return( ci$bca[4:5] )
}
# b <- bootSpread(dsLinear$BirthRate)

portfolioCartesian <- AnnotateData(dsLinear, dvName="BirthRate",centerFunction=median, 
                                   spreadFunction=bootSpread)
                                   #spreadFunction=seSpread)

CartesianRolling(dsLinear=portfolioCartesian$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, changePointLabels="Bombing Effect")


## ----Figure2IndividualStylized-------------------------------------------
fig2Theme <- ggplot2::theme(
  axis.title          = element_text(color="gray60", size=9),
  axis.text.x         = element_text(color="gray80", hjust=0),
  axis.text.y         = element_text(color="gray80"),
  axis.ticks.length   = grid::unit(0, "cm"), #g <- g + theme(axis.ticks=element_blank())
  axis.ticks.margin   = grid::unit(.00001, "cm"),
  panel.grid.minor.y  = element_line(color="gray90", size=.1),
  panel.grid.major    = element_line(color="gray85", size=.15),
  panel.margin        = grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin         = grid::unit(c(0, 0, 0, 0), "cm")
)
xScaleBlank <- scale_x_date(breaks=seq.Date(from=as.Date("1990-01-01"), to=as.Date("1999-01-01"), by="years"), labels=NULL)
xScale <- scale_x_date(breaks=seq.Date(from=as.Date("1990-01-01"), to=as.Date("1999-01-01"), by="years"), labels=scales::date_format("%Y"))
# yScale <- scale_y_continuous(breaks=5:7)
# yExpand <- expand_limits(y=c(5, 7))

topPanel <- CartesianRolling(dsLinear=portfolioCartesian$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, yTitle="General Fertility Rate", 
                              changePointLabels="Bombing Effect", 
                              drawRollingBand=FALSE, 
                              drawRollingLine=FALSE)
middlePanel <- CartesianRolling(portfolioCartesian$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, yTitle="General Fertility Rate",
                              changePointLabels="", 
                              drawRollingBand=FALSE, 
                              drawJaggedLine=FALSE)
bottomPanel <- CartesianRolling(portfolioCartesian$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, yTitle="General Fertility Rate", 
                              changePointLabels="", 
                              drawJaggedLine=FALSE)
topPanel <- topPanel + xScale + fig2Theme #+ yScale + yExpand
middlePanel <- middlePanel + xScale + fig2Theme #+ yScale + yExpand
bottomPanel <- bottomPanel + xScaleBlank + fig2Theme #+ yScale + yExpand

topPanel
middlePanel
bottomPanel


## ----Figure2Combined, fig.height=4.8-------------------------------------
#out.height=4.8, out.width=6.5,
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
  yTitle="General Fertility Rate",
  drawPeriodicBand=FALSE
)
cartesianPeriodicSimple


## ----Figure4Stylized-----------------------------------------------------
fig4Theme <- ggplot2::theme(
  axis.title          = element_text(color="gray60", size=9),
  axis.text.x         = element_text(color="gray80", hjust=0),
  axis.text.y         = element_text(color="gray80"),
  axis.ticks.length   = grid::unit(0, "cm"), #g <- g + theme(axis.ticks=element_blank())
  axis.ticks.margin   = grid::unit(.00001, "cm"),
  panel.grid.minor.y  = element_line(color="gray90", size=.1),
  panel.grid.major    = element_line(color="gray85", size=.15),
  panel.margin        = grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin         = grid::unit(c(0, 0, 0, 0), "cm")
)
xScale <- scale_x_date(breaks=seq.Date(from=as.Date("1990-01-01"), to=as.Date("1999-01-01"), by="years"), labels=scales::date_format("%Y"))
# yScale <- scale_y_continuous(breaks=5:7)
# yExpand <- expand_limits(y=c(5, 7))

cartesianPeriodicSimple <- cartesianPeriodicSimple + xScale + fig4Theme # + yScale + yExpand
cartesianPeriodicSimple


## ----Figure5Basic--------------------------------------------------------
cartesianPeriodic <- CartesianPeriodic(
  portfolioCartesian$dsLinear, 
  portfolioCartesian$dsPeriodic, 
  xName = "Date", 
  yName = "BirthRate",
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  changePointLabels = "Bombing Effect",
  yTitle="General Fertility Rate",
  drawPeriodicBand=TRUE #The only difference from the simple linear graph above
)
cartesianPeriodic


## ----Figure5Stylized-----------------------------------------------------
cartesianPeriodic <- cartesianPeriodic + xScale + fig4Theme # + yScale + yExpand
cartesianPeriodic


## ----Figure6, fig.height=6, fig.width=6----------------------------------
portfolioPolar <- PolarizeCartesian(portfolioCartesian$dsLinear, portfolioCartesian$dsStageCycle, yName="BirthRate", stageIDName="StageID", plottedPointCountPerCycle=7200)

windows.options(antialias = "cleartype")
grid.newpage()
PolarPeriodic(dsLinear=portfolioPolar$dsObservedPolar, portfolioPolar$dsStageCyclePolar, yName="Radius", 
              stageIDName="StageID", drawPeriodicBand=F,
              cardinalLabels=c("Jan1", "Apr1", "July1", "Oct1"))



## ----Figure7, fig.height=6.5*2/3-----------------------------------------
portfolioPolar <- PolarizeCartesian(portfolioCartesian$dsLinear, portfolioCartesian$dsStageCycle, yName="BirthRate", stageIDName="StageID", plottedPointCountPerCycle=7200)

grid.newpage()
pushViewport(viewport(
  layout=grid.layout(nrow=2, ncol=2, respect=TRUE, 
                     widths=unit(c(1,1), c("null", "null")), 
                     heights=unit(c(1,.5), c("null", "null"))), 
  gp=gpar(cex=1, fill=NA)
))
vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }

## Create top left panel
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
topLeftPanel <- PolarPeriodic(dsLinear=portfolioPolar$dsObservedPolar, portfolioPolar$dsStageCyclePolar, yName="Radius", 
                              stageIDName="StageID", #graphCeiling=7, 
                              cardinalLabels=c("Jan1", "Apr1", "July1", "Oct1"))
upViewport()

## Create top right panel
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
topRighttPanel <- PolarPeriodic(dsLinear=portfolioPolar$dsObservedPolar, portfolioPolar$dsStageCyclePolar, yName="Radius", 
                              stageIDName="StageID", #graphCeiling=7, 
                              drawObservedLine=F,
                              cardinalLabels=c("Jan1", "Apr1", "July1", "Oct1"), originLabel=NULL)
upViewport()

## Create bottom panel
pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2, gp=gpar(cex=1)))
print(cartesianPeriodic, vp=vpLayout(1:2, 2))
upViewport()


## ----session_info, echo=FALSE--------------------------------------------
cat("Report created by", Sys.info()["user"], "at", strftime(Sys.time(), "%c, %z"))
sessionInfo()


