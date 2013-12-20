#This next line is run when the whole file is executed, but not when knitr calls individual chunks.
rm(list=ls(all=TRUE)) #Clear the memory for any variables set from any previous runs.

## @knitr load_packages
############################
library(Wats)
library(grid)
library(ggplot2) 

## @knitr Figure2IndividualBasic
############################
filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
dsLinear <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
dsLinear$Date <- as.Date(dsLinear$Date) 
dsLinear$MonthID <- NULL
changeMonth <- as.Date("1996-02-15")
dsLinear$StageID <- ifelse(dsLinear$Date < changeMonth, 1L, 2L)
dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")

hSpread <- function( scores) { return( quantile(x=scores, probs=c(.25, .75)) ) }
portfolio <- AnnotateData(dsLinear, dvName="BirthRate",centerFunction=median, spreadFunction=hSpread)

CartesianRolling(dsLinear=portfolio$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, changePointLabels="Bombing Effect")

## @knitr Figure2IndividualStylized
############################
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
yScale <- scale_y_continuous(breaks=5:7)
yExpand <- expand_limits(y=c(5, 7))

topPanel <- CartesianRolling(dsLinear=portfolio$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, yTitle="General Fertility Rate", 
                             changePointLabels="Bombing Effect", 
                             drawRollingBand=FALSE, 
                             drawRollingLine=FALSE)
middlePanel <- CartesianRolling(portfolio$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, yTitle="General Fertility Rate",
                                changePointLabels="", 
                                drawRollingBand=FALSE, 
                                drawJaggedLine=FALSE)
bottomPanel <- CartesianRolling(portfolio$dsLinear, xName="Date", yName="BirthRate", stageIDName="StageID", changePoints=changeMonth, yTitle="General Fertility Rate", 
                                changePointLabels="", 
                                drawJaggedLine=FALSE)
topPanel <- topPanel + xScale + yScale + yExpand + fig2Theme 
middlePanel <- middlePanel + xScale + yScale + yExpand + fig2Theme
bottomPanel <- bottomPanel + xScaleBlank + yScale + yExpand + fig2Theme

topPanel
middlePanel
bottomPanel

## @knitr Figure2Combined
############################
vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,1)))
print(topPanel, vp=vpLayout(1, 1))
print(middlePanel, vp=vpLayout(2, 1))
print(bottomPanel, vp=vpLayout(3, 1))
popViewport()

## @knitr Figure4Basic
############################
cartesianPeriodicSimple <- CartesianPeriodic(
  portfolio$dsLinear, 
  portfolio$dsPeriodic, 
  xName = "Date", 
  yName = "BirthRate",
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  changePointLabels = "Bombing Effect",
  yTitle="General Fertility Rate",
  drawPeriodicBand=FALSE
)
cartesianPeriodicSimple

## @knitr Figure4Stylized
############################
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
yScale <- scale_y_continuous(breaks=5:7)
yExpand <- expand_limits(y=c(5, 7))

cartesianPeriodicSimple <- cartesianPeriodicSimple + xScale + yScale + yExpand + fig4Theme
cartesianPeriodicSimple

## @knitr Figure5Basic
############################
cartesianPeriodic <- CartesianPeriodic(
  portfolio$dsLinear, 
  portfolio$dsPeriodic, 
  xName = "Date", 
  yName = "BirthRate",
  stageIDName = "StageID", 
  changePoints = changeMonth, 
  changePointLabels = "Bombing Effect",
  yTitle="General Fertility Rate",
  drawPeriodicBand=TRUE #The only difference from the simple linear graph above
)
cartesianPeriodic

## @knitr Figure5Stylized
############################
cartesianPeriodic <- cartesianPeriodic + xScale + yScale + yExpand + fig4Theme
cartesianPeriodic
