rm(list=ls(all=TRUE))
require(colorspace)
require(grid)
require(plyr)
require(MASS) #For rlm
require(lubridate)
path <- "Datasets/BirthRatesOkc.csv"
#path <- "F:/Projects/RDev/WatsStaging/Datasets/BirthRatesRogers.csv"
#path <- "F:/Projects/RDev/WatsStaging/Datasets/BirthRatesTulsa.csv"
# setwd("F:/Projects/RDev/WatsStaging/Images/")
#dsLinear <- read.csv(path, colClasses=c("POSIXct", "integer", "numeric"))
dsLinear <- read.csv(path, colClasses=c("Date", "integer", "numeric"))
# # summary(dsLinear)
# class(dsLinear$Date)
ticksPerCycle <- 365
sampleSize <- nrow(dsLinear)
dsLinear$Date <- dsLinear$Date + 14 #Add fourteen days to the first of each month.
# dsLinear$Date
# as.POSIXlt(dsLinear$Date)$yday
startDate <- as.Date("1990-01-01")
changeDate <- as.Date("1996-04-01")
#changePoint <- 74 #The 74th month is April 1996
#fullFebruaryCount <- 9 #The number of Februaries with a full preceeding 12 month period.

#dsLinear$Tick <- as.POSIXlt(dsLinear$Date)$yday
dsLinear$Tick <- lubridate::yday(dsLinear$Date)

# dsLinear$CycleID <- as.POSIXlt(dsLinear$Date)$year - startDate$year
dsLinear$CycleID <- year(dsLinear$Date) - year(startDate)
dsLinear$Angle <- (dsLinear$Tick / ticksPerCycle) * 2* pi
dsLinear$AngleTotal <- (dsLinear$CycleID * 2* pi) + dsLinear$Angle
#dsLinear$MonthIndex <- as.POSIXlt(dsLinear$Date)$mon
dsLinear$MonthIndex <- month(dsLinear$Date)
dsLinear$Horizontal <- dsLinear$CycleID + (dsLinear$MonthIndex-1)/12

#totalPositionCount <- range(dsLinear$Date)[2]-range(dsLinear$Date)[1]
#cycleCount <- monthCount / ticksPerCycle
stageIDs <- 1:2
stageCount <- length(stageIDs)
stageBreaks <- as.POSIXct(c(startDate, changeDate, max(dsLinear$Date)+1))
dsLinear$Stage <- as.numeric(cut.POSIXt(as.POSIXct(dsLinear$Date), breaks=stageBreaks, labels=c("Pre", "Post")))
### Only manipulations specific the sample above this line;
###
lowerQuantile <- .25
upperQuantile <- .75

ddply(.data=dsLinear, .variables=.(MonthIndex, Stage), .fun=nrow)
ds <- dsLinear[ , c("CycleID", "Stage", "Angle", "AngleTotal")]
ds$Radius <- dsLinear$BirthRate
ds$AngleBin <- dsLinear$MonthIndex
#rm(dsLinear)

Bands <- function( Radius, ...) { return( c(Lower=as.numeric(quantile(Radius, lowerQuantile)), Upper=as.numeric(quantile(Radius, upperQuantile))) ) }
dsBands <- ddply(.data=ds[, c("AngleBin", "Stage", "Radius")], .variables=.(AngleBin, Stage), .fun=splat(Bands))
dsBands$Angle <- NA
for( binIndex in sort(unique(dsBands$AngleBin)) ) {
  meanAngle <- mean(ds[ds$AngleBin==binIndex, "Angle"], na.rm=T)
  dsBands[dsBands$AngleBin==binIndex, "Angle"] <- meanAngle
}
dsExtra <- dsBands[1:2, ]
#dsExtra$AngleBin <- max(dsBands$AngleBin)+1
dsExtra$AngleBin <- max(dsBands$AngleBin)
dsExtra$Angle <- dsBands[1:stageCount, "Angle"] + 2*pi
dsBands <- rbind(dsBands, dsExtra)

# dsBands[1:stageCount, "AngleBin"] <- max(dsBands$AngleBin)+1
# dsBands[1:stageCount, "Angle"] <- dsBands[1:stageCount, "Angle"] + 2*pi
# dsBands[25:26, "AngleBin"] <- 12
# dsBands[25:26, "Angle"] <- dsBands[1:2, "Angle"] + 2*pi
  
tail(ds)

###
### Only manipulations specific to graphic in this section
###
c1 <- c("orange", "blue")
c2 <- adjustcolor(c1, alpha.f=.5)

# xOffset <- -.5 #So the points are plotted in the middle of the month.
# For Okc
graphCeiling <- 7
graphFloor <- 5
yAxisTicks <- c(5, 6, 7)
graphHeight <- graphCeiling - graphFloor
interpolationPointsPerCycle <- 12*100
totalCycles <- max(ds$CycleID, na.rm=T) - min(ds$CycleID, na.rm=T) + 1
interpolationPointsTotal <- interpolationPointsPerCycle*totalCycles

interpolatedLine <- approx(x=ds$AngleTotal, y=ds$Radius, n=interpolationPointsTotal)

dsCart <- data.frame(X=rep(NA_real_, length(interpolatedLine$x)), Y=NA_real_)
dsCart$X <- (interpolatedLine$y - graphFloor) * sin(interpolatedLine$x)
dsCart$Y <- (interpolatedLine$y - graphFloor) * cos(interpolatedLine$x)
dsCart$StageID <- floor(approx(x=ds$Stage, n=interpolationPointsTotal)$y)

dsCartBands <- data.frame(StageID=rep(stageIDs, each=interpolationPointsPerCycle), XLower=NA_real_, YLower=NA_real_, XUpper=NA_real_, YUpper=NA_real_)
for( stageID in stageIDs ) {
  dsStageBands <- dsBands[dsBands$Stage==stageID, ]
  
  interpolatedLowerBand <- approx(x=dsStageBands$Angle, y=dsStageBands$Lower, n=interpolationPointsPerCycle)
  dsCartBands[dsCartBands$Stage==stageID, "XLower"] <- (interpolatedLowerBand$y - graphFloor) * sin(interpolatedLowerBand$x)
  dsCartBands[dsCartBands$Stage==stageID, "YLower"] <- (interpolatedLowerBand$y - graphFloor) * cos(interpolatedLowerBand$x)
                 
  interpolatedUpperBand <- approx(x=dsStageBands$Angle, y=dsStageBands$Upper, n=interpolationPointsPerCycle)
  dsCartBands[dsCartBands$Stage==stageID, "XUpper"] <- (interpolatedUpperBand$y - graphFloor) * sin(interpolatedUpperBand$x)
  dsCartBands[dsCartBands$Stage==stageID, "YUpper"] <- (interpolatedUpperBand$y - graphFloor) * cos(interpolatedUpperBand$x)
}
rm(dsStageBands)


###
### No graphics above this line; no manipulation below this line.
###

# if( names(dev.cur()) != "null device" ) dev.off()
# scale <- 2
# deviceWidth <-9 #20 #10 #6.5
# heightToWidthRatio <- 1
# height <- deviceWidth * heightToWidthRatio
# windows(width=deviceWidth, height=height)

vpRange <- c(-graphHeight, graphHeight) * 1.02



###
### Graphics settings above this line; drawing below this line.
###
grid.newpage()

#pushViewport(viewport(layout=grid.layout(nrow=1, ncol=1, respect=T), gp=gpar(cex=0.6, fill=NA)))
pushViewport(viewport(layout=grid.layout(nrow=2, ncol=2, respect=T, widths=unit(c(1,1), c("null", "null")), heights=unit(c(1,.5), c("null", "null"))), gp=gpar(cex=0.6, fill=NA)))

###
### Top left pane
###
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))

pushViewport(plotViewport(c(2, 2, 2, 2))) # pushViewport(plotViewport(c(0)))
pushViewport(dataViewport(xscale=vpRange, yscale=vpRange, name="plotRegion"))
# grid.abline(intercept=0, slope=0, gp=gpar(col="gray80"))
# grid.abline(intercept=0, slope=1e300, gp=gpar(col="gray80"))
grid.lines(x=c(-2,2), y=c(0,0), gp=gpar(col="gray80"), default.units="native")
grid.lines(x=c(0,0), y=c(-2,2), gp=gpar(col="gray80"), default.units="native")
grid.circle(x=0, y=0, r=0:2, default.units="native", gp=gpar(col="gray80"))
grid.text(c("Jan1", "Mar1", "July1", "Oct1"), x=c(0, 2, 0, -2), y=c(2, 0, -2, 0), gp=gpar(cex=2, col="gray50"), default.units="native")

# grid.text(label=1:nrow(dsCart), x=dsCart$X, y=dsCart$Y, default.units="native")
# grid.points(x=dsCart$X[c(1, 75, 76, nrow(dsCart))], y=dsCart$Y[c(1, 75, 76, nrow(dsCart))]) #Works when there's no interpolation

for( stageID in stageIDs ) {
  lowerX <- dsCartBands[dsCartBands$StageID==stageID, "XLower"]
  lowerY <- dsCartBands[dsCartBands$StageID==stageID, "YLower"]
  upperX <- dsCartBands[dsCartBands$StageID==stageID, "XUpper"]
  upperY <- dsCartBands[dsCartBands$StageID==stageID, "YUpper"]  
  
  x <- c(lowerX, rev(upperX))
  y <- c(lowerY, rev(upperY))
  grid.polygon(x=x, y=y, default.units="native", gp=gpar(fill=c2[stageID], col="transparent"))
}

# for( stageID in sort(unique(ds$Stage)) ) { #for( stageID in 2 ) {
#   grid.lines(x=dsCart[dsCart$Stage==stageID, "X"], y=dsCart[dsCart$Stage==stageID, "Y"], gp=gpar(col=c1[stageID], lwd=.2), default.units="native", name="l") #summary(lg) #lg$gp  
# }
lg <- polylineGrob(x=dsCart$X, y=dsCart$Y, id=dsCart$StageID, gp=gpar(col=c1, lwd=.2), default.units="native", name="l") #summary(lg) #lg$gp
grid.draw(lg)

upViewport(n=3)


###
### Top left pane
###
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
pushViewport(plotViewport(c(2, 2, 2, 2))) 
pushViewport(dataViewport(xscale=vpRange, yscale=vpRange, name="plotRegion"))
grid.lines(x=c(-2,2), y=c(0,0), gp=gpar(col="gray80"), default.units="native")
grid.lines(x=c(0,0), y=c(-2,2), gp=gpar(col="gray80"), default.units="native")
grid.circle(x=0, y=0, r=0:2, default.units="native", gp=gpar(col="gray80"))

for( stageID in stageIDs ) {
  lowerX <- dsCartBands[dsCartBands$StageID==stageID, "XLower"]
  lowerY <- dsCartBands[dsCartBands$StageID==stageID, "YLower"]
  upperX <- dsCartBands[dsCartBands$StageID==stageID, "XUpper"]
  upperY <- dsCartBands[dsCartBands$StageID==stageID, "YUpper"]  
  
  x <- c(lowerX, rev(upperX))
  y <- c(lowerY, rev(upperY))
  grid.polygon(x=x, y=y, default.units="native", gp=gpar(fill=c2[stageID], col="transparent"))
}
rm(x, y)
upViewport(n=3)


###
### Bottom pane
###
linearVPRangeX <- range(dsLinear$Horizontal)
linearVPRangeY <- range(dsLinear$BirthRate)
pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
pushViewport(plotViewport(c(2, 2, 2, 2))) 
pushViewport(dataViewport(xscale=linearVPRangeX, yscale=linearVPRangeY, name="plotRegion"))
# grid.lines(x=c(-2,2), y=c(0,0), gp=gpar(col="gray80"), default.units="native")
# grid.lines(x=c(0,0), y=c(-2,2), gp=gpar(col="gray80"), default.units="native")
# grid.circle(x=0, y=0, r=0:2, default.units="native", gp=gpar(col="gray80"))
# 
# for( stageID in stageIDs ) {
#   lowerX <- dsCartBands[dsCartBands$StageID==stageID, "XLower"]
#   lowerY <- dsCartBands[dsCartBands$StageID==stageID, "YLower"]
#   upperX <- dsCartBands[dsCartBands$StageID==stageID, "XUpper"]
#   upperY <- dsCartBands[dsCartBands$StageID==stageID, "YUpper"]  
#   
#   x <- c(lowerX, rev(upperX))
#   y <- c(lowerY, rev(upperY))
#   grid.polygon(x=x, y=y, default.units="native", gp=gpar(fill=c2[stageID], col="transparent"))
# }
# dv <- dataViewport(xData=x, yData=y, name="plotRegion")
# pushViewport(dv)
#pushViewport(viewport())
# grid.rect()
# grid.xaxis()
# grid.yaxis()


grid.lines(dsLinear$Horizontal, dsLinear$BirthRate, default.units="native")
grid.points(dsLinear$Horizontal, dsLinear$BirthRate, default.units="native")

upViewport(n=3)


###
### Debugging code
###
# grid.get("l")$gp
# grid.remove("l")
# grid.points(x=dsCart$X, y=dsCart$Y, gp=gpar(cex=.5, col=c1))
# grid.rect()
# grid.xaxis()
# grid.yaxis()
# grid.points(x=c(-2,-1,0, 1), y=c(0, 0, 0,0), default.units="native")
# grid.points(x=c(0,0,0,0), y=c(-2, -1, 0, 1), default.units="native")
#from line 224 "customgrid.Rnw"
# grid.text("temperature", y=unit(-3, "line"))
# grid.text("pressure", x=unit(-3, "line"), rot=90)


#  showViewport(newpage=TRUE, leaves=TRUE)
