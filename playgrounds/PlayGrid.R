rm(list=ls(all=TRUE))
# opar <- par(no.readonly=TRUE)
library(colorspace)
library(grid)
library(plyr)
library(MASS) #For rlm
library(lubridate)
library(gridBase)
filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
pathDirectoryOutput <- "./PublicationGraphs"

#path <- "F:/Projects/RDev/WatsStaging/Datasets/BirthRatesRogers.csv"
#path <- "F:/Projects/RDev/WatsStaging/Datasets/BirthRatesTulsa.csv"
dsLinear <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=F)
dsLinear$Date <- as.Date(dsLinear$Date)
dsLinear$MonthID <- NULL
summary(dsLinear)
sapply(dsLinear, class)
ticksPerCycle <- 365
sampleSize <- nrow(dsLinear)
dsLinear$Date <- dsLinear$Date + 14 #Add fourteen days to the first of each month.
# dsLinear$Date
# as.POSIXlt(dsLinear$Date)$yday
startDate <- as.Date("1990-01-01")
changeDate <- as.Date("1996-04-01")
firstYear <- year(startDate)
changePoint <- 74 #The 74th month is April 1996
monthsPerYear <- 12
monthCount <- nrow(dsLinear)
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
dsLinear$StageID <- as.numeric(cut.POSIXt(as.POSIXct(dsLinear$Date), breaks=stageBreaks, labels=c("Pre", "Post")))
### Only manipulations specific the sample above this line;
###
lowerQuantile <- .25
upperQuantile <- .75

ddply(.data=dsLinear, .variables=.(MonthIndex, StageID), .fun=nrow)
ds <- dsLinear[ , c("CycleID", "StageID", "Angle", "AngleTotal")]
ds$Radius <- dsLinear$BirthRate
ds$AngleBin <- dsLinear$MonthIndex
#rm(dsLinear)

Bands <- function( Radius, ...) {
  return( c(Lower=as.numeric(quantile(Radius, lowerQuantile)), Upper=as.numeric(quantile(Radius, upperQuantile))) )
}
dsBands <- ddply(.data=ds[, c("AngleBin", "StageID", "Radius")], .variables=.(AngleBin, StageID), .fun=splat(Bands))
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


offset <- 240
colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2] #h=120
smoothedLinear <- hcl(h=40, c=150, l=45)
lineColors <- c(rep(colorBefore, times=changePoint), rep(colorAfter, times=monthCount-changePoint))
transparencyFocus <- .5
transparencyBackground <- .3

bandColorBefore <- c(adjustcolor(colorBefore, transparencyFocus), adjustcolor(colorBefore, transparencyBackground))
bandColorAfter <- c(adjustcolor(colorAfter, transparencyBackground), adjustcolor(colorAfter, transparencyFocus))
# c1 <- c("orange", "blue")
c1 <- c(colorBefore, colorAfter)
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
dsCart$StageID <- floor(approx(x=ds$StageID, n=interpolationPointsTotal)$y)

dsCartBands <- data.frame(StageID=rep(stageIDs, each=interpolationPointsPerCycle), XLower=NA_real_, YLower=NA_real_, XUpper=NA_real_, YUpper=NA_real_)
for( stageID in stageIDs ) {
  dsStageBands <- dsBands[dsBands$StageID==stageID, ]

  interpolatedLowerBand <- approx(x=dsStageBands$Angle, y=dsStageBands$Lower, n=interpolationPointsPerCycle)
  dsCartBands[dsCartBands$StageID==stageID, "XLower"] <- (interpolatedLowerBand$y - graphFloor) * sin(interpolatedLowerBand$x)
  dsCartBands[dsCartBands$StageID==stageID, "YLower"] <- (interpolatedLowerBand$y - graphFloor) * cos(interpolatedLowerBand$x)

  interpolatedUpperBand <- approx(x=dsStageBands$Angle, y=dsStageBands$Upper, n=interpolationPointsPerCycle)
  dsCartBands[dsCartBands$StageID==stageID, "XUpper"] <- (interpolatedUpperBand$y - graphFloor) * sin(interpolatedUpperBand$x)
  dsCartBands[dsCartBands$StageID==stageID, "YUpper"] <- (interpolatedUpperBand$y - graphFloor) * cos(interpolatedUpperBand$x)
}
rm(dsStageBands)

# curve(sinpi(x*2), 0, 6)

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

windows.options(antialias = "cleartype")
# if( names(dev.cur()) != "null device" ) dev.off()
# scale <- 2
#Linear is 1/3 the height of the polars
deviceWidth <- 6.5*4/3 #20 #10 #6.5
# deviceWidth <- 9/2.8 * 1.5 #For the solo polar

height <- 6.5 #For the two big ones
# height <- 6.5/3 #For the solo linears
# height <- 6.5/2 *1.5 #For the solo polar
# windows(width=deviceWidth, height=height)
# opar <- par(no.readonly=TRUE)
# pdf(file=file.path(pathDirectoryOutput, "WatsWorkingPlot.pdf"), width=deviceWidth, height=height)
# png(file=file.path(pathDirectoryOutput, "WatsWorkingPlot.png"), width=deviceWidth, height=height, units="in", res=1200)
# opar <- par(no.readonly=TRUE)


grid.newpage()

# pushViewport(viewport(layout=grid.layout(nrow=1, ncol=1, respect=T), gp=gpar(cex=0.6, fill=NA)))
pushViewport(viewport(layout=grid.layout(nrow=2, ncol=2, respect=T, widths=unit(c(1,1), c("null", "null")), heights=unit(c(1,.5), c("null", "null"))), gp=gpar(cex=0.6, fill=NA)))
# pushViewport(viewport(layout=grid.layout(nrow=2, ncol=2, respect=F, widths=unit(c(1,1), c("null", "null")), heights=unit(c(1,.5), c("null", "null"))), gp=gpar(cex=0.6, fill=NA)))

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
grid.text(c("Jan1", "Apr1", "July1", "Oct1"), x=c(0, 2, 0, -2), y=c(2, 0, -2, 0), gp=gpar(cex=2, col="gray50"), default.units="native")
grid.text(c("A point at the origin represents a GFR of 5"), x=c(0), y=c(-2.2), gp=gpar(cex=1.5, col="gray70"), default.units="native")

# grid.text(label=1:nrow(dsCart), x=dsCart$X, y=dsCart$Y, default.units="native")
# grid.points(x=dsCart$X[c(1, 75, 76, nrow(dsCart))], y=dsCart$Y[c(1, 75, 76, nrow(dsCart))]) #Works when there's no interpolation

for( stageID in stageIDs ) {
  lowerX <- dsCartBands[dsCartBands$StageID==stageID, "XLower"]
  lowerY <- dsCartBands[dsCartBands$StageID==stageID, "YLower"]
  upperX <- dsCartBands[dsCartBands$StageID==stageID, "XUpper"]
  upperY <- dsCartBands[dsCartBands$StageID==stageID, "YUpper"]

  x <- c(lowerX, rev(upperX))
  y <- c(lowerY, rev(upperY))
#   grid.polygon(x=x, y=y, default.units="native", gp=gpar(fill=c2[stageID], col="transparent"))
}

# for( stageID in sort(unique(ds$StageID)) ) { #for( stageID in 2 ) {
#   grid.lines(x=dsCart[dsCart$StageID==stageID, "X"], y=dsCart[dsCart$StageID==stageID, "Y"], gp=gpar(col=c1[stageID], lwd=.2), default.units="native", name="l") #summary(lg) #lg$gp
# }
#lg <- polylineGrob(x=dsCart$X, y=dsCart$Y, id=dsCart$StageID, gp=gpar(col=c1, lwd=.2), default.units="native", name="l") #summary(lg) #lg$gp
lg <- polylineGrob(x=dsCart$X, y=dsCart$Y, id=dsCart$StageID, gp=gpar(col=c1, lwd=2), default.units="native", name="l") #summary(lg) #lg$gp
grid.draw(lg)

upViewport(n=3)
# dev.off()

###
### Top right pane
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
#
#
# ####################################################################################################
# ####################################################################################################
# ### Bottom pane
# ####################################################################################################
# ####################################################################################################
#
#
#
# linearVPRangeX <- range(dsLinear$Horizontal)
# linearVPRangeY <- range(dsLinear$BirthRate)
# pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
# pushViewport(plotViewport(c(0, 0, 0, 0)))
# pushViewport(dataViewport(xscale=linearVPRangeX, yscale=linearVPRangeY, name="plotRegion"))
# # grid.lines(dsLinear$Horizontal, dsLinear$BirthRate, default.units="native")
# # grid.points(dsLinear$Horizontal, dsLinear$BirthRate, default.units="native")
# # upViewport(n=3)
#
# #opar <- par(no.readonly=TRUE, new=TRUE, pty="m",  mar=c(5, 4, 0, 1) + 0.1) #When it's plotted with the polars)
# # opar <- par(no.readonly=TRUE, pty="m",  mar=c(0, 0, 0, 1) + 0.1) #When it's plotted with the polars)
# # par(fig=gridFIG())
# # par(new=TRUE)#, pty="m",  mar=c(5, 4, 0, 1) + 0.1) #When it's plotted with the polars)
# par(fig=gridFIG())
# # par(new=TRUE)
#
# yearCount <- monthCount / monthsPerYear
#
# gridColor <- gray(.9)
# labelColor <- gray(.7)
# polarGridLty <- 3
# interpolationPoints <- 0
# interpolatedCount <- (monthCount - 1) * interpolationPoints + monthCount
# #For Okc
# graphCeiling <- 7
# graphFloor <- 5
# yAxisTicks <- c(5, 6, 7)
# xOffset <- -.5 #So the points are plotted in the middle of the month.
#
#
# # op <- par( pty="s", mar=c(1, 0, 0, 0) + 0.1)
#
#
#
# # par(pty="m",  mar=c(.8, 4, 1, 1) + 0.1) #When it's plotted by itself
# #par(pty="m",  mar=c(5, 4, 0, 1) + 0.1) #When it's plotted with the polars
# par(pty="m",  mar=c(2, 4, 0, 1) + 0.1) #When it's plotted with the polars
# plot(NA, xlim=c(0, monthCount), ylim=c(graphFloor, graphCeiling), type="n", xaxt="n", xaxs="i", yaxt="n", yaxs="i", bty="n",
#      #  ylab="General Fertility Rate", xlab="",#xlab="Time",
#      ylab="", xlab="",#xlab="Time",
# #      sub=paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the before and after periods)"),
#      col.sub=labelColor, cex.lab=1.2)
#
# mtext(side=1, line=1, paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the before and after periods)"), col=labelColor)
# axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
#      col=gridColor, line=-1, tick=F, col.axis=colorBefore, cex.axis=1.5)
# axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
#      col=gridColor, line=-1, tick=F, col.axis=colorAfter, cex.axis=1.5)
# axis(2, at=yAxisTicks, col=labelColor, col.axis=labelColor, line=-0, tick=T, cex.axis=1.5)
# mtext("General Fertility Rate", side=2,line=2.5, cex=1.25)
#
#
# #  axis(1, at=seq(from=6, to=(monthCount), by=6), labels=rep(c("(Jun)", "(Dec)"), 5), col.axis=labelColor, line=0, tick=F, lty=0, cex.axis=.7)
#
#
# linearVerticesXPre <- rep(NA,changePoint)
# linearVerticesXPost <- numeric(0)
# linearBeforeLowerVerticesYPre <- rep(NA,changePoint)
# linearBeforeUpperVerticesYPre <- rep(NA,changePoint)
# linearBeforeLowerVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
# linearBeforeUpperVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
# linearAfterLowerVerticesYPre <- rep(NA,changePoint)
# linearAfterUpperVerticesYPre <- rep(NA,changePoint)
# linearAfterLowerVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
# linearAfterUpperVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
# monthOffset <- rep(0:(yearCount-1), each=monthsPerYear) * monthsPerYear
#
# ds <- data.frame(matrix(NA, nrow=monthCount, ncol=6))
# colnames(ds) <- c("MonthID", "MonthIndex", "Radians", "BirthRate", "X", "Y")
#
# for( yearIndex in 1:yearCount ) {
#   for( monthIndex in 1:monthsPerYear ) {
#     monthID <- (yearIndex - 1) * monthsPerYear + monthIndex
#
#     ds[monthID, 'MonthID'] <- monthID
#     ds[monthID, 'MonthIndex'] <- monthIndex
#     degrees <- monthIndex * (360 / monthsPerYear)
#     ds[monthID, 'Radians'] <- degrees / 180 * pi
#     ds[monthID, 'BirthRate'] <- dsLinear$BirthRate[monthID] - graphFloor
#   }
# }
# ds$X <- ds$BirthRate * sin(ds$Radians)
# ds$Y <- ds$BirthRate * cos(ds$Radians)
# abline(v=seq(from=monthsPerYear, to=monthCount, by=monthsPerYear), col=gridColor, lty=2)
# for( i in 2:monthCount ) {
#   x1 <- ds[i-1, 'MonthID'] + xOffset
#   x2 <- ds[i, 'MonthID'] + xOffset
#   y1 <- ds[i-1, 'BirthRate'] + graphFloor
#   y2 <- ds[i, 'BirthRate'] + graphFloor
#   lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
# }
# abline(v=changePoint + xOffset, col=colorAfter)
# mtext("Bombing Effect", side=3, at=changePoint + xOffset, col=colorAfter, cex=.8)
#
# #maxRate <- max(ds$BirthRate)
# tail(ds)
#
# dsInterpolated <-data.frame(matrix(NA, nrow=interpolatedCount, ncol=6))
# colnames(dsInterpolated) <- c("DurationID", "CycleID", "Radians", "BirthRate", "X", "Y")
# dsInterpolated[1, ] <- ds[1, ]
# rowTally <- 1
# for( pointIndex in 2:monthCount ) {
#   monthStart <- ds[pointIndex - 1, 'MonthIndex']
#   monthStop <- ds[pointIndex, 'MonthIndex']
#   birthStart <- ds[pointIndex - 1, 'BirthRate']
#   birthStop <- ds[pointIndex, 'BirthRate']
#
#   for( interpolationIndex in 1:(interpolationPoints+1) ) {
#     if( monthStop < monthStart ) monthStart <- 0
#     proportion <- interpolationIndex / (interpolationPoints + 1)
#     monthEst <- monthStart + proportion * (monthStop - monthStart)
#     birthRateEst <- birthStart + proportion * (birthStop - birthStart)
#     degrees <- monthEst * (360 / monthsPerYear)
#     radians <- degrees / 180 * pi
#     rowTally <- rowTally + 1
#     dsInterpolated[rowTally, 'DurationID'] <- rowTally
#     dsInterpolated[rowTally, 'CycleID'] <- monthEst
#     dsInterpolated[rowTally, 'Radians'] <- radians
#     dsInterpolated[rowTally, 'BirthRate'] <- birthRateEst
#   }
# }
# dsInterpolated$X <- dsInterpolated$BirthRate * sin(dsInterpolated$Radians)
# dsInterpolated$Y <- dsInterpolated$BirthRate * cos(dsInterpolated$Radians)
#
# dsBands <- data.frame(matrix(NA, nrow=length(unique(dsInterpolated$CycleID)), ncol=14))
# colnames(dsBands) <- c("CycleID", "Radians", "LowerBefore", "UpperBefore", "LowerAfter", "UpperAfter",
#                        "LowerBeforeX", "LowerBeforeY", "UpperBeforeX", "UpperBeforeY", "LowerAfterX", "LowerAfterY", "UpperAfterX", "UpperAfterY")
#
# rowTally <- 1
# for( cycleID in sort(unique(dsInterpolated$CycleID)) ) {
#   dsSliceBefore <- subset(dsInterpolated, CycleID==cycleID & DurationID <= changePoint * (interpolationPoints + 1))
#   dsSliceAfter <- subset(dsInterpolated, CycleID==cycleID & DurationID > changePoint * (interpolationPoints + 1))
#
#   dsBands[rowTally, 'CycleID'] <- cycleID
#   dsBands[rowTally, 'Radians'] <- dsSliceBefore$Radians[1] #They should all have the same radians
#   #   dsBands[rowTally, 'LowerBefore'] <- quantile(dsSliceBefore$BirthRate, prob=lowerQuantile)
#   #   dsBands[rowTally, 'UpperBefore'] <- quantile(dsSliceBefore$BirthRate, prob=upperQuantile)
#   #   dsBands[rowTally, 'LowerAfter'] <- quantile(dsSliceAfter$BirthRate, prob=lowerQuantile)
#   #   dsBands[rowTally, 'UpperAfter'] <- quantile(dsSliceAfter$BirthRate, prob=upperQuantile)
#   dsBands[rowTally, 'LowerBefore'] <- as.numeric(quantile(dsSliceBefore$BirthRate, prob=lowerQuantile))
#   dsBands[rowTally, 'UpperBefore'] <- as.numeric(quantile(dsSliceBefore$BirthRate, prob=upperQuantile))
#   dsBands[rowTally, 'LowerAfter'] <- as.numeric(quantile(dsSliceAfter$BirthRate, prob=lowerQuantile))
#   dsBands[rowTally, 'UpperAfter'] <- as.numeric(quantile(dsSliceAfter$BirthRate, prob=upperQuantile))
#
#   rowTally <- rowTally + 1
# }
#
# rowTally <- NA
# dsBands$LowerBeforeX <- dsBands$LowerBefore * sin(dsBands$Radians)
# dsBands$LowerBeforeY <- dsBands$LowerBefore * cos(dsBands$Radians)
# dsBands$UpperBeforeX <- dsBands$UpperBefore * sin(dsBands$Radians)
# dsBands$UpperBeforeY <- dsBands$UpperBefore * cos(dsBands$Radians)
# dsBands$LowerAfterX <- dsBands$LowerAfter * sin(dsBands$Radians)
# dsBands$LowerAfterY <- dsBands$LowerAfter * cos(dsBands$Radians)
# dsBands$UpperAfterX <- dsBands$UpperAfter * sin(dsBands$Radians)
# dsBands$UpperAfterY <- dsBands$UpperAfter * cos(dsBands$Radians)
#
# for( monthID in 1:changePoint ) {
#   desiredCycle <- monthID %% monthsPerYear
#   if(desiredCycle==0) desiredCycle <- 12
#
#   linearVerticesXPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$CycleID + monthOffset[monthID]
#   linearBeforeLowerVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerBefore
#   linearBeforeUpperVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperBefore
#   linearAfterLowerVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerAfter
#   linearAfterUpperVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperAfter
# }
# for( monthID in (changePoint+1):monthCount ) {
#   desiredCycle <- monthID %% monthsPerYear
#   if(desiredCycle==0) desiredCycle <- 12
#
#   linearVerticesXPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$CycleID + monthOffset[monthID]
#   linearBeforeLowerVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerBefore
#   linearBeforeUpperVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperBefore
#   linearAfterLowerVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerAfter
#   linearAfterUpperVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperAfter
# }
#
# linearVerticesXPre <- c(linearVerticesXPre, rev(linearVerticesXPre))
# linearVerticesXPost <- c(linearVerticesXPost, rev(linearVerticesXPost))
#
# linearBeforeVerticesYPre <- c(linearBeforeLowerVerticesYPre, rev(linearBeforeUpperVerticesYPre))+graphFloor
# linearAfterVerticesYPre <- c(linearAfterLowerVerticesYPre, rev(linearAfterUpperVerticesYPre))+graphFloor
# linearBeforeVerticesYPost <- c(linearBeforeLowerVerticesYPost, rev(linearBeforeUpperVerticesYPost))+graphFloor
# linearAfterVerticesYPost <- c(linearAfterLowerVerticesYPost, rev(linearAfterUpperVerticesYPost))+graphFloor
#
# polygon(x=linearVerticesXPre + xOffset, y=linearBeforeVerticesYPre, border=NA, col=bandColorBefore[1])
# polygon(x=linearVerticesXPre + xOffset, y=linearAfterVerticesYPre, border=NA, col=bandColorAfter[1])
# polygon(x=linearVerticesXPost + xOffset, y=linearBeforeVerticesYPost, border=NA, col=bandColorBefore[2])
# polygon(x=linearVerticesXPost + xOffset, y=linearAfterVerticesYPost, border=NA, col=bandColorAfter[2])
# # par(op)
#
# # dev.off()
# ###
# ### Debugging code
# ###
# # grid.get("l")$gp
# # grid.remove("l")
# # grid.points(x=dsCart$X, y=dsCart$Y, gp=gpar(cex=.5, col=c1))
# # grid.rect()
# # grid.xaxis()
# # grid.yaxis()
# # grid.points(x=c(-2,-1,0, 1), y=c(0, 0, 0,0), default.units="native")
# # grid.points(x=c(0,0,0,0), y=c(-2, -1, 0, 1), default.units="native")
# #from line 224 "customgrid.Rnw"
# # grid.text("temperature", y=unit(-3, "line"))
# # grid.text("pressure", x=unit(-3, "line"), rot=90)
#
#
# #  showViewport(newpage=TRUE, leaves=TRUE)
# opar <- par(no.readonly=TRUE)