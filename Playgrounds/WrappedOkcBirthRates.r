rm(list=ls(all=TRUE))
require(colorspace)
# path <- "F:/Users/wibeasley/Documents/School/Circular/BirthRatesOkc.txt"
# #path <- "F:/Users/wibeasley/Documents/School/Circular/BirthRatesRogers.txt"
# #path <- "F:/Users/wibeasley/Documents/School/Circular/BirthRatesTulsa.txt"

pathInput <- "./Datasets/BirthRatesOkc.txt"
pathDirectoryOutput <-  "./PublicationGraphs"
dsOkc <- read.table(pathInput, header=T)
firstYear <- 1990
changePoint <- 74 #The 74th month is April 1996
fullFebruaryCount <- 9 #The number of Februaries with a full preceeding 12 month period.

monthsPerYear <- 12
monthCount <- nrow(dsOkc)
yearCount <- monthCount / monthsPerYear
#lineColors <- terrain.colors(n=monthCount)[monthCount:1]
colorSchemeVersion <-6
if( colorSchemeVersion == 1) {
  colorBefore <- "peru"
  colorAfter <- "springgreen4"
}
if( colorSchemeVersion == 2) {
  offset <- 150+180
  colorBefore <- rainbow_hcl(3, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2]
}
if( colorSchemeVersion == 3) {
  offset <- 300
  colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2]
}
if( colorSchemeVersion == 4) {
  offset <- 40
  colorBefore <- rainbow_hcl(2, start=0+offset, end=180+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=180+offset, c=100, l=65)[2]
}
if( colorSchemeVersion == 5) {
  offset <- 0
  colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2]
}
if( colorSchemeVersion == 6) {
  offset <- 240
  colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2] #h=120
  smoothedLinear <- hcl(h=40, c=150, l=45)
}
windows.options(antialias = "cleartype")
# if( names(dev.cur()) != "null device" ) dev.off()
# scale <- 2
#Linear is 1/3 the height of the polars
deviceWidth <- 9 #20 #10 #6.5
# deviceWidth <- 9/2.8 * 1.5 #For the solo polar

height <- 6.5 #For the two big ones
# height <- 6.5/3 #For the solo linears
# height <- 6.5/2 *1.5 #For the solor polar
#windows(width=deviceWidth, height=height)
# pdf(file=file.path(pathDirectoryOutput, "WatsWorkingPlot.pdf"), width=deviceWidth, height=height)
# png(file=file.path(pathDirectoryOutput, "WatsWorkingPlot.png"), width=deviceWidth, height=height, units="in", res=1200)



lineColors <- c(rep(colorBefore, times=changePoint), rep(colorAfter, times=monthCount-changePoint))
transparencyFocus <- .5
transparencyBackground <- .3
#transparencyFocus <- .8
#transparencyBackground <- .4
# transparencyFocus <- 0
# transparencyBackground <- 0
FadeColor <- function( color, transparency ) {
    rgb <- col2rgb(color) / 255 #break it into components and rescale
		color <- rgb(red=rgb[1], green=rgb[2], blue=rgb[3], alpha=transparency)
}
bandColorBefore <- c(FadeColor(colorBefore, transparencyFocus), FadeColor(colorBefore, transparencyBackground))
bandColorAfter <- c(FadeColor(colorAfter, transparencyBackground), FadeColor(colorAfter, transparencyFocus))

gridColor <- gray(.9)
labelColor <- gray(.7)
polarGridLty <- 3
interpolationPoints <- 5
interpolatedCount <- (monthCount - 1) * interpolationPoints + monthCount
#For Okc
graphCeiling <- 7
graphFloor <- 5
yAxisTicks <- c(5, 6, 7)
##For Rogers
#graphCeiling <- 7.2
#graphFloor <- 3.5
#yAxisTicks <- c(4, 5, 6, 7)
##For Tulsa
#graphCeiling <- 7
#graphFloor <- 4.5
#yAxisTicks <- c(5, 6, 7)

graphHeight <- graphCeiling - graphFloor
xOffset <- -.5 #So the points are plotted in the middle of the month.

lowerQuantile <- .25
upperQuantile <- .75

#lowerQuantile <- 0#.25
#upperQuantile <- 1#.75

ds <- data.frame(matrix(NA, nrow=monthCount, ncol=6))
colnames(ds) <- c("MonthID", "MonthIndex", "Radians", "BirthRate", "X", "Y")

for( yearIndex in 1:yearCount ) {
  for( monthIndex in 1:monthsPerYear ) {
    monthID <- (yearIndex - 1) * monthsPerYear + monthIndex

    ds[monthID, 'MonthID'] <- monthID
    ds[monthID, 'MonthIndex'] <- monthIndex
    degrees <- monthIndex * (360 / monthsPerYear)
    ds[monthID, 'Radians'] <- degrees / 180 * pi
    ds[monthID, 'BirthRate'] <- dsOkc$BirthRate[monthID] - graphFloor
  }
}
ds$X <- ds$BirthRate * sin(ds$Radians)
ds$Y <- ds$BirthRate * cos(ds$Radians)
#maxRate <- max(ds$BirthRate)
tail(ds)

dsInterpolated <-data.frame(matrix(NA, nrow=interpolatedCount, ncol=6))
colnames(dsInterpolated) <- c("DurationID", "CycleID", "Radians", "BirthRate", "X", "Y")
dsInterpolated[1, ] <- ds[1, ]
rowTally <- 1
for( pointIndex in 2:monthCount ) {
  monthStart <- ds[pointIndex - 1, 'MonthIndex']
  monthStop <- ds[pointIndex, 'MonthIndex']
  birthStart <- ds[pointIndex - 1, 'BirthRate']
  birthStop <- ds[pointIndex, 'BirthRate']

  for( interpolationIndex in 1:(interpolationPoints+1) ) {
    if( monthStop < monthStart ) monthStart <- 0
    proportion <- interpolationIndex / (interpolationPoints + 1)
    monthEst <- monthStart + proportion * (monthStop - monthStart)
    birthRateEst <- birthStart + proportion * (birthStop - birthStart)
    degrees <- monthEst * (360 / monthsPerYear)
    radians <- degrees / 180 * pi
    rowTally <- rowTally + 1
    dsInterpolated[rowTally, 'DurationID'] <- rowTally
    dsInterpolated[rowTally, 'CycleID'] <- monthEst
    dsInterpolated[rowTally, 'Radians'] <- radians      
    dsInterpolated[rowTally, 'BirthRate'] <- birthRateEst
  }
}
dsInterpolated$X <- dsInterpolated$BirthRate * sin(dsInterpolated$Radians)
dsInterpolated$Y <- dsInterpolated$BirthRate * cos(dsInterpolated$Radians)

dsBands <- data.frame(matrix(NA, nrow=length(unique(dsInterpolated$CycleID)), ncol=14))
colnames(dsBands) <- c("CycleID", "Radians", "LowerBefore", "UpperBefore", "LowerAfter", "UpperAfter", 
  "LowerBeforeX", "LowerBeforeY", "UpperBeforeX", "UpperBeforeY", "LowerAfterX", "LowerAfterY", "UpperAfterX", "UpperAfterY")
  
rowTally <- 1
for( cycleID in sort(unique(dsInterpolated$CycleID)) ) {
  dsSliceBefore <- subset(dsInterpolated, CycleID==cycleID & DurationID <= changePoint * (interpolationPoints + 1))
  dsSliceAfter <- subset(dsInterpolated, CycleID==cycleID & DurationID > changePoint * (interpolationPoints + 1))  

  dsBands[rowTally, 'CycleID'] <- cycleID
  dsBands[rowTally, 'Radians'] <- dsSliceBefore$Radians[1] #They should all have the same radians
#   dsBands[rowTally, 'LowerBefore'] <- quantile(dsSliceBefore$BirthRate, prob=lowerQuantile)
#   dsBands[rowTally, 'UpperBefore'] <- quantile(dsSliceBefore$BirthRate, prob=upperQuantile)
#   dsBands[rowTally, 'LowerAfter'] <- quantile(dsSliceAfter$BirthRate, prob=lowerQuantile)
#   dsBands[rowTally, 'UpperAfter'] <- quantile(dsSliceAfter$BirthRate, prob=upperQuantile)
  dsBands[rowTally, 'LowerBefore'] <- as.numeric(quantile(dsSliceBefore$BirthRate, prob=lowerQuantile))
  dsBands[rowTally, 'UpperBefore'] <- as.numeric(quantile(dsSliceBefore$BirthRate, prob=upperQuantile))
  dsBands[rowTally, 'LowerAfter'] <- as.numeric(quantile(dsSliceAfter$BirthRate, prob=lowerQuantile))
  dsBands[rowTally, 'UpperAfter'] <- as.numeric(quantile(dsSliceAfter$BirthRate, prob=upperQuantile))
    
  rowTally <- rowTally + 1
}
rowTally <- NA
dsBands$LowerBeforeX <- dsBands$LowerBefore * sin(dsBands$Radians)
dsBands$LowerBeforeY <- dsBands$LowerBefore * cos(dsBands$Radians)
dsBands$UpperBeforeX <- dsBands$UpperBefore * sin(dsBands$Radians)
dsBands$UpperBeforeY <- dsBands$UpperBefore * cos(dsBands$Radians)
dsBands$LowerAfterX <- dsBands$LowerAfter * sin(dsBands$Radians)
dsBands$LowerAfterY <- dsBands$LowerAfter * cos(dsBands$Radians)
dsBands$UpperAfterX <- dsBands$UpperAfter * sin(dsBands$Radians)
dsBands$UpperAfterY <- dsBands$UpperAfter * cos(dsBands$Radians)


circle <- function(radius, segments=100, color=gray(0)) {
  for( i in 1:length(radius) ) {
    angles <- (0:segments)*2*pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- radius[i] * unit.circle
    lines(ellipse, col=color)
  }
}

PlotPolar <- function( drawLines ) {
  plot(dsInterpolated$X, dsInterpolated$Y, xlim=c(-graphHeight, graphHeight), ylim=c(-graphHeight, graphHeight), type="n", xaxt="n", yaxt="n",  bty="n",
    xlab="", ylab="", cex.axis=1.5)
#    main=substitute(list(beta[Season]==slopeSeasonal, beta[Duration]==slopeDuration, paste(italic(e), "~", italic(N)(0, sigma))),
    #main=substitute(list(beta[Season]==over(1,slopeSeasonalInv), beta[Duration]==over(1,slopeDurationInv), paste(italic(e), "~", italic(N)(0, over(1,sigmaInv)))),
#      list(slopeSeasonalInv=1/slopeSeasonal, slopeDurationInv=1/slopeDuration,sigmaInv=1/sigma))
  if( drawLines ) mtext(paste("A point at the origin represents a GFR of", graphFloor), side=1, col=labelColor, cex=1.2)
  abline(v=0, col=gridColor, lty=polarGridLty)
  abline(h=0, col=gridColor, lty=polarGridLty)
  text(c("Dec","Mar","June","Sept"), x=c(0, graphHeight, 0, -graphHeight), y=c(graphHeight, 0, -graphHeight, 0), xpd=T, col=labelColor, cex=1.5)
  circle(radius=seq(from=0, to=graphHeight, by=graphHeight/3), color=gridColor)

  for( i in 2:interpolatedCount ) {
    x1 <- dsInterpolated[i-1, 'X']
    x2 <- dsInterpolated[i, 'X']
    y1 <- dsInterpolated[i-1, 'Y']
    y2 <- dsInterpolated[i, 'Y']
    if( drawLines ) lines(x=c(x1, x2), y=c(y1, y2),  lwd=1, col=lineColors[floor(i/(interpolationPoints+1))+1])
  }
  polarBeforeVerticesX <- c(dsBands$LowerBeforeX, dsBands$LowerBeforeX[1], dsBands$UpperBeforeX[1], rev(dsBands$UpperBeforeX))
  polarBeforeVerticesY <- c(dsBands$LowerBeforeY, dsBands$LowerBeforeY[1], dsBands$UpperBeforeY[1], rev(dsBands$UpperBeforeY))
  polarAfterVerticesX <- c(dsBands$LowerAfterX, dsBands$LowerAfterX[1], dsBands$UpperAfterX[1], rev(dsBands$UpperAfterX))
  polarAfterVerticesY <- c(dsBands$LowerAfterY, dsBands$LowerAfterY[1], dsBands$UpperAfterY[1], rev(dsBands$UpperAfterY)) 
  polygon(x=polarBeforeVerticesX, y=polarBeforeVerticesY, border=NA, col=bandColorBefore[1])
  polygon(x=polarAfterVerticesX, y=polarAfterVerticesY, border=NA, col=bandColorAfter[2])
}
                         
### Start Graphing ###
layout(rbind(c(1,2), c(3)), heights=c(1,.5))
op <- par( pty="s", mar=c(1, 0, 0, 0) + 0.1)

PlotPolar(drawLines=TRUE)
PlotPolar(drawLines=FALSE)

# par(pty="m",  mar=c(.8, 4, 1, 1) + 0.1) #When it's plotted by itself
par(pty="m",  mar=c(5, 4, 0, 1) + 0.1) #When it's plotted with the polars
plot(NA, xlim=c(0, monthCount), ylim=c(graphFloor, graphCeiling), type="n", xaxt="n", xaxs="i", yaxt="n", yaxs="i", bty="n",
 #  ylab="General Fertility Rate", xlab="",#xlab="Time",
  ylab="", xlab="",#xlab="Time",
  sub=paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the before and after periods)"), 
  col.sub=labelColor, cex.lab=1.2)

  axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
    col=gridColor, line=-1, tick=F, col.axis=colorBefore, cex.axis=1.5)
  axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
    col=gridColor, line=-1, tick=F, col.axis=colorAfter, cex.axis=1.5)
  axis(2, at=yAxisTicks, col=labelColor, col.axis=labelColor, line=-0, tick=T, cex.axis=1.5)  
  mtext("General Fertility Rate", side=2,line=2.5, cex=1.25) 
    
    
#  axis(1, at=seq(from=6, to=(monthCount), by=6), labels=rep(c("(Jun)", "(Dec)"), 5), col.axis=labelColor, line=0, tick=F, lty=0, cex.axis=.7)

  abline(v=seq(from=monthsPerYear, to=monthCount, by=monthsPerYear), col=gridColor, lty=2)
  for( i in 2:monthCount ) {
    x1 <- ds[i-1, 'MonthID'] + xOffset
    x2 <- ds[i, 'MonthID'] + xOffset
    y1 <- ds[i-1, 'BirthRate'] + graphFloor
    y2 <- ds[i, 'BirthRate'] + graphFloor
    lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
  }
  abline(v=changePoint + xOffset, col=colorAfter)
  mtext("Bombing Effect", side=3, at=changePoint + xOffset, col=colorAfter, cex=.8)

linearVerticesXPre <- rep(NA,changePoint) 
linearVerticesXPost <- numeric(0)
linearBeforeLowerVerticesYPre <- rep(NA,changePoint) 
linearBeforeUpperVerticesYPre <- rep(NA,changePoint) 
linearBeforeLowerVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
linearBeforeUpperVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
linearAfterLowerVerticesYPre <- rep(NA,changePoint) 
linearAfterUpperVerticesYPre <- rep(NA,changePoint) 
linearAfterLowerVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
linearAfterUpperVerticesYPost <- rep(NA,monthCount - (changePoint+1) )
monthOffset <- rep(0:(yearCount-1), each=monthsPerYear) * monthsPerYear

for( monthID in 1:changePoint ) { 
  desiredCycle <- monthID %% monthsPerYear
  if(desiredCycle==0) desiredCycle <- 12
    
  linearVerticesXPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$CycleID + monthOffset[monthID]
  linearBeforeLowerVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerBefore
  linearBeforeUpperVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperBefore  
  linearAfterLowerVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerAfter
  linearAfterUpperVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperAfter
}
for( monthID in (changePoint+1):monthCount ) {  
  desiredCycle <- monthID %% monthsPerYear
  if(desiredCycle==0) desiredCycle <- 12
    
  linearVerticesXPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$CycleID + monthOffset[monthID]
  linearBeforeLowerVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerBefore
  linearBeforeUpperVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperBefore  
  linearAfterLowerVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerAfter
  linearAfterUpperVerticesYPost[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperAfter 
}

linearVerticesXPre <- c(linearVerticesXPre, rev(linearVerticesXPre))
linearVerticesXPost <- c(linearVerticesXPost, rev(linearVerticesXPost))

linearBeforeVerticesYPre <- c(linearBeforeLowerVerticesYPre, rev(linearBeforeUpperVerticesYPre))+graphFloor
linearAfterVerticesYPre <- c(linearAfterLowerVerticesYPre, rev(linearAfterUpperVerticesYPre))+graphFloor
linearBeforeVerticesYPost <- c(linearBeforeLowerVerticesYPost, rev(linearBeforeUpperVerticesYPost))+graphFloor
linearAfterVerticesYPost <- c(linearAfterLowerVerticesYPost, rev(linearAfterUpperVerticesYPost))+graphFloor

polygon(x=linearVerticesXPre + xOffset, y=linearBeforeVerticesYPre, border=NA, col=bandColorBefore[1])
polygon(x=linearVerticesXPre + xOffset, y=linearAfterVerticesYPre, border=NA, col=bandColorAfter[1])
polygon(x=linearVerticesXPost + xOffset, y=linearBeforeVerticesYPost, border=NA, col=bandColorBefore[2])
polygon(x=linearVerticesXPost + xOffset, y=linearAfterVerticesYPost, border=NA, col=bandColorAfter[2])
par(op)
# dev.off()

########################################################
### Linear Graphs
########################################################
monthsInAverage <- 12
dsMoving <- data.frame(matrix(NA, nrow=nrow(ds), ncol=4))
colnames(dsMoving) <- c("MonthID", "BirthRate", "UpperQuantile", "LowerQuantile")
dsAnnualAverage <- data.frame(matrix(NA, nrow=fullFebruaryCount, ncol=2))
colnames(dsAnnualAverage) <- c("MonthID", "BirthRate")
dsMoving$MonthID <- ds$MonthID

annualAverageTally <- 1

for( i in monthsInAverage:monthCount ) {
  startRow <- i - (monthsInAverage-1)
  stopRow <- i
  dsMoving$BirthRate[i] <- mean(ds$BirthRate[startRow:stopRow])  
#  dsMoving$BirthRate[i] <- quantile(ds$BirthRate[startRow:stopRow], .5)  
  dsMoving$UpperQuantile[i] <- quantile(ds$BirthRate[startRow:stopRow], upperQuantile)
  dsMoving$LowerQuantile[i] <- quantile(ds$BirthRate[startRow:stopRow], lowerQuantile)  
  if( (i >= monthsInAverage) && (i %% monthsInAverage == 2) ) {
    dsAnnualAverage$MonthID[annualAverageTally] <- ds$MonthID[i]
    dsAnnualAverage$BirthRate[annualAverageTally] <- mean(ds$BirthRate[startRow:stopRow])      
    annualAverageTally <- annualAverageTally + 1
  }
}




PlotBlank <- function( ) {
  plot(NA, xlim=c(-1,1), ylim=c(-1, 1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
}
PlotLinear <- function( displayMovingAverage=TRUE, displayMovingAverageBands=TRUE ) {
  if( displayMovingAverageBands )
    subTitle <- paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the previous", monthsInAverage, "months)")
  else
    subTitle <- ""

  plot(c(0,0), xlim=c(0, monthCount), ylim=c(graphFloor, graphCeiling), type="n", xaxt="n", xaxs="i", yaxt="n", yaxs="i", bty="n",
    ylab="", sub=subTitle, col.sub=labelColor, xlab="",#xlab="Time",
       cex.lab=2, cex.sub=1.5)
  axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
    col=gridColor, line=-1, tick=F, col.axis=colorBefore, cex.axis=1.5)
  axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
    col=gridColor, line=-1, tick=F, col.axis=colorAfter, cex.axis=1.5)
  axis(2, at=yAxisTicks, col=labelColor, col.axis=labelColor, line=-0, tick=T, cex.axis=1.5)
  mtext("General Fertility Rate", side=2,line=2.5, cex=1) 

  abline(v=seq(from=monthsPerYear, to=monthCount, by=monthsPerYear), col=gridColor, lty=2)
  if( displayMovingAverage ) {
    for( i in 2:monthCount ) {
      x1 <- dsMoving[i-1, 'MonthID'] + xOffset
      x2 <- dsMoving[i, 'MonthID'] + xOffset
      y1 <- dsMoving[i-1, 'BirthRate'] + graphFloor
      y2 <- dsMoving[i, 'BirthRate'] + graphFloor
      lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
    }
  }
  else {
    for( i in 2:monthCount ) {
      x1 <- ds[i-1, 'MonthID'] + xOffset
      x2 <- ds[i, 'MonthID'] + xOffset
      y1 <- ds[i-1, 'BirthRate'] + graphFloor
      y2 <- ds[i, 'BirthRate'] + graphFloor
      lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
    }
  }

  points(x=ds$MonthID + xOffset, y=ds$BirthRate+ graphFloor, col=lineColors, cex=1, xpd=NA)
  abline(v=changePoint + xOffset, col=colorAfter)
  mtext("Bombing Effect", side=3, at=changePoint + xOffset, col=colorAfter, cex=.8)
  if( displayMovingAverageBands ) {
    linearVerticesXPre <- rep(NA,changePoint)
    linearVerticesXPost <- numeric(0)
    linearBeforeLowerVerticesY <- rep(NA,changePoint)
    linearBeforeUpperVerticesY <- rep(NA,changePoint)
    linearAfterLowerVerticesY <- rep(NA,monthCount - (changePoint+1) )
    linearAfterUpperVerticesY <- rep(NA,monthCount - (changePoint+1) )

    for( monthID in 1:changePoint ) {
      linearVerticesXPre[monthID] <- dsMoving$MonthID[monthID]
      linearBeforeLowerVerticesY[monthID] <- dsMoving$LowerQuantile[monthID]
      linearBeforeUpperVerticesY[monthID] <- dsMoving$UpperQuantile[monthID]
    }
    for( monthID in changePoint:monthCount ) {
      linearVerticesXPost[monthID] <- dsMoving$MonthID[monthID]
      linearAfterLowerVerticesY[monthID] <- dsMoving$LowerQuantile[monthID]
      linearAfterUpperVerticesY[monthID] <- dsMoving$UpperQuantile[monthID]
    }

    linearVerticesXPre <- c(linearVerticesXPre, rev(linearVerticesXPre))
    linearVerticesXPost <- c(linearVerticesXPost, rev(linearVerticesXPost))

    linearBeforeVerticesY <- c(linearBeforeLowerVerticesY, rev(linearBeforeUpperVerticesY))+graphFloor
    linearAfterVerticesY <- c(linearAfterLowerVerticesY, rev(linearAfterUpperVerticesY))+graphFloor

    polygon(x=linearVerticesXPre + xOffset, y=linearBeforeVerticesY, border=NA, col=bandColorBefore[1])
    polygon(x=linearVerticesXPost + xOffset, y=linearAfterVerticesY, border=NA, col=bandColorAfter[2])
  }
  colorTrendLine <- smoothedLinear#"tomato"
#   colorTrendLine <- "springgreen3"
#  colorTrendLine <- "purple"

  lines(x=dsAnnualAverage$MonthID + xOffset, y=dsAnnualAverage$BirthRate+ graphFloor, col=colorTrendLine)
  points(x=dsAnnualAverage$MonthID + xOffset, y=dsAnnualAverage$BirthRate+ graphFloor, col=colorTrendLine, pch=3)
}
# pdf(file=file.path(pathDirectoryOutput, "Figure2.pdf"), width=deviceWidth, height=height)
# png(file=file.path(pathDirectoryOutput, "Figure2.png"), width=deviceWidth, height=height, units="in", res=1200)

layout(rbind(1, 2,3), heights=c(.5, .5, .5))
par(pty="m",  mar=c(5, 4.5, 1, 1) + 0.1)
PlotLinear(displayMovingAverage=FALSE, displayMovingAverageBands=FALSE)
PlotLinear(displayMovingAverageBands=FALSE)
PlotLinear()
par(op)
# dev.off()
# savePlot(filename="Linear6", type="tiff")