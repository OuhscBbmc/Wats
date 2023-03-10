rm(list=ls(all=TRUE))
library(colorspace)
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
if(colorSchemeVersion == 1) {
  colorBefore <- "peru"
  colorAfter <- "springgreen4"
}
if(colorSchemeVersion == 2) {
  offset <- 150+180
  colorBefore <- rainbow_hcl(3, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2]
}
if(colorSchemeVersion == 3) {
  offset <- 300
  colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2]
}
if(colorSchemeVersion == 4) {
  offset <- 40
  colorBefore <- rainbow_hcl(2, start=0+offset, end=180+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=180+offset, c=100, l=65)[2]
}
if(colorSchemeVersion == 5) {
  offset <- 0
  colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2]
}
if(colorSchemeVersion == 6) {
  offset <- 240
  colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
  colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2] #h=120
  smoothedLinear <- hcl(h=40, c=150, l=45)
}
windows.options(antialias = "cleartype")
# if(names(dev.cur()) != "null device" ) dev.off()
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
FadeColor <- function(color, transparency) {
    rgb <- col2rgb(color) / 255 #break it into components and rescale
		color <- rgb(red=rgb[1], green=rgb[2], blue=rgb[3], alpha=transparency)
}
bandColorBefore <- c(FadeColor(colorBefore, transparencyFocus), FadeColor(colorBefore, transparencyBackground))
bandColorAfter <- c(FadeColor(colorAfter, transparencyBackground), FadeColor(colorAfter, transparencyFocus))

gridColor <- gray(.9)
label_color <- gray(.7)
polarGridLty <- 3
interpolationPoints <- 5
interpolatedCount <- (monthCount - 1) * interpolationPoints + monthCount
#For Okc
graph_ceiling <- 7
graph_floor <- 5
yAxisTicks <- c(5, 6, 7)
##For Rogers
#graph_ceiling <- 7.2
#graph_floor <- 3.5
#yAxisTicks <- c(4, 5, 6, 7)
##For Tulsa
#graph_ceiling <- 7
#graph_floor <- 4.5
#yAxisTicks <- c(5, 6, 7)

graphHeight <- graph_ceiling - graph_floor
xOffset <- -.5 #So the points are plotted in the middle of the month.

lowerQuantile <- .25
upperQuantile <- .75

#lowerQuantile <- 0#.25
#upperQuantile <- 1#.75

ds <- tibble::tibble(matrix(NA, nrow=monthCount, ncol=6))
colnames(ds) <- c("MonthID", "MonthIndex", "Radians", "birth_rate", "X", "Y")

for(yearIndex in 1:yearCount) {
  for(monthIndex in 1:monthsPerYear) {
    monthID <- (yearIndex - 1) * monthsPerYear + monthIndex

    ds[monthID, 'MonthID'] <- monthID
    ds[monthID, 'MonthIndex'] <- monthIndex
    degrees <- monthIndex * (360 / monthsPerYear)
    ds[monthID, 'Radians'] <- degrees / 180 * pi
    ds[monthID, 'birth_rate'] <- dsOkc$birth_rate[monthID] - graph_floor
  }
}
ds$X <- ds$birth_rate * sin(ds$Radians)
ds$Y <- ds$birth_rate * cos(ds$Radians)
#maxRate <- max(ds$birth_rate)
tail(ds)

dsInterpolated <-tibble::tibble(matrix(NA, nrow=interpolatedCount, ncol=6))
colnames(dsInterpolated) <- c("DurationID", "CycleID", "Radians", "birth_rate", "X", "Y")
dsInterpolated[1, ] <- ds[1, ]
rowTally <- 1
for(pointIndex in 2:monthCount) {
  monthStart <- ds[pointIndex - 1, 'MonthIndex']
  monthStop <- ds[pointIndex, 'MonthIndex']
  birthStart <- ds[pointIndex - 1, 'birth_rate']
  birthStop <- ds[pointIndex, 'birth_rate']

  for(interpolationIndex in 1:(interpolationPoints+1)) {
    if(monthStop < monthStart) monthStart <- 0
    proportion <- interpolationIndex / (interpolationPoints + 1)
    monthEst <- monthStart + proportion * (monthStop - monthStart)
    birthRateEst <- birthStart + proportion * (birthStop - birthStart)
    degrees <- monthEst * (360 / monthsPerYear)
    radians <- degrees / 180 * pi
    rowTally <- rowTally + 1
    dsInterpolated[rowTally, 'DurationID'] <- rowTally
    dsInterpolated[rowTally, 'CycleID'] <- monthEst
    dsInterpolated[rowTally, 'Radians'] <- radians
    dsInterpolated[rowTally, 'birth_rate'] <- birthRateEst
  }
}
dsInterpolated$X <- dsInterpolated$birth_rate * sin(dsInterpolated$Radians)
dsInterpolated$Y <- dsInterpolated$birth_rate * cos(dsInterpolated$Radians)

dsBands <- tibble::tibble(matrix(NA, nrow=length(unique(dsInterpolated$CycleID)), ncol=14))
colnames(dsBands) <- c("CycleID", "Radians", "LowerBefore", "UpperBefore", "LowerAfter", "UpperAfter",
  "LowerBeforeX", "LowerBeforeY", "UpperBeforeX", "UpperBeforeY", "LowerAfterX", "LowerAfterY", "UpperAfterX", "UpperAfterY")

rowTally <- 1
for(cycleID in sort(unique(dsInterpolated$CycleID))) {
  dsSliceBefore <- subset(dsInterpolated, CycleID==cycleID & DurationID <= changePoint * (interpolationPoints + 1))
  dsSliceAfter <- subset(dsInterpolated, CycleID==cycleID & DurationID > changePoint * (interpolationPoints + 1))

  dsBands[rowTally, 'CycleID'] <- cycleID
  dsBands[rowTally, 'Radians'] <- dsSliceBefore$Radians[1] #They should all have the same radians
  dsBands[rowTally, 'LowerBefore'] <- as.numeric(quantile(dsSliceBefore$birth_rate, prob=lowerQuantile))
  dsBands[rowTally, 'UpperBefore'] <- as.numeric(quantile(dsSliceBefore$birth_rate, prob=upperQuantile))
  dsBands[rowTally, 'LowerAfter'] <- as.numeric(quantile(dsSliceAfter$birth_rate, prob=lowerQuantile))
  dsBands[rowTally, 'UpperAfter'] <- as.numeric(quantile(dsSliceAfter$birth_rate, prob=upperQuantile))

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
  for(i in 1:length(radius)) {
    angles <- (0:segments)*2*pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- radius[i] * unit.circle
    lines(ellipse, col=color)
  }
}

PlotPolar <- function(drawLines) {
  plot(dsInterpolated$X, dsInterpolated$Y, xlim=c(-graphHeight, graphHeight), ylim=c(-graphHeight, graphHeight), type="n", xaxt="n", yaxt="n",  bty="n",
    xlab="", ylab="", cex.axis=1.5)
#    main=substitute(list(beta[Season]==slopeSeasonal, beta[Duration]==slopeDuration, paste(italic(e), "~", italic(N)(0, sigma))),
    #main=substitute(list(beta[Season]==over(1,slopeSeasonalInv), beta[Duration]==over(1,slopeDurationInv), paste(italic(e), "~", italic(N)(0, over(1,sigmaInv)))),
#      list(slopeSeasonalInv=1/slopeSeasonal, slopeDurationInv=1/slopeDuration,sigmaInv=1/sigma))
  if(drawLines) mtext(paste("A point at the origin represents a GFR of", graph_floor), side=1, col=label_color, cex=1.2)
  abline(v=0, col=gridColor, lty=polarGridLty)
  abline(h=0, col=gridColor, lty=polarGridLty)
  text(c("Dec","Mar","June","Sept"), x=c(0, graphHeight, 0, -graphHeight), y=c(graphHeight, 0, -graphHeight, 0), xpd=T, col=label_color, cex=1.5)
  circle(radius=seq(from=0, to=graphHeight, by=graphHeight/3), color=gridColor)

  for(i in 2:interpolatedCount) {
    x1 <- dsInterpolated[i-1, 'X']
    x2 <- dsInterpolated[i, 'X']
    y1 <- dsInterpolated[i-1, 'Y']
    y2 <- dsInterpolated[i, 'Y']
    if(drawLines) lines(x=c(x1, x2), y=c(y1, y2),  lwd=1, col=lineColors[floor(i/(interpolationPoints+1))+1])
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
op <- par(pty="s", mar=c(1, 0, 0, 0) + 0.1)

PlotPolar(drawLines=TRUE)
PlotPolar(drawLines=FALSE)

# par(pty="m",  mar=c(.8, 4, 1, 1) + 0.1) #When it's plotted by itself
par(pty="m",  mar=c(5, 4, 0, 1) + 0.1) #When it's plotted with the polars
plot(NA, xlim=c(0, monthCount), ylim=c(graph_floor, graph_ceiling), type="n", xaxt="n", xaxs="i", yaxt="n", yaxs="i", bty="n",
 #  ylab="General Fertility Rate", xlab="",#xlab="Time",
  ylab="", xlab="",#xlab="Time",
  sub=paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the before and after periods)"),
  col.sub=label_color, cex.lab=1.2)

  axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
    col=gridColor, line=-1, tick=FALSE, col.axis=colorBefore, cex.axis=1.5)
  axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
    col=gridColor, line=-1, tick=FALSE, col.axis=colorAfter, cex.axis=1.5)
  axis(2, at=yAxisTicks, col=label_color, col.axis=label_color, line=-0, tick=T, cex.axis=1.5)
  mtext("General Fertility Rate", side=2,line=2.5, cex=1.25)


#  axis(1, at=seq(from=6, to=(monthCount), by=6), labels=rep(c("(Jun)", "(Dec)"), 5), col.axis=label_color, line=0, tick=FALSE, lty=0, cex.axis=.7)

  abline(v=seq(from=monthsPerYear, to=monthCount, by=monthsPerYear), col=gridColor, lty=2)
  for(i in 2:monthCount) {
    x1 <- ds[i-1, 'MonthID'] + xOffset
    x2 <- ds[i, 'MonthID'] + xOffset
    y1 <- ds[i-1, 'birth_rate'] + graph_floor
    y2 <- ds[i, 'birth_rate'] + graph_floor
    lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
  }
  abline(v=changePoint + xOffset, col=colorAfter)
  mtext("Bombing Effect", side=3, at=changePoint + xOffset, col=colorAfter, cex=.8)

linearVerticesXPre <- rep(NA,changePoint)
linearVerticesXPost <- numeric(0)
linearBeforeLowerVerticesYPre <- rep(NA,changePoint)
linearBeforeUpperVerticesYPre <- rep(NA,changePoint)
linearBeforeLowerVerticesYPost <- rep(NA,monthCount - (changePoint+1))
linearBeforeUpperVerticesYPost <- rep(NA,monthCount - (changePoint+1))
linearAfterLowerVerticesYPre <- rep(NA,changePoint)
linearAfterUpperVerticesYPre <- rep(NA,changePoint)
linearAfterLowerVerticesYPost <- rep(NA,monthCount - (changePoint+1))
linearAfterUpperVerticesYPost <- rep(NA,monthCount - (changePoint+1))
monthOffset <- rep(0:(yearCount-1), each=monthsPerYear) * monthsPerYear

for(monthID in 1:changePoint) {
  desiredCycle <- monthID %% monthsPerYear
  if(desiredCycle==0) desiredCycle <- 12

  linearVerticesXPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$CycleID + monthOffset[monthID]
  linearBeforeLowerVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerBefore
  linearBeforeUpperVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperBefore
  linearAfterLowerVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$LowerAfter
  linearAfterUpperVerticesYPre[monthID] <- subset(dsBands, CycleID==desiredCycle)$UpperAfter
}
for(monthID in (changePoint+1):monthCount) {
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

linearBeforeVerticesYPre <- c(linearBeforeLowerVerticesYPre, rev(linearBeforeUpperVerticesYPre))+graph_floor
linearAfterVerticesYPre <- c(linearAfterLowerVerticesYPre, rev(linearAfterUpperVerticesYPre))+graph_floor
linearBeforeVerticesYPost <- c(linearBeforeLowerVerticesYPost, rev(linearBeforeUpperVerticesYPost))+graph_floor
linearAfterVerticesYPost <- c(linearAfterLowerVerticesYPost, rev(linearAfterUpperVerticesYPost))+graph_floor

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
dsMoving <- tibble::tibble(matrix(NA, nrow=nrow(ds), ncol=4))
colnames(dsMoving) <- c("MonthID", "birth_rate", "UpperQuantile", "LowerQuantile")
dsAnnualAverage <- tibble::tibble(matrix(NA, nrow=fullFebruaryCount, ncol=2))
colnames(dsAnnualAverage) <- c("MonthID", "birth_rate")
dsMoving$MonthID <- ds$MonthID

annualAverageTally <- 1

for(i in monthsInAverage:monthCount) {
  startRow <- i - (monthsInAverage-1)
  stopRow <- i
  dsMoving$birth_rate[i] <- mean(ds$birth_rate[startRow:stopRow])
#  dsMoving$birth_rate[i] <- quantile(ds$birth_rate[startRow:stopRow], .5)
  dsMoving$UpperQuantile[i] <- quantile(ds$birth_rate[startRow:stopRow], upperQuantile)
  dsMoving$LowerQuantile[i] <- quantile(ds$birth_rate[startRow:stopRow], lowerQuantile)
  if((i >= monthsInAverage) && (i %% monthsInAverage == 2)) {
    dsAnnualAverage$MonthID[annualAverageTally] <- ds$MonthID[i]
    dsAnnualAverage$birth_rate[annualAverageTally] <- mean(ds$birth_rate[startRow:stopRow])
    annualAverageTally <- annualAverageTally + 1
  }
}




PlotBlank <- function() {
  plot(NA, xlim=c(-1,1), ylim=c(-1, 1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
}
PlotLinear <- function(displayMovingAverage=TRUE, displayMovingAverageBands=TRUE) {
  if(displayMovingAverageBands)
    subTitle <- paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the previous", monthsInAverage, "months)")
  else
    subTitle <- ""

  plot(c(0,0), xlim=c(0, monthCount), ylim=c(graph_floor, graph_ceiling), type="n", xaxt="n", xaxs="i", yaxt="n", yaxs="i", bty="n",
    ylab="", sub=subTitle, col.sub=label_color, xlab="",#xlab="Time",
       cex.lab=2, cex.sub=1.5)
  axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
    col=gridColor, line=-1, tick=FALSE, col.axis=colorBefore, cex.axis=1.5)
  axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
    col=gridColor, line=-1, tick=FALSE, col.axis=colorAfter, cex.axis=1.5)
  axis(2, at=yAxisTicks, col=label_color, col.axis=label_color, line=-0, tick=T, cex.axis=1.5)
  mtext("General Fertility Rate", side=2,line=2.5, cex=1)

  abline(v=seq(from=monthsPerYear, to=monthCount, by=monthsPerYear), col=gridColor, lty=2)
  if(displayMovingAverage) {
    for(i in 2:monthCount){
      x1 <- dsMoving[i-1, 'MonthID'] + xOffset
      x2 <- dsMoving[i, 'MonthID'] + xOffset
      y1 <- dsMoving[i-1, 'birth_rate'] + graph_floor
      y2 <- dsMoving[i, 'birth_rate'] + graph_floor
      lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
    }
  }
  else {
    for(i in 2:monthCount) {
      x1 <- ds[i-1, 'MonthID'] + xOffset
      x2 <- ds[i, 'MonthID'] + xOffset
      y1 <- ds[i-1, 'birth_rate'] + graph_floor
      y2 <- ds[i, 'birth_rate'] + graph_floor
      lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
    }
  }

  points(x=ds$MonthID + xOffset, y=ds$birth_rate+ graph_floor, col=lineColors, cex=1, xpd=NA)
  abline(v=changePoint + xOffset, col=colorAfter)
  mtext("Bombing Effect", side=3, at=changePoint + xOffset, col=colorAfter, cex=.8)
  if(displayMovingAverageBands) {
    linearVerticesXPre <- rep(NA,changePoint)
    linearVerticesXPost <- numeric(0)
    linearBeforeLowerVerticesY <- rep(NA,changePoint)
    linearBeforeUpperVerticesY <- rep(NA,changePoint)
    linearAfterLowerVerticesY <- rep(NA,monthCount - (changePoint+1))
    linearAfterUpperVerticesY <- rep(NA,monthCount - (changePoint+1))

    for(monthID in 1:changePoint) {
      linearVerticesXPre[monthID] <- dsMoving$MonthID[monthID]
      linearBeforeLowerVerticesY[monthID] <- dsMoving$LowerQuantile[monthID]
      linearBeforeUpperVerticesY[monthID] <- dsMoving$UpperQuantile[monthID]
    }
    for(monthID in changePoint:monthCount) {
      linearVerticesXPost[monthID] <- dsMoving$MonthID[monthID]
      linearAfterLowerVerticesY[monthID] <- dsMoving$LowerQuantile[monthID]
      linearAfterUpperVerticesY[monthID] <- dsMoving$UpperQuantile[monthID]
    }

    linearVerticesXPre <- c(linearVerticesXPre, rev(linearVerticesXPre))
    linearVerticesXPost <- c(linearVerticesXPost, rev(linearVerticesXPost))

    linearBeforeVerticesY <- c(linearBeforeLowerVerticesY, rev(linearBeforeUpperVerticesY))+graph_floor
    linearAfterVerticesY <- c(linearAfterLowerVerticesY, rev(linearAfterUpperVerticesY))+graph_floor

    polygon(x=linearVerticesXPre + xOffset, y=linearBeforeVerticesY, border=NA, col=bandColorBefore[1])
    polygon(x=linearVerticesXPost + xOffset, y=linearAfterVerticesY, border=NA, col=bandColorAfter[2])
  }
  colorTrendLine <- smoothedLinear#"tomato"
#   colorTrendLine <- "springgreen3"
#  colorTrendLine <- "purple"

  lines(x=dsAnnualAverage$MonthID + xOffset, y=dsAnnualAverage$birth_rate+ graph_floor, col=colorTrendLine)
  points(x=dsAnnualAverage$MonthID + xOffset, y=dsAnnualAverage$birth_rate+ graph_floor, col=colorTrendLine, pch=3)
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