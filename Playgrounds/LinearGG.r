rm(list=ls(all=TRUE))
require(colorspace)
require(plyr)
require(ggplot2)

pathDirectory <- getwd()
pathInput <- file.path(pathDirectory, "Datasets/BirthRatesOkc.txt")
pathDirectoryOutput <-  file.path(pathDirectory, "PublicationGraphs")

ds <- read.table(pathInput, header=TRUE, stringsAsFactor=F)
ds$Date <- as.Date(ds$Date)
firstYear <- 1990
changePoint <- 74 #The 74th month is April 1996
fullFebruaryCount <- 9 #The number of Februaries with a full preceeding 12 month period.

monthsPerYear <- 12
monthCount <- nrow(ds)
yearCount <- monthCount / monthsPerYear

offset <- 240
colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2] #h=120
smoothedLinear <- hcl(h=40, c=150, l=45)


lineColors <- c(rep(colorBefore, times=changePoint), rep(colorAfter, times=monthCount-changePoint))
transparencyFocus <- .5
transparencyBackground <- .3
bandColorBefore <- c(adjustcolor(colorBefore, transparencyFocus), adjustcolor(colorBefore, transparencyBackground))
bandColorAfter <- c(adjustcolor(colorAfter, transparencyBackground), adjustcolor(colorAfter, transparencyFocus))

gridColor <- gray(.9)
labelColor <- gray(.7)
polarGridLty <- 3
interpolationPoints <- 5
interpolatedCount <- (monthCount - 1) * interpolationPoints + monthCount

graphCeiling <- 7
graphFloor <- 5
yAxisTicks <- c(5, 6, 7)

graphHeight <- graphCeiling - graphFloor
xOffset <- -.5 #So the points are plotted in the middle of the month.

lowerQuantile <- .25
upperQuantile <- .75

# ds <- data.frame(matrix(NA, nrow=monthCount, ncol=6))
# ds <- data.frame(MonthID=rep(NA_integer_, monthCount), MonthIndex=NA_integer_, Radians=NA_real_, BirthRate=NA_real_)
ds$MonthIndex <- ds$MonthID %% monthsPerYear
ds$StageID <- ifelse(ds$MonthID<=changePoint, 1, 2)
# ds$Radians <- ds$MonthIndex * (2 * pi / monthsPerYear) 
# ds$X <- ds$BirthRate * sin(ds$Radians)
# ds$Y <- ds$BirthRate * cos(ds$Radians)
#maxRate <- max(ds$BirthRate)
tail(ds)

Summarize <- function( df ) {
  data.frame( 
    Lower=quantile(df$BirthRate, probs=lowerQuantile), 
    Upper=quantile(df$BirthRate, probs=upperQuantile)  
)}
dsBand <- ddply(ds, .variables=c("MonthIndex", "StageID"), Summarize)
dsBand <- rename(dsBand, replace=c("StageID"="StageIDBand"))
dsBands <- join(x=ds, y=dsBand, by="MonthIndex")
dsBands$InPhase <- (dsBands$StageID == dsBands$StageIDBand)

# colnames(dsBands) <- c("CycleID", "Radians", "LowerBefore", "UpperBefore", "LowerAfter", "UpperAfter", 
#   "LowerBeforeX", "LowerBeforeY", "UpperBeforeX", "UpperBeforeY", "LowerAfterX", "LowerAfterY", "UpperAfterX", "UpperAfterY")
#   


g <- ggplot(ds, aes(x=Date, y=BirthRate, color=factor(StageID)))
g <- g + geom_point(shape=1)
g <- g + geom_line()
g <- g + scale_color_manual(values=c("1"=colorBefore, "2"=colorAfter), guide=FALSE)
g <- g + theme_bw()
g

# 
# 
# ########################################################
# ### Linear Graphs
# ########################################################
# monthsInAverage <- 12
# dsMoving <- data.frame(matrix(NA, nrow=nrow(ds), ncol=4))
# colnames(dsMoving) <- c("MonthID", "BirthRate", "UpperQuantile", "LowerQuantile")
# dsAnnualAverage <- data.frame(matrix(NA, nrow=fullFebruaryCount, ncol=2))
# colnames(dsAnnualAverage) <- c("MonthID", "BirthRate")
# dsMoving$MonthID <- ds$MonthID
# 
# annualAverageTally <- 1
# 
# for( i in monthsInAverage:monthCount ) {
#   startRow <- i - (monthsInAverage-1)
#   stopRow <- i
#   dsMoving$BirthRate[i] <- mean(ds$BirthRate[startRow:stopRow])  
# #  dsMoving$BirthRate[i] <- quantile(ds$BirthRate[startRow:stopRow], .5)  
#   dsMoving$UpperQuantile[i] <- quantile(ds$BirthRate[startRow:stopRow], upperQuantile)
#   dsMoving$LowerQuantile[i] <- quantile(ds$BirthRate[startRow:stopRow], lowerQuantile)  
#   if( (i >= monthsInAverage) && (i %% monthsInAverage == 2) ) {
#     dsAnnualAverage$MonthID[annualAverageTally] <- ds$MonthID[i]
#     dsAnnualAverage$BirthRate[annualAverageTally] <- mean(ds$BirthRate[startRow:stopRow])      
#     annualAverageTally <- annualAverageTally + 1
#   }
# }
# 
# 
# 
# 
# PlotBlank <- function( ) {
#   plot(NA, xlim=c(-1,1), ylim=c(-1, 1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
# }
# PlotLinear <- function( displayMovingAverage=TRUE, displayMovingAverageBands=TRUE ) {
#   if( displayMovingAverageBands )
#     subTitle <- paste("(Bands mark the", lowerQuantile, "and", upperQuantile, "quantiles for the previous", monthsInAverage, "months)")
#   else
#     subTitle <- ""
# 
#   plot(c(0,0), xlim=c(0, monthCount), ylim=c(graphFloor, graphCeiling), type="n", xaxt="n", xaxs="i", yaxt="n", yaxs="i", bty="n",
#     ylab="", sub=subTitle, col.sub=labelColor, xlab="",#xlab="Time",
#        cex.lab=2, cex.sub=1.5)
#   axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
#     col=gridColor, line=-1, tick=F, col.axis=colorBefore, cex.axis=1.5)
#   axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
#     col=gridColor, line=-1, tick=F, col.axis=colorAfter, cex.axis=1.5)
#   axis(2, at=yAxisTicks, col=labelColor, col.axis=labelColor, line=-0, tick=T, cex.axis=1.5)
#   mtext("General Fertility Rate", side=2,line=2.5, cex=1) 
# 
#   abline(v=seq(from=monthsPerYear, to=monthCount, by=monthsPerYear), col=gridColor, lty=2)
#   if( displayMovingAverage ) {
#     for( i in 2:monthCount ) {
#       x1 <- dsMoving[i-1, 'MonthID'] + xOffset
#       x2 <- dsMoving[i, 'MonthID'] + xOffset
#       y1 <- dsMoving[i-1, 'BirthRate'] + graphFloor
#       y2 <- dsMoving[i, 'BirthRate'] + graphFloor
#       lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
#     }
#   }
#   else {
#     for( i in 2:monthCount ) {
#       x1 <- ds[i-1, 'MonthID'] + xOffset
#       x2 <- ds[i, 'MonthID'] + xOffset
#       y1 <- ds[i-1, 'BirthRate'] + graphFloor
#       y2 <- ds[i, 'BirthRate'] + graphFloor
#       lines(x=c(x1, x2), y=c(y1, y2), col=lineColors[i], lwd=2)
#     }
#   }
# 
#   points(x=ds$MonthID + xOffset, y=ds$BirthRate+ graphFloor, col=lineColors, cex=1, xpd=NA)
#   abline(v=changePoint + xOffset, col=colorAfter)
#   mtext("Bombing Effect", side=3, at=changePoint + xOffset, col=colorAfter, cex=.8)
#   if( displayMovingAverageBands ) {
#     linearVerticesXPre <- rep(NA,changePoint)
#     linearVerticesXPost <- numeric(0)
#     linearBeforeLowerVerticesY <- rep(NA,changePoint)
#     linearBeforeUpperVerticesY <- rep(NA,changePoint)
#     linearAfterLowerVerticesY <- rep(NA,monthCount - (changePoint+1) )
#     linearAfterUpperVerticesY <- rep(NA,monthCount - (changePoint+1) )
# 
#     for( monthID in 1:changePoint ) {
#       linearVerticesXPre[monthID] <- dsMoving$MonthID[monthID]
#       linearBeforeLowerVerticesY[monthID] <- dsMoving$LowerQuantile[monthID]
#       linearBeforeUpperVerticesY[monthID] <- dsMoving$UpperQuantile[monthID]
#     }
#     for( monthID in changePoint:monthCount ) {
#       linearVerticesXPost[monthID] <- dsMoving$MonthID[monthID]
#       linearAfterLowerVerticesY[monthID] <- dsMoving$LowerQuantile[monthID]
#       linearAfterUpperVerticesY[monthID] <- dsMoving$UpperQuantile[monthID]
#     }
# 
#     linearVerticesXPre <- c(linearVerticesXPre, rev(linearVerticesXPre))
#     linearVerticesXPost <- c(linearVerticesXPost, rev(linearVerticesXPost))
# 
#     linearBeforeVerticesY <- c(linearBeforeLowerVerticesY, rev(linearBeforeUpperVerticesY))+graphFloor
#     linearAfterVerticesY <- c(linearAfterLowerVerticesY, rev(linearAfterUpperVerticesY))+graphFloor
# 
#     polygon(x=linearVerticesXPre + xOffset, y=linearBeforeVerticesY, border=NA, col=bandColorBefore[1])
#     polygon(x=linearVerticesXPost + xOffset, y=linearAfterVerticesY, border=NA, col=bandColorAfter[2])
#   }
#   colorTrendLine <- smoothedLinear#"tomato"
# #   colorTrendLine <- "springgreen3"
# #  colorTrendLine <- "purple"
# 
#   lines(x=dsAnnualAverage$MonthID + xOffset, y=dsAnnualAverage$BirthRate+ graphFloor, col=colorTrendLine)
#   points(x=dsAnnualAverage$MonthID + xOffset, y=dsAnnualAverage$BirthRate+ graphFloor, col=colorTrendLine, pch=3)
# }
# # pdf(file=file.path(pathDirectoryOutput, "Figure2.pdf"), width=deviceWidth, height=height)
# # png(file=file.path(pathDirectoryOutput, "Figure2.png"), width=deviceWidth, height=height, units="in", res=1200)
# 
# layout(rbind(1, 2,3), heights=c(.5, .5, .5))
# par(pty="m",  mar=c(5, 4.5, 1, 1) + 0.1)
# PlotLinear(displayMovingAverage=FALSE, displayMovingAverageBands=FALSE)
# PlotLinear(displayMovingAverageBands=FALSE)
# PlotLinear()

# dev.off()
# savePlot(filename="Linear6", type="tiff")