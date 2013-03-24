rm(list=ls(all=TRUE))
require(colorspace)
require(lubridate)
require(plyr)
require(ggplot2)
require(zoo)

pathDirectory <- getwd()
pathInput <- file.path(pathDirectory, "Datasets/BirthRatesOkc.txt")
pathDirectoryOutput <-  file.path(pathDirectory, "PublicationGraphs")

ds <- read.table(pathInput, header=TRUE, stringsAsFactor=F)
ds$Date <- as.Date(ds$Date)
day(ds$Date) <- 15

dateBombing <- as.Date("1995-04-19")
changePoint <- 74 #The 74th month is Feb 1996
changeMonth <- as.Date("1996-02-15") # as.Date(dateBombing + weeks(40))
fullFebruaryCount <- 9 #The number of Februaries with a full preceeding 12 month period.

monthsPerYear <- 12
monthCount <- nrow(ds)
yearCount <- monthCount / monthsPerYear

offset <- 240
colorBefore <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[1]
colorAfter <- rainbow_hcl(2, start=0+offset, end=240+offset, c=100, l=65)[2] #h=120
smoothedLinear <- hcl(h=40, c=150, l=45)

transparencyFocus <- .5
transparencyBackground <- .3
bandColorBefore <- c(adjustcolor(colorBefore, transparencyFocus), adjustcolor(colorBefore, transparencyBackground))
bandColorAfter <- c(adjustcolor(colorAfter, transparencyBackground), adjustcolor(colorAfter, transparencyFocus))

gridColor <- gray(.9)
labelColor <- gray(.7)

graphCeiling <- 7
graphFloor <- 5
yAxisTicks <- c(5, 6, 7)

graphHeight <- graphCeiling - graphFloor
xOffset <- -.5 #So the points are plotted in the middle of the month.

lowerQuantile <- .25
upperQuantile <- .75

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
CalculateLowerBand <- function( x ) { return( quantile(x, probs=lowerQuantile) ) }
CalculateUpperBand <- function( x ) { return( quantile(x, probs=upperQuantile) ) }

dsBand <- ddply(ds, .variables=c("MonthIndex", "StageID"), Summarize)
dsBand <- rename(dsBand, replace=c("StageID"="StageIDBand"))
dsBands <- join(x=ds, y=dsBand, by="MonthIndex")
dsBands$InPhase <- (dsBands$StageID == dsBands$StageIDBand)

ds$Rolling <- rollapply(ds$BirthRate, 12, mean, align="right", fill=NA)
ds$RollingLower <- rollapply(ds$BirthRate, 12, CalculateLowerBand, align="right", fill=NA)
ds$RollingUpper <- rollapply(ds$BirthRate, 12, CalculateUpperBand, align="right", fill=NA)


dsFebruary <- ds[ds$MonthIndex==2 & !is.na(ds$Rolling), ]
dsStage1 <- ds[!is.na(ds$Rolling) & ds$MonthID<=changePoint, ]
dsStage2 <- ds[!is.na(ds$Rolling) & ds$MonthID>=changePoint, ]
# ds$level <- ds$BirthRate

# axis(1, at=seq(from=0, to=changePoint-monthsPerYear, by=12)+6, labels=seq(from=firstYear, to=firstYear+5, by=1),
#      col=gridColor, line=-1, tick=F, col.axis=colorBefore, cex.axis=1.5)
# axis(1, at=seq(from=changePoint+1, to=monthCount, by=12)+(6-changePoint%%monthsPerYear), labels=seq(from=firstYear+6, to=firstYear+yearCount - 1, by=1),
#      col=gridColor, line=-1, tick=F, col.axis=colorAfter, cex.axis=1.5)
#dateLocations <- seq.Date(from=as.Date("1990-07-01"), to=as.Date("1999-07-01"), by="year")
dateLocations <- seq.Date(from=as.Date("1990-01-01"), to=as.Date("2000-01-01"), by="year")
dateColors <- c(rep(colorBefore, 6), rep(colorAfter, 5))
dsLabelsX <- data.frame(
  X=seq.Date(from=as.Date("1990-07-01"), to=as.Date("1999-07-01"), by="year"), 
  Y=graphFloor, 
  Color=c(rep(colorBefore, 6), rep(colorAfter, 4)),
  stringsAsFactors=FALSE
)
dsLabelsX$Label <- lubridate::year(dsLabelsX$X)
# dsLabelsX$Color <- as.character(dsLabelsX$Color)


g <- ggplot(ds, aes(x=Date, y=BirthRate, color=StageID))
#g <- ggplot(ds, aes(x=MonthID, y=BirthRate, color=StageID))
g <- g + geom_ribbon(data=dsStage1, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorBefore[2], color=NA )
g <- g + geom_ribbon(data=dsStage2, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorAfter[2], color=NA )
g <- g + geom_point(shape=1)
g <- g + geom_line(size=1)
g <- g + geom_line(data=ds[!is.na(ds$Rolling), ], aes(y=Rolling), size=2)

g <- g + geom_line(data=dsFebruary, aes(y=Rolling), size=1, color=smoothedLinear)
g <- g + geom_point(data=dsFebruary, aes(y=Rolling), size=4, shape=3, color=smoothedLinear)

g <- g + annotate("text", x=dsLabelsX$X, y=dsLabelsX$Y, color=dsLabelsX$Color, label=dsLabelsX$Label, vjust=-.5, size=8)
g <- g + scale_x_date(breaks=dateLocations, labels=scales::date_format("%Y"))
g <- g + scale_y_continuous(breaks=yAxisTicks)
g <- g + scale_color_continuous(low=colorBefore, high=colorAfter, guide=FALSE)
g <- g + geom_vline(x=as.integer(changeMonth), color=colorAfter)
# g <- g + annotate("segment", x=as.integer(changeMonth), xend=as.integer(changeMonth), y=5, yend=7, color=colorAfter)
# g <- g + annotate("segment", x=changeMonth, xend=changeMonth, y=5, yend=7, color=colorAfter)
g <- g + annotate("text", x=changeMonth, y=max(ds$BirthRate), color=colorAfter, label="Bombing Effect")
g <- g + theme_bw()
g <- g + theme(axis.text.x=element_text(colour=dateColors, size=16))
g <- g + labs(x="", y="General Fertility Rate")
g

# ggsave(file.path(pathDirectoryOutput, "LinearGGTry1.pdf"))