rm(list=ls(all=TRUE))
require(grid)
require(colorspace)
require(lubridate)
require(plyr)
require(ggplot2)
require(zoo)

pathDirectory <- getwd()
pathInput <- file.path(pathDirectory, "Datasets/BirthRatesOkc.txt")
pathDirectoryOutput <-  file.path(pathDirectory, "PublicationGraphs")
widthTotal <- 6.5
heightTotal <- 4.5

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

graphCeiling <- 7
graphFloor <- 5
yAxisTicks <- c(5, 6, 7)

lowerQuantile <- .25
upperQuantile <- .75

ds$MonthIndex <- ds$MonthID %% monthsPerYear
ds$StageID <- ifelse(ds$MonthID<=changePoint, 1, 2)
# ds$Radians <- ds$MonthIndex * (2 * pi / monthsPerYear) 
# ds$X <- ds$BirthRate * sin(ds$Radians)
# ds$Y <- ds$BirthRate * cos(ds$Radians)
#maxRate <- max(ds$BirthRate)
# tail(ds)

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

dateLocations <- seq.Date(from=as.Date("1990-01-01"), to=as.Date("2000-01-01"), by="year")
dateColors <- c(rep(colorBefore, 6), rep(colorAfter, 5))
dsLabelsX <- data.frame(
  X=seq.Date(from=as.Date("1990-07-01"), to=as.Date("1999-07-01"), by="year"), 
  Y=graphFloor, 
  Color=c(rep(colorBefore, 6), rep(colorAfter, 4)),
  stringsAsFactors=FALSE
)
dsLabelsX$Label <- lubridate::year(dsLabelsX$X)

dsBreak <- data.frame(X=changeMonth,XEnd=changeMonth, Y=5, YEnd=6.8, Label="Bombing Effect")

LinearPlot <- function( showLine=TRUE, showSmoother=TRUE, showRibbon=TRUE, showYears=TRUE, labelBreak=TRUE ) { 
  g <- ggplot(ds, aes(x=Date, y=BirthRate, color=StageID))#
  g <- g + geom_segment(data=dsBreak, aes(x=X, xend=XEnd, y=Y, yend=YEnd), color=colorAfter, size=3, alpha=.3)
  
  if( labelBreak ) g <- g + geom_text(data=dsBreak, aes(x=X, y=YEnd, label=Label), color=colorAfter, vjust=-.5, alpha=.5, size=4)#6
  
  g <- g + geom_line(data=dsFebruary, aes(y=Rolling), color=smoothedLinear, alpha=.5)
  g <- g + geom_point(data=dsFebruary, aes(y=Rolling), size=2, shape=3, color=smoothedLinear)
  
  if( showRibbon ) g <- g + geom_ribbon(data=dsStage1, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorBefore[2], color=NA )
  if( showRibbon ) g <- g + geom_ribbon(data=dsStage2, aes(ymin=RollingLower, ymax=RollingUpper), fill=bandColorAfter[2], color=NA )
  g <- g + geom_point(shape=1, alpha=.5)
  if( showLine ) {
    #g <- g + geom_line() #This produces blocky lines, b/c it's checking for color switches
    g <- g + geom_line(data=dsStage1, color=colorBefore )
    g <- g + geom_line(data=dsStage2, color=colorAfter )
  }
  if( showSmoother) {
    #g <- g + geom_path(data=ds[!is.na(ds$Rolling), ], aes(y=Rolling)) #This produces blocky lines, b/c it's checking for color switches
    g <- g + geom_line(data=dsStage1, aes(y=Rolling), color=colorBefore )
    g <- g + geom_line(data=dsStage2, aes(y=Rolling), color=colorAfter )
  }
  
  
  if( showYears ) 
    g <- g + annotate("text", x=dsLabelsX$X, y=dsLabelsX$Y, color=dsLabelsX$Color, label=dsLabelsX$Label, vjust=-.5, size=3) #8
  if( !showYears ) 
    g <- g + annotate("text", x=dsLabelsX$X, y=dsLabelsX$Y, color=dsLabelsX$Color, label=dsLabelsX$Label, vjust=-.5, size=3, alpha=.3) #8

  g <- g + scale_x_date(breaks=dateLocations, labels=scales::date_format("%Y"))
  g <- g + scale_y_continuous(breaks=yAxisTicks)
  g <- g + scale_color_continuous(low=colorBefore, high=colorAfter, guide=FALSE)

  g <- g + theme_minimal()
  g <- g + theme(axis.title=element_text(color="gray60", size=9))
  g <- g + theme(axis.text.x=element_blank())
  g <- g + theme(axis.text.y=element_text(color="gray80"))
  g <- g + theme(axis.ticks.length = unit(0, "cm")) #g <- g + theme(axis.ticks=element_blank())
  g <- g + theme(axis.ticks.margin = unit(.00001, "cm"))
  g <- g + theme(panel.grid.minor.y=element_line(color="gray90", size=.1))
  g <- g + theme(panel.grid.major=element_line(color="gray85", size=.15))  
  g <- g + theme(panel.margin = unit(c(0, 0, 0, 0), "cm"))
  g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  g <- g + labs(x="", y="General Fertility Rate")
  if( showRibbon ) g <- g + labs(x="Bands Mark the .25 and .75 quantile for the previous 12 months")
  g
}


LinearPlot()
top <- LinearPlot(showSmoother=FALSE, showRibbon=FALSE, showYears=FALSE) #Top Panel
middle <- LinearPlot(showLine=FALSE, showRibbon=FALSE, showYears=FALSE, labelBreak=FALSE) #Middle Panel
bottom <- LinearPlot(showLine=FALSE, labelBreak=FALSE) #Bottom Panel
# ggsave(file.path(pathDirectoryOutput, "LinearGGTry1.pdf"))

vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }
pdf(file.path(pathDirectoryOutput, "Fig2.pdf"), width=widthTotal, height=heightTotal)
# png(file.path(pathDirectoryOutput, "Fig2.png"), width=widthTotal, height=heightTotal, units="in", res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,1)))
print(top, vp=vpLayout(1,1))
print(middle, vp=vpLayout(2,1))
print(bottom, vp=vpLayout(3,1))
dev.off()


