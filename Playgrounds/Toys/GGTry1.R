rm(list=ls(all=TRUE))
library(ggplot2)
library(colorspace)
# setwd("F:/Projects/RDev/WatsStaging/Images/")
set.seed(33)
periodLength <- 6
stageCount <- 2
stageDifference <- (c(0, 5, -2, 2, -3)/1)[seq_len(stageCount)]
breakQuantiles <- c(.3, .5, .6, .8)[seq_len(stageCount-1)]
periodDifference <- runif(periodLength)*2


pointsPerStage <- periodLength * 2
totalPeriods <- 10 #stageCount*pointsPerStage
totalPoints <- periodLength * totalPeriods
x <- seq_len(totalPoints)
periodID <- rep(seq_len(periodLength), totalPeriods) 
theta <- periodID / (2*pi)
residual <- 50 + rnorm(totalPoints, sd=5)

alphaLevel <- .2
#groupColors <- c(rgb(1, .5, .5, alphaLevel), rgb(.5, 1, .5, alphaLevel), rgb(.5, .5, 1, alphaLevel))
#groupColors <- adjustcolor(col=c("blue", "tomato", "orange"), alpha=alphaLevel)
#groupColors <- c("blue", "tomato", "orange")
groupColors <- rev(rainbow_hcl(n=stageCount))


# if( names(dev.cur()) != "null device" ) dev.off()
# deviceWidth <- 10 #20 #10 #6.5
# heightToWidthRatio <- .8
# windows(width=deviceWidth, height=deviceWidth*heightToWidthRatio)

ds <- data.frame(X=x, PeriodID=periodID, Theta=theta, Residual=residual, LowerY=NA, UpperY=NA)#, LowerY=lowerY, UpperY=upperY )
#breakPointsInside <- c(200, 400)
breakPointsInside <- as.numeric(quantile(ds$X, breakQuantiles))
breakPoints <- c(min(ds$X), breakPointsInside, max(ds$X))
ds$StageID <- factor(as.numeric(cut(ds$X, breaks=breakPoints, include.lowest=T)))
ds$Y <- ds$Residual + stageDifference[ds$StageID] + periodDifference[ds$PeriodID]

dsFull <- data.frame(X=rep(ds$X, stageCount), StageID=rep(ds$StageID, each=nrow(ds)), X=rep(ds$X, stageCount),PeriodID=rep(ds$PeriodID, stageCount), Theta=rep(ds$Theta, stageCount))
dsFull$Y <- 1000

for( stageID in as.numeric(sort(unique(ds$StageID))) ) {
  dsStage <- ds[ds$StageID==stageID, ]
  for( periodID in unique(dsStage$PeriodID)) {
    sliceY <- dsStage[dsStage$PeriodID == periodID, "Y"]
    bandRange <- quantile(sliceY, c(.25, .75))
    #ds[ds$StageID==stageID & ds$PeriodID==periodID, c("LowerY", "UpperY")] <- t(as.numeric(bandRange))
    ds[ds$StageID==stageID & ds$PeriodID==periodID, c("LowerY")] <- as.numeric(bandRange[1])
    ds[ds$StageID==stageID & ds$PeriodID==periodID, c("UpperY")] <- as.numeric(bandRange[2])
    
    dsFull[dsFull$StageID==stageID & dsFull$PeriodID==periodID, c("LowerY")] <- as.numeric(bandRange[1])
    dsFull[dsFull$StageID==stageID & dsFull$PeriodID==periodID, c("UpperY")] <- as.numeric(bandRange[2])
  }
}


###
### No graphics above this line; no manipulation below this line.
###


#p <- ggplot(data=ds, mapping=aes(x=X, ymin=LowerY, ymax=UpperY, color=StageID, fill=StageID))
#p <- ggplot(data=ds, mapping=aes(x=X, y=Y, ymin=LowerY, ymax=UpperY, color=StageID, fill=StageID)) + scale_x_continuous(limits=c(0, 6))
p <- ggplot(data=ds, mapping=aes(x=Theta, y=Y, ymin=LowerY, ymax=UpperY)) + scale_x_continuous(limits=c(0, 60))
#p <- ggplot(data=ds, mapping=aes(x=Theta, y=Y, ymin=LowerY, ymax=UpperY, color=StageID, fill=StageID))

p <- p + scale_y_continuous(limits=range(ds$Y))
p <- p + scale_color_manual(values=groupColors)
#p <- p +layer(geom="point", geom_params=list(size=1), stat="identity", stat_params=list()) #+scale_alpha_discrete(range = c(0, 1))
#p <- p +layer(geom="ribbon", geom_params=list(color=NA, alpha=alphaLevel), stat="identity", stat_params=list(), data=dsFull, aes(x=X, ymin=LowerY, ymax=UpperY, color=StageID, fill=StageID)) + scale_fill_manual(values=groupColors) 
#p <- p +layer(geom="ribbon", geom_params=list(color=NA, alpha=alphaLevel), stat="identity", stat_params=list(), data=dsFull, aes(x=Theta, ymin=LowerY, ymax=UpperY, color=StageID, fill=StageID)) + scale_fill_manual(values=groupColors)
#p <- p +layer(geom="line", geom_params=list(size=.5), stat="identity", stat_params=list(), mapping=aes(x=X, y=Y)) 

#p <- p +layer(geom="path", geom_params=list(size=.5), stat="identity", stat_params=list(), mapping=aes(x=Theta, y=Y))
p <- p +layer(geom="path", geom_params=list(size=.5), stat="identity", stat_params=list(), mapping=aes(x=X, y=Y))



#p <- p +layer(geom="boxplot", geom_params=list(), stat="identity", stat_params=list())
p <- p + coord_polar(theta="x")

print(p)
summary(p)