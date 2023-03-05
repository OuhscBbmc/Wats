rm(list=ls(all=TRUE))
library(ggplot2)
library(colorspace)
# setwd("F:/Projects/RDev/WatsStaging/Images/")
set.seed(33)
periodLength <- 6
stage_count <- 2
stageDifference <- (c(0, 5, -2, 2, -3)/1)[seq_len(stage_count)]
breakQuantiles <- c(.3, .5, .6, .8)[seq_len(stage_count-1)]
periodDifference <- runif(periodLength)*2


pointsPerStage <- periodLength * 2
totalPeriods <- 10 #stage_count*pointsPerStage
totalPoints <- periodLength * totalPeriods
x <- seq_len(totalPoints)
periodID <- rep(seq_len(periodLength), totalPeriods)
theta <- periodID / (2*pi)
residual <- 50 + rnorm(totalPoints, sd=5)

alphaLevel <- .2
#groupColors <- c(rgb(1, .5, .5, alphaLevel), rgb(.5, 1, .5, alphaLevel), rgb(.5, .5, 1, alphaLevel))
#groupColors <- adjustcolor(col=c("blue", "tomato", "orange"), alpha=alphaLevel)
#groupColors <- c("blue", "tomato", "orange")
groupColors <- rev(rainbow_hcl(n=stage_count))


# if( names(dev.cur()) != "null device" ) dev.off()
# deviceWidth <- 10 #20 #10 #6.5
# heightToWidthRatio <- .8
# windows(width=deviceWidth, height=deviceWidth*heightToWidthRatio)

ds <- tibble::tibble(X=x, PeriodID=periodID, theta=theta, Residual=residual, lower_y=NA, upper_y=NA)#, lower_y=lower_y, upper_y=upper_y )
#breakPointsInside <- c(200, 400)
breakPointsInside <- as.numeric(quantile(ds$X, breakQuantiles))
breakPoints <- c(min(ds$X), breakPointsInside, max(ds$X))
ds$stage_id <- factor(as.numeric(cut(ds$X, breaks=breakPoints, include.lowest=T)))
ds$Y <- ds$Residual + stageDifference[ds$stage_id] + periodDifference[ds$PeriodID]

dsFull <- tibble::tibble(X=rep(ds$X, stage_count), stage_id=rep(ds$stage_id, each=nrow(ds)), X=rep(ds$X, stage_count),PeriodID=rep(ds$PeriodID, stage_count), theta=rep(ds$theta, stage_count))
dsFull$Y <- 1000

for( stageID in as.numeric(sort(unique(ds$stage_id))) ) {
  ds_stage <- ds[ds$stage_id==stageID, ]
  for( periodID in unique(ds_stage$PeriodID)) {
    sliceY <- ds_stage[ds_stage$PeriodID == periodID, "Y"]
    bandRange <- quantile(sliceY, c(.25, .75))
    #ds[ds$stage_id==stageID & ds$PeriodID==periodID, c("lower_y", "upper_y")] <- t(as.numeric(bandRange))
    ds[ds$stage_id==stageID & ds$PeriodID==periodID, c("lower_y")] <- as.numeric(bandRange[1])
    ds[ds$stage_id==stageID & ds$PeriodID==periodID, c("upper_y")] <- as.numeric(bandRange[2])

    dsFull[dsFull$stage_id==stageID & dsFull$PeriodID==periodID, c("lower_y")] <- as.numeric(bandRange[1])
    dsFull[dsFull$stage_id==stageID & dsFull$PeriodID==periodID, c("upper_y")] <- as.numeric(bandRange[2])
  }
}


###
### No graphics above this line; no manipulation below this line.
###


#p <- ggplot(data=ds, mapping=aes(x=X, ymin=lower_y, ymax=upper_y, color=stage_id, fill=stage_id))
#p <- ggplot(data=ds, mapping=aes(x=X, y=Y, ymin=lower_y, ymax=upper_y, color=stage_id, fill=stage_id)) + scale_x_continuous(limits=c(0, 6))
p <- ggplot(data=ds, mapping=aes(x=theta, y=Y, ymin=lower_y, ymax=upper_y)) + scale_x_continuous(limits=c(0, 60))
#p <- ggplot(data=ds, mapping=aes(x=theta, y=Y, ymin=lower_y, ymax=upper_y, color=stage_id, fill=stage_id))

p <- p + scale_y_continuous(limits=range(ds$Y))
p <- p + scale_color_manual(values=groupColors)
#p <- p +layer(geom="point", geom_params=list(size=1), stat="identity", stat_params=list()) #+scale_alpha_discrete(range = c(0, 1))
#p <- p +layer(geom="ribbon", geom_params=list(color=NA, alpha=alphaLevel), stat="identity", stat_params=list(), data=dsFull, aes(x=X, ymin=lower_y, ymax=upper_y, color=stage_id, fill=stage_id)) + scale_fill_manual(values=groupColors)
#p <- p +layer(geom="ribbon", geom_params=list(color=NA, alpha=alphaLevel), stat="identity", stat_params=list(), data=dsFull, aes(x=theta, ymin=lower_y, ymax=upper_y, color=stage_id, fill=stage_id)) + scale_fill_manual(values=groupColors)
#p <- p +layer(geom="line", geom_params=list(size=.5), stat="identity", stat_params=list(), mapping=aes(x=X, y=Y))

#p <- p +layer(geom="path", geom_params=list(size=.5), stat="identity", stat_params=list(), mapping=aes(x=theta, y=Y))
p <- p +layer(geom="path", geom_params=list(size=.5), stat="identity", stat_params=list(), mapping=aes(x=X, y=Y))



#p <- p +layer(geom="boxplot", geom_params=list(), stat="identity", stat_params=list())
p <- p + coord_polar(theta="x")

print(p)
summary(p)