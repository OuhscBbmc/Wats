rm(list=ls(all=TRUE))
library(colorspace)
library(grid)

# setwd("F:/Projects/RDev/WatsStaging/Images/")
set.seed(33)
periodLength <- 60
stage_count <- 2
stageDifference <- (c(0, 5, -2, 2, -3)/1)[seq_len(stage_count)]
breakQuantiles <- c(.3, .5, .6, .8)[seq_len(stage_count-1)]
periodDifference <- runif(periodLength)*2


pointsPerStage <- periodLength * 2
totalPeriods <- 10 #stage_count*pointsPerStage
totalPoints <- periodLength * totalPeriods
timeIndex <- seq_len(totalPoints)
periodID <- rep(seq_len(periodLength), totalPeriods)
theta <- periodID * (2*pi/periodLength)
residual <- 50 + rnorm(totalPoints, sd=5)

ds <- data.frame(TimeIndex=timeIndex, PeriodID=periodID, Theta=theta, Residual=residual, LowerY=NA, UpperY=NA)#, LowerY=lowerY, UpperY=upperY )
#breakPointsInside <- c(200, 400)
breakPointsInside <- as.numeric(quantile(ds$TimeIndex, breakQuantiles))
breakPoints <- c(min(ds$TimeIndex), breakPointsInside, max(ds$TimeIndex))
ds$stage_id <- factor(as.numeric(cut(ds$TimeIndex, breaks=breakPoints, include.lowest=T)))
ds$R <- ds$Residual + stageDifference[ds$stage_id] + periodDifference[ds$PeriodID]
ds$X <- ds$R * sin(ds$Theta)
ds$Y <- ds$R * cos(ds$Theta)

# plot(ds$X, ds$Y)
# lines(ds$X, ds$Y)

# dsFull <- data.frame(TimeIndex=rep(ds$TimeIndex, stage_count), stage_id=rep(ds$stage_id, each=nrow(ds)), X=rep(ds$X, stage_count),PeriodID=rep(ds$PeriodID, stage_count), Theta=rep(ds$Theta, stage_count))
# dsFull$Y <- 1000

for( stageID in as.numeric(sort(unique(ds$stage_id))) ){
  dsStage <- ds[ds$stage_id==stageID, ]
  for( periodID in unique(dsStage$PeriodID)) {
    sliceY <- dsStage[dsStage$PeriodID == periodID, "Y"]
    bandRange <- quantile(sliceY, c(.25, .75))
    #ds[ds$stage_id==stageID & ds$PeriodID==periodID, c("LowerY", "UpperY")] <- t(as.numeric(bandRange))
    ds[ds$stage_id==stageID & ds$PeriodID==periodID, c("LowerY")] <- as.numeric(bandRange[1])
    ds[ds$stage_id==stageID & ds$PeriodID==periodID, c("UpperY")] <- as.numeric(bandRange[2])

#     dsFull[dsFull$stage_id==stageID & dsFull$PeriodID==periodID, c("LowerY")] <- as.numeric(bandRange[1])
#     dsFull[dsFull$stage_id==stageID & dsFull$PeriodID==periodID, c("UpperY")] <- as.numeric(bandRange[2])
  }
}





###
### No graphics above this line; no manipulation below this line.
###
alphaLevel <- .2
groupColors <- rev(rainbow_hcl(n=stage_count))


# if( names(dev.cur()) != "null device" ) dev.off()
# deviceWidth <- 10 #20 #10 #6.5
# heightToWidthRatio <- 1
# windows(width=deviceWidth, height=deviceWidth*heightToWidthRatio)
grid.newpage()


#from line 295 "customgrid.Rnw"
pushViewport(viewport(layout=grid.layout(2, 2), gp=gpar(cex=0.6, fill=NA)))
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
#from line 177 "customgrid.Rnw"
pushViewport(plotViewport(c(2, 2, 2, 2)))
pushViewport(dataViewport(xData=ds$X, yData=ds$Y, name="plotRegion"))

#from line 298 "customgrid.Rnw"
grid.points(x=ds$X, y=ds$Y, gp=gpar(cex=.5))
grid.lines(x=ds$X, y=ds$Y, gp=gpar(cex=.5), default.units="native")
#grid.points(pressure$temperature, pressure$pressure, gp=gpar(cex=0.5))
grid.rect()
# grid.pol


grid.xaxis()
grid.yaxis()
#from line 224 "customgrid.Rnw"
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)

#from line 304 "customgrid.Rnw"
popViewport(3)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
#from line 177 "customgrid.Rnw"
pushViewport(plotViewport(c(5, 4, 2, 2)))
pushViewport(dataViewport(pressure$temperature, pressure$pressure, name="plotRegion"))

#from line 307 "customgrid.Rnw"
grid.points(pressure$temperature, pressure$pressure, pch=2, gp=gpar(cex=0.5))
grid.rect()
grid.xaxis()
grid.yaxis()
#from line 224 "customgrid.Rnw"
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)

#from line 313 "customgrid.Rnw"
popViewport(3)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
#from line 177 "customgrid.Rnw"
pushViewport(plotViewport(c(5, 4, 2, 2)))
pushViewport(dataViewport(pressure$temperature, pressure$pressure, name="plotRegion"))

#from line 316 "customgrid.Rnw"
grid.points(pressure$temperature, pressure$pressure, pch=2, gp=gpar(cex=0.5))
grid.rect()
grid.xaxis()
grid.yaxis()
#from line 224 "customgrid.Rnw"
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)

#from line 322 "customgrid.Rnw"
#from line 271 "customgrid.Rnw"
upViewport(2)
grid.rect(gp=gpar(lty="dashed"))

#from line 323 "customgrid.Rnw"
#from line 290 "customgrid.Rnw"
downViewport("plotRegion")
grid.text("Pressure (mm Hg)\nversus\nTemperature (Celsius)",  x=unit(150, "native"), y=unit(600, "native"))
upViewport(4)
#from line 324 "customgrid.Rnw"
