rm(list=ls(all=TRUE)) #Clear memory from previous runs. This isn't called by knit, because it's above the chunk declaration.

## @knitr LoadPackages
library(base)
library(utils)
library(stats)
library(grDevices)
library(grid)
library(plyr) 
library(scales)
library(ggplot2) 
library(boot)
library(RVignetteExample)

## @knitr Definitions
durationInSeconds <- 1.3

## @knitr Analysis1
a1 <- RVignetteExample::ComplicatedFunction(durationInSeconds)
print(a1)

## @knitr Analysis2
a2 <- RVignetteExample::ComplicatedFunction(durationInSeconds)
print(a2)

## @knitr Graph1
graphics::plot(x=1:10, y=10:1)
graphics::text(x=1:10, y=1:10, labels=a1)

## @knitr Graph2
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1, ncol=1, respect=T)))
grid::grid.rect() #For exploring nested viewports
grid::pushViewport(grid::viewport(layout.pos.col=1, layout.pos.row=1)) 
grid::grid.rect() #For exploring nested viewports
grid::grid.text(a2, x=0, y=0, hjust=-.1, vjust=-.2, gp=grid::gpar(cex=1.5, col="blue", lineheight=.8, fill="tan1"), default.units="npc")
grid::popViewport(2)

