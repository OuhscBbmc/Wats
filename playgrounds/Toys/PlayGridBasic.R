rm(list=ls(all=TRUE))
library(colorspace)
library(grid)
#grid.lines()
# Using id (NOTE: locations are not in consecutive blocks)
grid.newpage()
# x <- 1:10
# y <- runif(length(x))

# pushViewport(viewport(layout=grid.layout(2, 2), gp=gpar(cex=0.6, fill=NA)))
# pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
# #line 177 "customgrid.Rnw"
#pushViewport(plotViewport(c(2, 2, 2, 2)))
# #pushViewport(dataViewport(xData=x, yData=y, name="plot_region"))
#


#grid.lines(x=x, y=y, size = unit(1, "char"), gp=gpar(col=1:5, lwd=3))
#grid.polyline(x=x, y=y, id=rep(1:5, 4), gp=gpar(col=1:5, lwd=3))
#grid.points(x=x, y=y, size = unit(1, "char"),  gp=gpar(lwd=1))
#grid.points(x=x, y=y, size = unit(1, "char"),  gp=gpar(lwd=1))





grid.rect(gp=gpar(col="gray"))

n <- 50
angle <- seq(0, 2*pi, length=n)
#x <- seq(0.1, 0.5, length=n)
x <- seq_len(n)
y <- 0.5 + 0.3*sin(angle)

# x <- seq(0.1, 0.9, length=50)
# y <- runif(length(x), 0.1, 0.9)

dv <- dataViewport(xData=x, yData=y, name="plot_region")
pushViewport(dv)
#pushViewport(viewport())
# grid.rect()
# grid.xaxis()
# grid.yaxis()

#unit <- "npc"
unit <- "native"
grid.lines(x, y, default.units=unit)
grid.points(x, y, default.units=unit)

# grid.draw(linesGrob(x, y, default.units=unit))
# grid.draw(pointsGrob(x, y, default.units=unit))

# grid.lines(x=x, y=y, vp=dv)
# grid.points(x=x, y=y, vp=dv)
#grid.segments(6:8/10, 0.2, 7:9/10, 0.8, arrow=arrow(angle=15, type="closed"))