rm(list=ls(all=TRUE))
library(colorspace)
library(grid)
#grid.lines()
# Using id (NOTE: locations are not in consecutive blocks)
grid.newpage()

#line 1098 "objectgrid.Rnw"
#label <- textGrob("A\nPlot\nLabel ", x=0, just="left")
x <- seq(0.1, 0.9, length=50)
y <- runif(length(x), 0.1, 0.9)
# gplot <- gTree(
#   children=gList(
#     rectGrob(gp=gpar(col="gray60", fill="white")), 
#     linesGrob(x, y), 
#     pointsGrob(x, y, pch=16,  size=unit(1.5, "mm"))
#   ),
#   vp=viewport(width=unit(1, "npc") - unit(5, "mm"), height=unit(1, "npc") - unit(5, "mm"))
# )



#line 1128 "objectgrid.Rnw"
#layout <- grid.layout(1, 2, widths=unit(c(1, 1), c("null", "grobwidth"), list(NULL, label)))



#line 1161 "objectgrid.Rnw"
#grid.rect(gp=gpar(col="gray60", fill="gray90"))
#line 1152 "objectgrid.Rnw"
#pushViewport(viewport(layout=layout))
# pushViewport(viewport(layout.pos.col=2))
# grid.draw(label)
# popViewport()
#pushViewport(viewport(layout.pos.col=1))
pushViewport(viewport())
grid.rect(gp=gpar(col="gray60", fill="gray90"))
#grid.draw(gplot)
grid.draw(linesGrob(x, y))
grid.draw(pointsGrob(x, y))
#popViewport(2)
popViewport(1)

#line 1163 "objectgrid.Rnw"

