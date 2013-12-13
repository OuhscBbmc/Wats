# A set of examples illustrating what a line and rectangle look like
# when displayed in a variety of coordinate systems.  From top left to
# bottom right: Cartesian, polar with x position mapped to angle, polar
# with y position mapped to angle, flipped, transformed with log in y
# direction, and equal scales.
require(ggplot2)

line <- data.frame(x = c(1, 200), y = c(100, 1))
base <- ggplot(mapping = aes(x, y)) + geom_line(data = line)
base <- base + coord_polar("x") 
#This works:
base + scale_x_continuous(limits=c(0, 200))
#This doesn't work (presumably because 100 < 200)
#base + scale_x_continuous(limits=c(0, 100))

require(ggplot2)
spiral <- data.frame(t=0:14,r=0:14)
ggplot(spiral) + geom_line(aes(x=t %%3,y=r,group=t %/% 3)) + coord_polar()

#Hi, is still the state of multi-turn polar graphs in ggplot2?  Stavros's code produces the same described interruption, but I was curious if there's a new approach since September.  I wrote the attached graph with base graphics, and I'd like to rewrite it in ggplot2 before exposing it in a package.
