rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
library(ggplot2)
library(datasets)

#Create a simple dataset for the line
dsLine <- mtcars[ c("mpg", "wt")]

#Create a dataset for the ribbon and REMOVE the y variable
dsRibbon <- dsLine
dsRibbon$upper <- dsRibbon$wt + .5
dsRibbon$lower <- dsRibbon$wt - .5
dsRibbon$waylower <- dsRibbon$wt - 10

# dsRibbon$wt <- NULL

g <- ggplot(dsLine, aes_string(x="mpg", y="wt")) +
  geom_line()
g

#Setting y to NULL works with `aes` (unlike `aes_string` below).
g + geom_ribbon(mapping=aes(y=NULL, ymin=lower, ymax=upper), data=dsRibbon, alpha=.1)

#This still looks for 'wt', even though y is set to NULL.
#   The error is "Error in eval(expr, envir, enclos) : object 'wt' not found"
g + geom_ribbon(mapping=aes_string(y=NULL, ymin="lower", ymax="upper"), data=dsRibbon, alpha=.1)

#This looks for `wt`, even though the the geom_ribbon doesn't need it (correct?).
#   The error is "Error in eval(expr, envir, enclos) : object 'wt' not found"
g + geom_ribbon(mapping=aes_string(ymin="lower", ymax="upper"), data=dsRibbon, alpha=.1)

#This changes y to some variable that is present in the dataset, but not used by geom_ribbon.
#   This produces the intended graph.
g + geom_ribbon(mapping=aes_string(y="lower", ymin="lower", ymax="upper"), data=dsRibbon, alpha=.1)

#This changes y to some variable that is present in the dataset, but not used by geom_ribbon.
#   However the low value still affects the y coordinates.
g + geom_ribbon(mapping=aes_string(y="waylower", ymin="lower", ymax="upper"), data=dsRibbon, alpha=.1)
