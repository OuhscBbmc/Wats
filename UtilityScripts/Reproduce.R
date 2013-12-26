###################################
### Reproducible Research
###################################
# When executed by R, this file will manipulated the original data sources (Oklahoma vital statistics
# and US Census estimates) to produce a groomed dataset suitable for analysis and graphing.

rm(list=ls(all=TRUE))

source("./UtilityScripts/IsolateCensusPopsForGfr.R")
source("./UtilityScripts/CalculateGfr.R")


# path_md <- gsub(pattern=".Rmd$", replacement=".md", x=path_rmd)
# path_html <- gsub(pattern=".Rmd$", replacement=".html", x=path_rmd)
# knit(input=path_rmd, output=path_md)
# markdownToHTML(file=path_md, output=path_html) #The markdownToHTML function is working correctly now.  The workaround isn't necessary.
