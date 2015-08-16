library(devtools)
setwd("D:/Projects/Ouhsc/")
devtools::create(  path=file.path(getwd(), "Wats"), check=TRUE, description=list(
  "Title"= "Wrap Around Time Series graphics",
  "Description"="Wrap-around Time Series Plots (WATS Plots) for Interrupted Time Series Designs",
  "Date"="2013-12-13",
  "License"= "GPL (>= 2)",
  "Authors@R"='c(person("Will", "Beasley", role = c("aut", "cre"), email = "wibeasley@hotmail.com"),
               person("Joe", "Rodgers", role = "aut", email = "joseph.l.rodgers@vanderbilt.edu"),
               person("Matthew", "Schuelke", role = "ctb", email = "matthew.schuelke.ctr@us.af.mil"))'
))
