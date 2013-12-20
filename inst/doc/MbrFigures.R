
## ----set_root_dir, echo=FALSE--------------------------------------------
require(knitr)
opts_knit$set(root.dir = "./../") 


## ----set_options, echo=FALSE, results='hide'-----------------------------
require(knitr)
opts_chunk$set(
    comment=NA, 
    tidy=FALSE,
    fig.width=6.5, 
    fig.height=1.6,
    fig.path='figure_rmd/'
)

FigureResolution <- 60
options(width=120) #So the output is 50% wider than the default.

read_chunk("./vignettes/MbrFigures.R") 


## ----load_packages, echo=TRUE, message=FALSE-----------------------------


## ----Figure2IndividualBasic----------------------------------------------


## ----Figure2IndividualStylized-------------------------------------------


## ----Figure2Combined, fig.height=4.8, dpi=FigureResolution---------------


## ----Figure4Basic--------------------------------------------------------


## ----Figure4Stylized-----------------------------------------------------


## ----Figure5Basic--------------------------------------------------------


## ----Figure5Stylized-----------------------------------------------------


## ----session_info, echo=FALSE--------------------------------------------
cat("Report created by", Sys.info()["user"], "at", strftime(Sys.time(), "%c, %z"))
sessionInfo()


