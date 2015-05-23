<!-- rmarkdown v1 -->

| [GitHub](https://github.com/OuhscBbmc/Wats) | [Travis-CI](https://travis-ci.org/OuhscBbmc/Wats/builds) | [AppVeyor](https://ci.appveyor.com/project/wibeasley/Wats/history) | [Coveralls](https://coveralls.io/r/OuhscBbmc/Wats) |
| :----- | :---------------------------: | :-----------------------------: | :-------: |
| [Master](https://github.com/OuhscBbmc/Wats/tree/master) | [![Build Status](https://travis-ci.org/OuhscBbmc/Wats.svg?branch=master)](https://travis-ci.org/OuhscBbmc/Wats) | [![Build status](https://ci.appveyor.com/api/projects/status/2wkbuwqcmog4nhxf/branch/master?svg=true)](https://ci.appveyor.com/project/wibeasley/wats/branch/master) | [![Coverage Status](https://coveralls.io/repos/wibeasley/Wats/badge.svg?branch=master)](https://coveralls.io/r/OuhscBbmc/Wats?branch=master) |
| [Dev](https://github.com/OuhscBbmc/Wats/tree/dev) | [![Build Status](https://travis-ci.org/OuhscBbmc/Wats.svg?branch=dev)](https://travis-ci.org/OuhscBbmc/Wats) | [![Build status](https://ci.appveyor.com/api/projects/status/2wkbuwqcmog4nhxf/branch/dev?svg=true)](https://ci.appveyor.com/project/wibeasley/wats/branch/dev) | [![Coverage Status](https://coveralls.io/repos/OuhscBbmc/Wats/badge.svg?branch=dev)](https://coveralls.io/r/OuhscBbmc/Wats?branch=dev) | -- |
| | *Ubuntu 12.04 LTS* | *Windows Server 2012* | *Test Coverage* | *Independently-hosted Archive* |

## Welcome to the `Wats` Package
This implements the approaches described in
[Joseph Lee Rodgers](http://www.vanderbilt.edu/psychological_sciences/bio/joe-rodgers), [William Howard Beasley](https://scholar.google.com/citations?user=ffsJTC0AAAAJ), and Matthew Schuelke (2014).
[Wrap-around Time Series Plots (WATS Plots) for Interrupted Time Series Designs:
Applications to Fertility Rates and the Oklahoma City Bombing.](http://www.tandfonline.com/doi/abs/10.1080/00273171.2014.946589) *Multivariate Behavioral Research*.

The figures can be viewed in the vignettes, or in the [Handouts](https://github.com/OuhscBbmc/Wats/blob/master/UtilityScripts/Handouts.md) document.  The release version of Wats is available on [CRAN](http://cran.r-project.org/web/packages/Wats/).

## Article Abstract
> Many data structures, particular time series data, are naturally seasonal, cyclical, or otherwise circular.  Past graphical methods for time series have focused on linear plots.  In this paper, we move graphical analysis onto the circle.  We focus on two methods, one old and one new.  Rose diagrams are circular histograms, and can be produced in several different forms using the RRose software system.  In addition, we propose, develop, and illustrate a new circular graphical method, called Wrap-Around Time Series Plots (WATS plots) that is useful to support time series analyses in general, but in particular in relation to interrupted time series designs.  We illustrate the use of WATS Plots from an interrupted time series design evaluating the effect of the Oklahoma City bombing on birth rates in Oklahoma County during the ten years surrounding the bombing of the Murrah Building in Oklahoma City.  We compare WATS Plots to linear time series representations with smoothing.  Each method is shown to have advantages in relation to the other; in our example, the WATS Plots more clearly show the existence and effect size of the fertility differential.

> Keywords:  time series, interrupted time series design, group differences, graphical analysis, circular data, H-spread

## Selected Figures
#### Figure 2

<img src="./vignettes/figure_mbr_rmd/Figure2Stylized-1.png" alt="Figure2Stylized" style="width: 600px;"/>
#### Figure 4

<img src="./vignettes/figure_mbr_rmd/Figure6-1.png" alt="Figure6" style="width: 600px;"/>

## Reproducible Research
When the Wats package is installed on your local machine, the [`Reproduce.R`](https://github.com/OuhscBbmc/Wats/blob/master/UtilityScripts/Reproduce.R) script starts with our initial datasets (ie, the vital statistics birth counts and the US Census population estimates) to create the derivative datasets and resulting graphs and analysis.

## Nonstandard Directories
The following directories are not part of the standard R package:
 * `Datasets`: CSV versions of the *.rda data.frames officially included in the package.
 * `DocumentationForDevelopers`: Notes and links that should help package *developers* set up on their computer.  Typical package *users* won't have a need for this.
 * `PublicationGraphs`: A deprecated location that contains loose graphs of older versions of the manuscript.
 * `Playgrounds`: R snippets to help developers experiment with potential new features.
 * `UtilityScripts`: R scripts that aren't incorpated into the package.  They help automate certain tasks, or document how parts of the package were created.

## Installing 

| [CRAN](http://cran.rstudio.com/) | [Version](http://cran.r-project.org/web/packages/Wats/) | [Rate](http://cranlogs.r-pkg.org/) | [Zenodo](https://zenodo.org/search?ln=en&p=redcapr) | 
|  :---- | :----: | :----: | :----: |
| [Latest](http://cran.r-project.org/web/packages/Wats/) | [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Wats)](http://cran.r-project.org/web/packages/Wats) | ![CRANPace](http://cranlogs.r-pkg.org/badges/Wats) | [![DOI](https://zenodo.org/badge/4971/OuhscBbmc/Wats.png)](http://dx.doi.org/10.5281/zenodo.11921) |
|   | *Latest CRAN version* | *CRAN Downloads* | *Independently-hosted Archive* |

The released CRAN version of Wats can be installed through R with.
```R
install.packages("Wats")
```

The latest development version of Wats can be installed from [GitHub](https://github.com/OuhscBbmc/Wats) after installing the `devtools` package.
```R
install.packages("devtools")
devtools::install_github(repo="OuhscBbmc/Wats")
```

## Code Repository
The software is written primarily in R, under the MIT License.  The DOI of this major release of the *repository* is [10.5281/zenodo.11921](http://dx.doi.org/10.5281/zenodo.11921).  (The DOI of the article is [10.1080/00273171.2014.946589](http://www.tandfonline.com/doi/abs/10.1080/00273171.2014.946589).)
