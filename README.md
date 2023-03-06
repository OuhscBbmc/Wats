WATS
==========================

Welcome to the `Wats` Package. This implements the approaches described in
[Joseph Lee Rodgers](https://www.vanderbilt.edu/psychological_sciences/bio/joe-rodgers), [William Howard Beasley](https://scholar.google.com/citations?user=ffsJTC0AAAAJ), and Matthew Schuelke (2014).
[Wrap-around Time Series Plots (WATS Plots) for Interrupted Time Series Designs:
Applications to Fertility Rates and the Oklahoma City Bombing.](https://www.tandfonline.com/doi/abs/10.1080/00273171.2014.946589) *Multivariate Behavioral Research*.

The figures can be viewed in the vignettes, or in the [Handouts](https://github.com/OuhscBbmc/Wats/blob/main/utility/handouts.md) document.  The release version of Wats is available on [CRAN](https://cran.r-project.org/package=Wats).

Article Abstract
-------------------------

> Many data structures, particular time series data, are naturally seasonal, cyclical, or otherwise circular.  Past graphical methods for time series have focused on linear plots.  In this paper, we move graphical analysis onto the circle.  We focus on two methods, one old and one new.  Rose diagrams are circular histograms, and can be produced in several different forms using the RRose software system.  In addition, we propose, develop, and illustrate a new circular graphical method, called Wrap-Around Time Series Plots (WATS plots) that is useful to support time series analyses in general, but in particular in relation to interrupted time series designs.  We illustrate the use of WATS Plots from an interrupted time series design evaluating the effect of the Oklahoma City bombing on birth rates in Oklahoma County during the ten years surrounding the bombing of the Murrah Building in Oklahoma City.  We compare WATS Plots to linear time series representations with smoothing.  Each method is shown to have advantages in relation to the other; in our example, the WATS Plots more clearly show the existence and effect size of the fertility differential.

> Keywords: time series, interrupted time series design, group differences, graphical analysis, circular data, H-spread

Selected Figures
-------------------------

### Figure 2

<img src="https://raw.githubusercontent.com/OuhscBbmc/Wats/main/vignettes/figure-mbr-rmd/fig-2-stylized-1.png" alt="fig-2-stylized" style="width: 600px;"/>

### Figure 4

<img src="https://raw.githubusercontent.com/OuhscBbmc/Wats/main/vignettes/figure-mbr-rmd/fig-6-1.png" alt="fig-6" style="width: 600px;"/>

Reproducible Research
-------------------------

When the Wats package is installed on your local machine, the [`Reproduce.R`](https://github.com/OuhscBbmc/Wats/blob/main/utility/reproduce.R) script starts with our initial datasets (ie, the vital statistics birth counts and the US Census population estimates) to create the derivative datasets and resulting graphs and analysis.

Nonstandard Directories
-------------------------

The following directories are not part of the standard R package:

 * `datasets`: CSV versions of the *.rda data.frames officially included in the package.
 * `documentation-for-developers`: Notes and links that should help package *developers* set up on their computer.  Typical package *users* won't have a need for this.
 * `publication-graphs`: A deprecated location that contains loose graphs of older versions of the manuscript.
 * `playgrounds`: R snippets to help developers experiment with potential new features.
 * `utility`: R scripts that aren't incorporated into the package.  They help automate certain tasks, or document how parts of the package were created.

Installing
-------------------------

The released [CRAN](https://cran.r-project.org/package=Wats) version of Wats can be installed through R with.

```R
install.packages("Wats")
```

The latest development version of Wats can be installed from [GitHub](https://github.com/OuhscBbmc/Wats) after installing the `remotes` package.

```R
install.packages("remotes")
remotes::install_github("OuhscBbmc/Wats")
```

Code Repository
-------------------------

The software is written primarily in R, under the MIT License.  The DOI of this major release of the *repository* is [10.5281/zenodo.11921](https://doi.org/10.5281/zenodo.11921).  (The DOI of the article is [10.1080/00273171.2014.946589](https://www.tandfonline.com/doi/abs/10.1080/00273171.2014.946589).)

Build Status and Package Characteristics
-------------------------

| [Branch](https://github.com/OuhscBbmc/Wats) | [GitHub Actions](https://github.com/OuhscBbmc/Wats/actions) | [Codecov](https://app.codecov.io/gh/OuhscBbmc/Wats) |
| :----- | :---------------------------: | :-------: |
| [Main](https://github.com/OuhscBbmc/Wats/tree/main) | [![R-CMD-check](https://github.com/OuhscBbmc/Wats/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/OuhscBbmc/Wats/actions) |  [![codecov](https://codecov.io/gh/OuhscBbmc/Wats/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OuhscBbmc/Wats/branch/main) |
| [Dev](https://github.com/OuhscBbmc/Wats/tree/dev) | [![R-CMD-check](https://github.com/OuhscBbmc/Wats/workflows/R-CMD-check/badge.svg?branch=dev)](https://github.com/OuhscBbmc/Wats/actions) | [![codecov](https://codecov.io/gh/OuhscBbmc/Wats/branch/dev/graph/badge.svg)](https://app.codecov.io/gh/OuhscBbmc/Wats/branch/dev) |
| | *Ubuntu Latest* | *Test Coverage* |

| Key | Value |
| :--- | :----- |
| [License](https://choosealicense.com/) | [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://choosealicense.com/licenses/mit/) |
| [CRAN Version](https://cran.r-project.org/package=Wats) | [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/Wats)](https://cran.r-project.org/package=Wats) |
| [CRAN Rate](https://cranlogs.r-pkg.org/) | ![CRAN Pace](https://cranlogs.r-pkg.org/badges/Wats) |
| [Zenodo Archive](https://zenodo.org/search?ln=en&p=Wats) | [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11921.svg)](https://doi.org/10.5281/zenodo.11921) |
| [Production Doc](https://www.rdocumentation.org/) | [![RDoc](https://api.rdocumentation.org/badges/version/Wats)](https://www.rdocumentation.org/packages/Wats) |
| [Development Doc](https://ouhscbbmc.github.io/Wats/) | [![rdoc](https://img.shields.io/badge/pkgdown-GitHub.io-orange.svg?longCache=true&style=style=for-the-badge)](https://ouhscbbmc.github.io/Wats/) |
