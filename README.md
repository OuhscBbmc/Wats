## Welcome to the `Wats` Package
This implements the approaches described in
Rodger, Joseph Lee, William Howard Beasley, and Matthew Schuelke (in submission).
Wrap-around Time Series Plots (WATS Plots) for Interrupted Time Series Designs:
Applications to Fertility Rates and the Oklahoma City Bombing.

## Article Abstract
> Many data structures, particular time series data, are naturally seasonal, cyclical, or otherwise circular.  Past graphical methods for time series have focused on linear plots.  In this paper, we move graphical analysis onto the circle.  We focus on two methods, one old and one new.  Rose diagrams are circular histograms, and can be produced in several different forms using the RRose software system.  In addition, we propose, develop, and illustrate a new circular graphical method, called Wrap-Around Time Series Plots (WATS plots) that is useful to support time series analyses in general, but in particular in relation to interrupted time series designs.  We illustrate the use of WATS Plots from an interrupted time series design evaluating the effect of the Oklahoma City bombing on birth rates in Oklahoma County during the ten years surrounding the bombing of the Murrah Building in Oklahoma City.  We compare WATS Plots to linear time series representations with smoothing.  Each method is shown to have advantages in relation to the other; in our example, the WATS Plots more clearly show the existence and effect size of the fertility differential.

> Keywords:  time series, interrupted time series design, group differences, graphical analysis, circular data, H-spread

## Reproducible Research
When the Wats package is installed on your local machine, the [`Reproduce.R`](https://github.com/wibeasley/Wats/blob/master/UtilityScripts/Reproduce.R) script starts with our initial datasets (ie, the vital statistics birth counts and the US Census population estimates) to create the derivative datasets and resulting graphs and analysis.

## Nonstandard Directories
The following directories are not part of the standard R package:
 * `Datasets`: CSV versions of the *.rda data.frames officially included in the package.
 * `DocuemtnationForDevelopers`: Notes and links that should help package *developers* set up on their computer.  Typical package *users* won't have a need for this.
 * `PublicationGraphs`: A deprecated location that contains loose graphs of older versions of the manuscript.
 * `Playgrounds`: R snippets to help developers experiment with potential new features.
 * `UtilityScripts`: R scripts that aren't incorpated into the package.  They help automate certain tasks, or document how parts of the package were created.


