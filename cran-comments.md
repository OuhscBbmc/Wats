## Description
This submission is fixes a small incompatibility with the [upcoming version of ggplot2](https://github.com/hadley/ggplot2/blob/47679a52fd2a3ac89583da923dc9882c0020866c/revdep/summary.md#wats-0101).  ggplot2's more stringent checking caught a mistake in my package.  When calling `ggplot2::geom_vline`, I used the parameter `x`, instead of the correct `xintercept`.  This [has been fixed](https://github.com/OuhscBbmc/Wats/commit/1e3543a17feff9bad7b77152ca457e6fdf1cf440).  Please tell me if there's something else I should do for CRAN.

-Will Beasley

## Test environments
* Local Win8, R 3.2.2 patched
* win-builder (version="R-devel"); http://win-builder.r-project.org/e7AbG2GnK8Xa
* Travis CI, Ubuntu 12.04 LTS; https://travis-ci.org/OuhscBbmc/Wats
* AppVeyor, Windows Server 2012; https://ci.appveyor.com/project/wibeasley/wats

## R CMD check results
* No ERRORs or WARNINGs on any builds.
* No NOTEs on win-builder.
* No other unexplainable NOTEs on the other builds.

## Downstream dependencies
No other packages depend/import this one.
