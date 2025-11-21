# Changelog

## Version 1.0.1 (Released 2023-03-10)

CRAN release: 2023-03-10

A modernization of the package. See Issue
[\#9](https://github.com/OuhscBbmc/Wats/issues/9) for low-level details

Breaking Changes:

- snake_case functions & variable names
  ([\#16](https://github.com/OuhscBbmc/Wats/issues/16))
- data manipulation functions accept & return tibbles instead of
  data.frames ([\#24](https://github.com/OuhscBbmc/Wats/issues/24))

New Features:

- pkgdown website for documentation
  ([\#9](https://github.com/OuhscBbmc/Wats/issues/9))
- Transition to GitHub Actions, from Travis & AppVeyor
  ([\#9](https://github.com/OuhscBbmc/Wats/issues/9))

## Version 0.10.3 (Developed 2015-11-10)

CRAN release: 2015-11-11

Changes: \* This submission is fixes a small incompatibility with the
upcoming version of ggplot2. ggplot2’s more stringent checking caught a
mistake; when calling
[`ggplot2::geom_vline`](https://ggplot2.tidyverse.org/reference/geom_abline.html),
Wats used the parameter `x`, instead of the correct `xintercept`.

## Version 0.2 (Developed from 2013-12-30 through 2014-05-24)

New Features: \* Vignette showing reanalysis with 2014 version of the
data.

Changes: \* Additional documentation and explanation \* Finalized for
resubmission to [Multivariate Behavioral
Research](http://www.tandfonline.com/loi/hmbr20) \* Tweaked default
cosmetic properties of some components \* Adding Reproduce file \*
Modified and relocated some package dependencies \* Changed from GPL-2
to MIT license \* Upgraded to Roxygen 4.0.0 \* Upgraded to ggplot2 1.0.0

Bug Fixes \*
[`annotate_data()`](https://ouhscbbmc.github.io/Wats/reference/annotate_data.md)
properly accepts the `center_function` parameter, and doesn’t revert to
`median`. Thanks Mark Joseph Lachowicz
(<mark.j.lachowicz@vanderbilt.edu>). GitHub Issue
[\#2](https://github.com/OuhscBbmc/Wats/issues/2). \* Removed
unnecessary aes setting of y in cartesian_periodic.R

## Version 0.1 (Developed from 2010 through 2013-12-11)

New Features: \* `AugmentZZZ()` and
[`annotate_data()`](https://ouhscbbmc.github.io/Wats/reference/annotate_data.md)
functions. \*
[`cartesian_rolling()`](https://ouhscbbmc.github.io/Wats/reference/cartesian_rolling.md),
[`cartesian_periodic()`](https://ouhscbbmc.github.io/Wats/reference/cartesian_periodic.md),
and
[`polar_periodic()`](https://ouhscbbmc.github.io/Wats/reference/polar_periodic.md)
functions. \* Vignette to reproduce figures in MBR manuscript.
