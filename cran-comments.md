## Description

The package was removed from CRAN a few years ago because a dependency was removed from CRAN ([BayesSingleSub](https://CRAN.R-project.org/package=BayesSingleSub)).  The Wats package has been modified to avoid using it.

-Will Beasley

## Test environments

* Local Ubuntu, R 4.2.2 patched
* win-builder (version="R-devel"); https://win-builder.r-project.org/0Px7c9ja34aq
* GitHub Actions, Ubuntu LTS; https://github.com/OuhscBbmc/Wats/actions
* R-hub Windows Server: https://builder.r-hub.io/status/Wats_0.11.1.9000.tar.gz-b306d7ad1ec942a68780cc667ee86975
* R-hub Ubuntu: https://builder.r-hub.io/status/Wats_0.11.1.9000.tar.gz-f7bd4b19f5b84b8cb5e6945cb6120f26
* R-hub Fedora: https://builder.r-hub.io/status/Wats_0.11.1.9000.tar.gz-389c7794fb4b4f63945b5c99e243cd2f

## R CMD check results

* No ERRORs or WARNINGs on any builds.

* NOTE on all test platforms: Package was archived on CRAN

* NOTE on R-hub's Windows platform that appears to be a problem with the platform:
  > checking for detritus in the temp directory ... NOTE
  > Found the following files/directories:
  >   'lastMiKTeXException'

* Note on R-hub's Fedora platform that appears to a be a problem with the [testing platform](https://stackoverflow.com/a/75007979/1082435)
  > checking HTML version of manual ... NOTE
  > Skipping checking HTML validation: no command 'tidy' found


## Downstream dependencies

No other packages depend/import this one.
