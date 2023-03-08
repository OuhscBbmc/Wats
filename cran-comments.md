## Description

This is a resubmission of a package that was removed from CRAN a few years ago.  It was removed because a dependency has beed removed from CRAN ([BayesSingleSub](https://CRAN.R-project.org/package=BayesSingleSub)).  The Wats package has been modified to avoid using BayesSingleSub.

Please tell me if there's anything I can modify to improve the quality of the package or make subsequent submissions easier for the CRAN team. --Will Beasley

## Test environments

* Local Ubuntu, R 4.2.2 patched
* win-builder (version="R-devel"); https://win-builder.r-project.org/0Px7c9ja34aq
* GitHub Actions, Ubuntu LTS; https://github.com/OuhscBbmc/Wats/actions
* R-hub Windows Server: https://builder.r-hub.io/status/Wats_1.0.0.tar.gz-393bad9aa18948efbcf004f50fb5b432
* R-hub Ubuntu: https://builder.r-hub.io/status/Wats_1.0.0.tar.gz-58056f8fd01f4108b5d9c11dfc9425c2
* R-hub Fedora: https://builder.r-hub.io/status/Wats_1.0.0.tar.gz-023d2d700406418c93761fbee03837cc

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

* Note on R-hub's Ubuntu platform *occasionally* says that the `annotate_data()` documentation examples are taking more than 5 elapsed sec.  But I think it's a problem with the platform because the examples take less than 0.1 elapsed seconds on my modest laptop.
  ```
  Examples with CPU (user + system) or elapsed time > 5s
                           user system elapsed
  annotate_data           1.391  0.027   6.190
  ```

## Downstream dependencies

No other packages depend/import this one.
