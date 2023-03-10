## Description

This is a resubmission of a package that was removed from CRAN a few years ago.  It was removed because a dependency has been removed from CRAN ([BayesSingleSub](https://CRAN.R-project.org/package=BayesSingleSub)).  The Wats package has been modified to avoid using BayesSingleSub.

Yesterday the package was rejected with [five suggestions](https://github.com/OuhscBbmc/Wats/issues/26#issuecomment-1463104664).  (Thank you, Victoria for the helpful context and comments.)  I have addressed all five:

1. I referenced the article in the description field.  I had trouble understanding how the angled brackets should be used, and followed the examples of [xgboost](https://github.com/dmlc/xgboost/blob/master/R-package/DESCRIPTION) and [Rcpp](https://github.com/RcppCore/Rcpp/blob/master/DESCRIPTION).
2. I added more about the graph
3. I replaced `dontrun{}` with `donttest{}`.  I agree it shouldn't be a problem to run it all, but it occasionally exceeds 5sec on R-hub.  CRAN's servers may be more powerful, but I don't want to risk it.  I've been ripleyed for less.
4. I removed the call to `options()` entirely.  It was unnecessary anyway.
5. I removed the example of installing the package (even though it was in a `dontrun{}` block).  When I wrote that in 2014, fewer people knew how to install dev versions.

Please tell me if there's anything I can modify to improve the quality of the package or make subsequent submissions easier for the CRAN team. --Will Beasley

## Test environments

* Local Ubuntu, R 4.2.2 patched
* win-builder (version="R-devel"); https://win-builder.r-project.org/B137tudl1V6e
* GitHub Actions, Ubuntu LTS; https://github.com/OuhscBbmc/Wats/actions
* R-hub Windows Server: https://builder.r-hub.io/status/Wats_1.0.1.tar.gz-af05daf88ecc49aabb3d90e3704df552
* R-hub Ubuntu: https://builder.r-hub.io/status/Wats_1.0.1.tar.gz-fb2f91510cab4d759b32b02a830e8495
* R-hub Fedora: https://builder.r-hub.io/status/Wats_1.0.1.tar.gz-c07f9758c4c74863a8e2c66b865a122b

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
