rm(list=ls(all=TRUE))
# library(devtools)
# library(staticdocs)

if( base::Sys.info()["sysname"] == "Windows" )
  base::options(device = "windows") #http://support.rstudio.org/help/discussions/problems/80-error-in-function-only-one-rstudio-graphics-device-is-permitted

devtools::document()
devtools::check_doc() #Should return NULL
system("R CMD Rd2pdf --no-preview --force --output=./DocumentationPeek.pdf ." )

devtools::run_examples(); dev.off() #This overwrites the NAMESPACE file too
# devtools::run_examples(, "CountyMonthBirthRateDoc.Rd")
test_results <- devtools::test()
devtools::clean_vignettes()
devtools::build_vignettes()

# staticdocs::build_package(package="Wats", base_path="./../", examples=FALSE)

# devtools::build_win(version="R-devel") #CRAN submission policies encourage the development version
# devtools::revdep_check()
# devtools::revdep_check(libpath ="D:/Projects/RLibraries")
# devtools::release(check=FALSE) #Careful, the last question ultimately uploads it to CRAN, where you can't delete/reverse your decision.
# Alternatively, packages can be submitted here, which doesn't require an extra email: http://cran.r-project.org/submit.html
# Either way, check that the tarball was uploaded to ftp://cran.r-project.org/incoming/
