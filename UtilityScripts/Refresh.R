rm(list=ls(all=TRUE))
require(devtools)
require(staticdocs)

if( base::Sys.info()["sysname"] == "Windows" )
  base::options(device = "windows") #http://support.rstudio.org/help/discussions/problems/80-error-in-function-only-one-rstudio-graphics-device-is-permitted

devtools::document()
devtools::check_doc() #Should return NULL
#  system("R CMD Rd2pdf --force --output=./DocumentationPeek.pdf ." )

devtools::run_examples(); dev.off() #This overwrites the NAMESPACE file too
# devtools::run_examples(, "CountyMonthBirthRateDoc.Rd")
test_results <- devtools::test()
devtools::clean_vignettes()
devtools::build_vignettes()

# staticdocs::build_package(package="Wats", base_path="./../", examples=FALSE)

# system("R CMD build --resave-data .") #Then move it up one directory.
# tarBallPattern <- "^Wats_.+\\.tar\\.gz$"
# file.copy(from=list.files(pattern=tarBallPattern), to="../", overwrite=TRUE)
# system(paste("R CMD check --as-cran", list.files(pattern=tarBallPattern, path="..//", full.names=TRUE)))
# unlink(list.files(pattern=tarBallPattern))
# unlink(list.files(pattern=tarBallPattern, path="..//", full.names=TRUE))
# unlink("Wats.Rcheck", recursive=T)
# system("R CMD check --as-cran D:/Projects/RDev/Wats/Wats_0.1-1.tar.gz")

# devtools::build_win(version="R-devel") #CRAN submission policies encourage the development version
devtools::revdep_check(pkg="Wats", recursive=TRUE)
# devtools::release(check=FALSE) #Careful, the last question ultimately uploads it to CRAN, where you can't delete/reverse your decision.
# Alternatively, packages can be submitted here, which doesn't require an extra email: http://cran.r-project.org/submit.html
# Either way, check that the tarball was uploaded to ftp://cran.r-project.org/incoming/
