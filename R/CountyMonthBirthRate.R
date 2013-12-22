#' @name CountyMonthBirthRate
#' @docType data
#' @title Monthly Growth Fertility Rates (GFR) for 12 urban Oklahoma counties
#' 
#' @description Monthly Growth Fertility Rates (GFR) for 12 urban counties in Oklahoma
#' between January 1990 and December 1999.  The GFR is defined as the number of births divided 
#' by the number of females (ages 15-44), multiplied by 1,000.
#' 
#' @format A data frame with 1,440 observations on the following 8 variables.
#' 
#' 
#' 
#' \enumerate{ 
#'    \item{Fips}{The county's value according to the Federal Information Processing Standards.  \code{integer}}
#'    \item{CountyName}{The lower case name of the county. \code{character}} 
#'    \item{Year}{The year of the record, ranging from 1990 to 1999. \code{integer}}
#'    \item{Month}{The month of the record, ranging from 1 to 12. \code{integer}} 
#'    \item{FecundPopluation}{The number of females in the county, ages of 15 to 44. \code{integer}} 
#'    \item{BirthCount}{The number of birth in a county for the given month. \code{numeric - double precision float} 
#'    \item{Date}{The year and month of the record, with a date of the 15th. Centering the date within the month makes the value a little more representative and the graphs a little easier.\code{date}} 
#'    \item{Gfr}{The \emph{G}rowth \emph{F}ertility \emph{R}ate. \code{numeric - double precision float}} 
#' }
#' 
#' @details 
#' <<Joe, can you please finish this sentence?>>
#' The monthly birth counts were copied from county records by ZZZ during the summer of 200?.  It was collected
#' for the purposes of \href{http://www.ncbi.nlm.nih.gov/pubmed/16463916}{Rodgers, St. John, & Coleman (2005)}.
#' 
#' The US Census' intercensal estimates are used for the January values of
#' \code{FecundPopluation}.  Values for February-December are interpolated using
#' \href{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/approxfun.html}{\code{approx}}.
#' 
#' @author Will Beasley
#' @references
#' Rodgers, J. L., St. John, C. A. & Coleman R.  (2005).  
#' \href{http://www.ncbi.nlm.nih.gov/pubmed/16463916}{Did Fertility Go Up after the Oklahoma City Bombing?}  An Analysis of Births in Metropolitan Counties in Oklahoma, 1990-1999.  
#' \emph{Demography, 42}, 675-692.
#' 
#' \href{http://www.census.gov/popest/data/intercensal/st-co/characteristics.html}{Intercensal
#' estimates for 199x.}
#' 
#' \href{http://www.census.gov/popest/data/intercensal/county/county2010.html}{Intercensal
#' estimates for 200x.}
#' 
#' @keywords datasets
#' @examples 
#' require(ggplot2)
#' ggplot(dsCountyMonth, aes(x=Date, y=Gfr, color=factor(Fips))) + 
#' geom_line() +
#' labs(title="County Fertility - Longitudinal") 
#' 
#' ggplot(dsCountyMonth, aes(x=Gfr, color=factor(Fips))) + 
#' geom_density() +
#' labs(title="Distributions of County Fertility")
NULL