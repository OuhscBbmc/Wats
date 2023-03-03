rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

# Import only certain functions of a package into the search path.
# import::from("magrittr", "%>%")

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
# requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"       )
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
# requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
inputPathBirthCountCountyMonth          <- "datasets/raw/birth-count-county.csv"
inputPathCensusCountyMonth              <- "datasets/derived/census-county-month.csv"
outputPathBirthCountCountyMonthCsv2014  <- "datasets/derived/county-month-birth-rate-2014-version.csv"
outputPathBirthCountCountyMonthCsv2005  <- "datasets/derived/county-month-birth-rate-2005-version.csv"
outputPathBirthCountCountyMonthRda2014  <- "data/county_month_birth_rate_2014_version.rda"
outputPathBirthCountCountyMonthRda2005  <- "data/county_month_birth_rate_2005_version.rda"
change_month                            <- as.Date("1996-02-15")

# OuhscMunge::readr_spec_aligned(inputPathCensusCountyMonth)
col_types_census <-
  readr::cols_only(
    `fips`                = readr::col_character(),
    `county_name`         = readr::col_character(),
    `year`                = readr::col_integer(),
    `month`               = readr::col_integer(),
    `fecund_population`   = readr::col_integer()
  )

# OuhscMunge::readr_spec_aligned(inputPathBirthCountCountyMonth)

col_types_birth_count <- readr::cols_only(
  `county_name`   = readr::col_character(),
  `year`          = readr::col_integer(),
  `month`         = readr::col_integer(),
  `birth_count`   = readr::col_integer()
)
# ---- load-data ---------------------------------------------------------------
ds_census       <- readr::read_csv(inputPathCensusCountyMonth, col_types = col_types_census)
ds_birth_count  <- readr::read_csv(inputPathBirthCountCountyMonth, col_types = col_types_birth_count)

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_census) # Help write `dplyr::select()` call.
ds_census <-
  ds_census |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    fips,
    county_name,
    year,
    month,
    fecund_population,
  )

# ---- groom-birth-count -------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_birth_count) # Help write `dplyr::select()` call.
ds_birth_count <-
  ds_birth_count |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    county_name,
    year,
    month,
    birth_count,
  ) |>
  dplyr::mutate(
    dummy       = TRUE,
    year        = 1900L + year,

    county_name  =
      dplyr::case_match(
        county_name,
        "canadian"  ~ "canadian",
        "clevelan"  ~ "cleveland",
        "comanche"  ~ "comanche",
        "creek"     ~ "creek",
        "logan"     ~ "logan",
        "mcclain"   ~ "mcclain",
        "oklahoma"  ~ "oklahoma",
        "osage"     ~ "osage",
        "pottawat"  ~ "pottawatomie",
        "rogers"    ~ "rogers",
        "tulsa"     ~ "tulsa",
        "wagoner"   ~ "wagoner"
      ),
  )

# ---- join --------------------------------------------------------------------
ds_county_month <-
  ds_census |>
  dplyr::left_join(
    ds_birth_count,
    by = c("county_name", "year", "month")
  ) |>
  dplyr::mutate(
    date        = as.Date(ISOdate(year, month, 15L)),
    days_in_month = lubridate::days_in_month(date),
    days_in_year  = as.integer(365L + lubridate::leap_year(date)),

    stage_id      = dplyr::if_else(date < change_month, 1L, 2L), # Define pre/post bombing stages (+9 months)
  )

testit::assert("All left records should find a right record", all(ds_county_month$dummy))
ds_county_month$dummy <- NULL

rm(ds_census, ds_birth_count, inputPathCensusCountyMonth, inputPathBirthCountCountyMonth)


# ---- calculate-gfr -----------------------------------------------------------
# Calculate GFR for the 2005 and the 2014 Versions
ds_county_month_2014 <-  ds_county_month #This is what fertility researchers should use.
ds_county_month_2005 <-  ds_county_month #This is better for 2014 article, and recreates the 2005 article.

#The 2014 version uses the interpolated
ds_county_month_2014 <-
  ds_county_month_2014 |>
  dplyr::mutate(
    # Adjust for months of unequal days.  Each monthly record is multiplied by about 12.
    birth_rate_monthly = birth_count / fecund_population * 1000L,
    birth_rate         = birth_rate_monthly * days_in_year / days_in_month,
  ) |>
  dplyr::mutate(
    birth_rate_monthly = round(birth_rate_monthly  , 2),
    birth_rate         = round(birth_rate          , 2),
  )

# To recreate the 2005 paper, use only the 1990 estimate.
ds_county_month_2005 <-
  ds_county_month_2005 |>
  dplyr::group_by(fips) |>
  dplyr::mutate(
    birth_rate_monthly = (birth_count / fecund_population[1] * 1000L),
    birth_rate         = birth_rate_monthly * days_in_year / days_in_month,
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    birth_rate_monthly = round(birth_rate_monthly  , 2),
    birth_rate         = round(birth_rate          , 2),
  )

# library(ggplot2)
# ggplot(ds_county_month, aes(x=date, y=birth_rate, color=factor(fips))) + geom_line() + labs(title="Distributions of County Fertility")
# ggplot(ds_county_month, aes(x=birth_rate, color=factor(fips))) + geom_density()

# filePathOutcomes <- file.path(devtools::inst(name="Wats"), "extdata", "BirthRatesOk.txt")
# dsOld <- read.table(file=filePathOutcomes, header=TRUE, sep="\t", stringsAsFactors=FALSE)
# dsOld$date <- as.Date(dsOld$date) + days(15)
#
# ggplot(ds_county_month[ds_county_month$fips==40109, ], aes(x=date, color=factor(fips))) +
#   geom_line(aes(y=birth_rate), color="tomato") +
#   geom_line(aes(y=BirthRateUnadjustedFrom1990), color="blue") +
#   geom_line(mapping=aes(y=birth_rate), data=dsOld, color="green")

###################
# Write to disk
###################
county_month_birth_rate_2014_version <- ds_county_month_2014
county_month_birth_rate_2005_version <- ds_county_month_2005

readr::write_csv(county_month_birth_rate_2014_version, outputPathBirthCountCountyMonthCsv2014)
readr::write_csv(county_month_birth_rate_2005_version, outputPathBirthCountCountyMonthCsv2005)
save(county_month_birth_rate_2014_version, file=outputPathBirthCountCountyMonthRda2014, compress="xz")
save(county_month_birth_rate_2005_version, file=outputPathBirthCountCountyMonthRda2005, compress="xz")
