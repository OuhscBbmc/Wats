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
requireNamespace("dplyr"        )
requireNamespace("testit"       )
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
# requireNamespace("OuhscMunge"   ) # remotes::install_github("OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
input_dir_census_199x           <- "datasets/raw/census-199x"
input_path_census200x           <- "datasets/raw/census-200x/CO-EST00INT-AGESEX-5YR.csv"
input_path_fips                 <- "datasets/raw/county-fips.csv"
ouput_path_census_county_year   <- "datasets/derived/census-county-year.csv"
ouput_path_census_county_month  <- "datasets/derived/census-county-month.csv"

age_group_labels <-
  c(
    "0"   = "00",
    "1"   = "01-04",
    "2"   = "05-09",
    "3"   = "10-14",
    "4"   = "15-19",
    "5"   = "20-24",
    "6"   = "25-29",
    "7"   = "30-34",
    "8"   = "35-39",
    "9"   = "40-44",
    "10"  = "45-49",
    "11"  = "50-54",
    "12"  = "55-59",
    "13"  = "60-64",
    "14"  = "65-69",
    "15"  = "70-74",
    "16"  = "75-79",
    "17"  = "80-84",
    "18"  = "85+"
  )
ds_lu_race_gender <-  # Just for 199x.
  tibble::tribble(
    ~race_gender_id, ~female, ~race,
    1L             ,  FALSE , "White male",
    2L             ,  TRUE  , "White female",
    3L             ,  FALSE , "Black male",
    4L             ,  TRUE  , "Black female",
    5L             ,  FALSE , "American Indian or Alaska Native male",
    6L             ,  TRUE  , "American Indian or Alaska Native female",
    7L             ,  FALSE , "Asian or Pacific Islander male",
    8L             ,  TRUE  , "Asian or Pacific Islander female"
  )

# #Identify and isolate the levels need to calculate GFR (ie, females between 15 & 44)
eligible_age_labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")

cols_positions_199x <-
  readr::fwf_cols(
    year                = c( 1,  2),
    fips                = c( 5,  9),
    age_group_id        = c(11, 13),
    race_gender_id      = c(14, 15),
    latino              = c(16, 16),
    population_count    = c(18, 24)
  )

cols_types_199x <-
  readr::cols_only(
    year                = readr::col_integer(),
    fips                = readr::col_character(),
    age_group_id        = readr::col_integer(),
    race_gender_id      = readr::col_integer(),
    latino              = readr::col_integer(),
    population_count    = readr::col_integer()
  )

cols_types_200x <- readr::cols_only(
  # `SUMLEV`              = readr::col_character(),
  # `STATE`               = readr::col_integer(),
  `COUNTY`              = readr::col_character(),
  # `STNAME`              = readr::col_character(),
  # `CTYNAME`             = readr::col_character(),
  `SEX`                 = readr::col_integer(),
  `AGEGRP`              = readr::col_integer(),
  # `ESTIMATESBASE2000`   = readr::col_integer(),
  `POPESTIMATE2000`     = readr::col_integer(),
  # `POPESTIMATE2001`     = readr::col_integer(),
  # `POPESTIMATE2002`     = readr::col_integer(),
  # `POPESTIMATE2003`     = readr::col_integer(),
  # `POPESTIMATE2004`     = readr::col_integer(),
  # `POPESTIMATE2005`     = readr::col_integer(),
  # `POPESTIMATE2006`     = readr::col_integer(),
  # `POPESTIMATE2007`     = readr::col_integer(),
  # `POPESTIMATE2008`     = readr::col_integer(),
  # `POPESTIMATE2009`     = readr::col_integer(),
  # `CENSUS2010POP`       = readr::col_integer(),
  # `POPESTIMATE2010`     = readr::col_integer()
)

# OuhscMunge::readr_spec_aligned(input_path_fips)
col_types_fips <- readr::cols_only(
  `county_name`   = readr::col_character(),
  `fips`          = readr::col_character(),
  `wats_urban`    = readr::col_logical()
)

# ---- load-data ---------------------------------------------------------------
# For 199x, create a list of data.frames; each one is a year's data.  Then bind to create a single dataset
ds_census_199x <-
  input_dir_census_199x |>
  fs::dir_ls(regexp = "/STCH-icen199\\d\\.txt$") |>
  readr::read_fwf(
    col_positions = cols_positions_199x,
    col_types     = cols_types_199x
    # id            = "file_path"
  )
# table(fs::path_file(ds_census_199x$file_path), ds_census_199x$age_group)

#For 200x, the schema is different, and everything comes in one data file.  Each year is a distinct column.
# OuhscMunge::readr_spec_aligned(input_path_census200x)
ds_census_200x <-
  input_path_census200x |>
  readr::read_csv(col_types = cols_types_200x)

#In the FIPS dataset, there is one record for each county.
ds_fips <-
  input_path_fips |>
  readr::read_csv(col_types = col_types_fips)

rm(cols_positions_199x)
rm(cols_types_199x, cols_types_200x, col_types_fips)
rm(input_dir_census_199x, input_path_census200x, input_path_fips)

# ---- tweak-data --------------------------------------------------------------
#For 199x: See the codebook at datasets/census-intercensal/STCH-Intercensal_layout.txt.  The State FIPS is missing for some reason
#For 200x: See the codebook at ./Datasets/CensusIntercensal/CO-EST00INT-AGESEX-5YR.pdf.
# colnames(dsCensus199x) <- c("year", "fips", "AgeGroup", "RaceGender", "Latino", "PopulationCount")

# ---- groom-199x --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_census_199x) # Help write `dplyr::select()` call.
ds_county_year_199x <-
  ds_census_199x |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    year,
    fips,
    age_group_id,
    race_gender_id,
    # latino,
    population_count,
  ) |>
  dplyr::left_join(ds_lu_race_gender, "race_gender_id") |>
  dplyr::left_join(ds_fips, by = "fips") |> # Merge the counties by the FIPS to get their county name and urban/rural status.
  dplyr::mutate(
    year          = year + 1900L,
    age_group     = dplyr::recode_factor(age_group_id, !!!age_group_labels),
    # latino        = dplyr::recode(latino, "1" = TRUE, "2" = FALSE),
    gfr_eligible  = (female & age_group %in% eligible_age_labels),
  ) |>
  dplyr::filter(gfr_eligible & wats_urban) |>
  dplyr::group_by(fips, year, county_name) |>
  dplyr::summarize(
    fecund_population_count = sum(population_count)
  ) |>
  dplyr::ungroup()

#Assert the values aren't too funky.
testit::assert("All years in dsCensus199x should be in the 1990s", all(1990L <= ds_county_year_199x$year & ds_county_year_199x$year <=1999L))
testit::assert("All County FIPS should start with 40 (ie, be in Oklahoma).", all(grepl(pattern="^40\\d{3}$", x=ds_county_year_199x$fips, perl=TRUE)))
# testit::assert("The mean of the Latino values should be 0.5.", mean(ds_census_199x$latino)==0.5)
# testit::assert("The proportion of GFR eligible rows should be correct.", mean(ds_census_199x$gfr_eligible) == (6/19 * 1/2))
rm(ds_census_199x)

# ---- groom-2000 --------------------------------------------------------------
# Groom the Census data from the 2000s & Keep only the needed rows

# OuhscMunge::column_rename_headstart(ds_census_200x) # Help write `dplyr::select()` call.
ds_county_year_2000 <-
  ds_census_200x |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    county                         = `COUNTY`,
    sex                            = `SEX`,
    age_group_id                   = `AGEGRP`,
    population_count               = `POPESTIMATE2000`,
  ) |>
  dplyr::mutate(
    year          = 2000L,
    fips          = sprintf("40%s", county),
    # sex           = dplyr::recode_factor(sex, "0" = "total", "1" = "male", "2" = "female"),
    female        = dplyr::recode(as.character(sex), "0" = NA, "1" = FALSE, "2" = TRUE),
    # female        = dplyr::case_match(sex, 0L ~ NA, 1L ~ FALSE, 2L ~ TRUE),

    age_group     = dplyr::recode_factor(age_group_id, !!!age_group_labels),
    gfr_eligible  = (female & age_group %in% eligible_age_labels),
  ) |>
  dplyr::left_join(ds_fips, by = "fips") |> # Merge the counties by the FIPS to get their county name and urban/rural status.
  tidyr::drop_na(female) |>
  dplyr::filter(gfr_eligible & wats_urban)  |>
  dplyr::group_by(fips, year, county_name) |>
  dplyr::summarize(
    fecund_population_count  = sum(population_count),
  ) |>
  dplyr::ungroup()

# testit::assert("The proportion of GFR eligible rows should be correct.", mean(ds_census_200x$gfr_eligible, na.rm = T) == (6/19 * 1/2))
rm(ds_census_200x)
rm(ds_fips)
rm(ds_lu_race_gender)
rm(age_group_labels)
rm(eligible_age_labels)

# ---- stack -------------------------------------------------------------------
ds_county_year <-
  ds_county_year_199x |>
  dplyr::select(!!!colnames(ds_county_year_2000)) |>
  dplyr::union_all(ds_county_year_2000) |>
  dplyr::mutate(
    county_name   = tolower(county_name),
    # county_name   = paste0('"', county_name, '"'),
  ) |>
  dplyr::arrange(fips, year)

rm(ds_county_year_199x, ds_county_year_2000)

# ---- interpolate-within-year -------------------------------------------------
create_next_year_pop_count <- function( d ) {
  ceiling_year <- max(d$year)
  next_year <- d$year + 1L
  next_pop_count <- d$fecund_population_count[match(next_year, d$year)]
  ds_out <- data.frame(
    year = d$year,
    year_next = next_year,
    fecund_population_count = d$fecund_population_count,
    fecund_population_count_next = next_pop_count
  )
  ds_out[ds_out$year < ceiling_year, ]
}

ds_next <-
  ds_county_year |>
  dplyr::group_by(fips, county_name) |>
  dplyr::do(create_next_year_pop_count(.)) |>
  dplyr::ungroup()

interpolate_months <- function( d ) {
  months_per_year <- 12L
  months <- seq_len(months_per_year)
  pop_interpolated <- approx(x=c(d$year, d$year_next), y=c(d$fecund_population_count, d$fecund_population_count_next), n=months_per_year+1)
  data.frame(
    month = months,
    fecund_population = as.integer(pop_interpolated$y[months])
  )
}

ds_county_month <-
  ds_next |>
  dplyr::group_by(fips, county_name, year) |>
  dplyr::do(interpolate_months(.)) |>
  dplyr::ungroup()

# ---- save-to-disk ------------------------------------------------------------
readr::write_csv(ds_county_year , ouput_path_census_county_year )
readr::write_csv(ds_county_month, ouput_path_census_county_month)
