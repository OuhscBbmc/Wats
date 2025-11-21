# Calculates variables necessary for WATS Plots

Calculates variables necessary for WATS Plots. This the first of two
functions that needs to be called to produce WATS Plots.
[`annotate_data()`](https://ouhscbbmc.github.io/Wats/reference/annotate_data.md)
is the second.

## Usage

``` r
augment_year_data_with_month_resolution(ds_linear, date_name)
augment_year_data_with_second_resolution(ds_linear, date_name)
```

## Arguments

- ds_linear:

  The [data.frame](https://rdrr.io/r/base/data.frame.html) to containing
  the detailed data.

- date_name:

  The variable name in `ds_linear` containing the date or datetime
  value.

## Value

Returns a
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
with additional variables: `cycle_tally`, `proportion_through_cycle`,
`proportion_id`, and `terminal_point_in_cycle`.

## Examples

``` r
library(Wats)
ds_linear <-
  Wats::county_month_birth_rate_2005_version |>
  dplyr::filter(county_name == "oklahoma") |>
  augment_year_data_with_month_resolution(date_name = "date")

head(ds_linear)
#> # A tibble: 6 × 18
#>   fips  county_name  year month fecund_population birth_count date      
#>   <chr> <chr>       <int> <int>             <int>       <int> <date>    
#> 1 40109 oklahoma     1990     1            143192         853 1990-01-15
#> 2 40109 oklahoma     1990     2            143278         758 1990-02-15
#> 3 40109 oklahoma     1990     3            143365         886 1990-03-15
#> 4 40109 oklahoma     1990     4            143452         871 1990-04-15
#> 5 40109 oklahoma     1990     5            143538         822 1990-05-15
#> 6 40109 oklahoma     1990     6            143625         834 1990-06-15
#> # ℹ 11 more variables: days_in_month <int>, days_in_year <int>, stage_id <int>,
#> #   birth_rate_monthly <dbl>, birth_rate <dbl>, cycle_tally <dbl>,
#> #   proportion_through_cycle <dbl>, proportion_id <dbl>,
#> #   starting_point_in_cycle <lgl>, terminal_point_in_cycle <lgl>,
#> #   stage_progress <dbl>
```
