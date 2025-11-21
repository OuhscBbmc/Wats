# Finds midpoints and bands for the within and between cycles.

Finds midpoints and bands for the within and between cycles. This the
second of two functions that needs to be called to produce WATS Plots.
`AugmentZZZ` is the first.

## Usage

``` r
annotate_data(
  ds_linear,
  dv_name,
  center_function,
  spread_function,
  cycle_tally_name = "cycle_tally",
  stage_id_name = "stage_id",
  stage_progress_name = "stage_progress",
  proportion_through_cycle_name = "proportion_through_cycle",
  proportion_id_name = "proportion_id",
  terminal_point_in_cycle_name = "terminal_point_in_cycle"
)
```

## Arguments

- ds_linear:

  The [data.frame](https://rdrr.io/r/base/data.frame.html) to containing
  the detailed data.

- dv_name:

  The name of the dependent/criterion variable.

- center_function:

  A function to calculate the center of a subsample.

- spread_function:

  A function to calculate the bands of a subsample.

- cycle_tally_name:

  The variable name indicating how many cycles have been completed.

- stage_id_name:

  The variable name indicating the stage. In a typical interrupted time
  series, these values are "1" before the interruption and "2" after.

- stage_progress_name:

  The variable name indicating the stage in a decimal form. This is
  mostly for internal uses.

- proportion_through_cycle_name:

  The variable name indicating how far the point is through a cycle. For
  example, 0 degrees would be `0`, 180 degrees would be `0.5`, 359
  degrees would be `0.9972`, and 360 degrees would be `0`.

- proportion_id_name:

  The variable name indicating the ordinal position through a cycle.

- terminal_point_in_cycle_name:

  The variable name indicating the last point within a given cycle.

## Value

Returns a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with additional variables. TODO: say what the variables are.

## Examples

``` r
system.time({
library(Wats)
ds_linear <-
  Wats::county_month_birth_rate_2005_version |>
  dplyr::filter(county_name == "oklahoma") |>
  augment_year_data_with_month_resolution(date_name = "date")

h_spread <- \(scores) { quantile(x = scores, probs = c(.25, .75)) }

portfolio <- annotate_data(
  ds_linear       = ds_linear,
  dv_name         = "birth_rate",
  center_function = median,
  spread_function = h_spread
)
portfolio$ds_stage_cycle
portfolio$ds_linear
portfolio$ds_periodic
})
#>    user  system elapsed 
#>   0.242   0.012   0.254 
```
