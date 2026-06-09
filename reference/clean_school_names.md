# Standardize NCAA School Names

This function standardizes college names to cfbplotR defaults. This
helps for joins and plotting.

## Usage

``` r
clean_school_names(school, keep_non_matches = TRUE)
```

## Arguments

- school:

  a character vector of names

- keep_non_matches:

  If `TRUE` (the default) an element of `school` that can't be matched
  to any of the internal mapping vectors will be kept as is. Otherwise
  it will be replaced with `NA`.

## Value

A character vector with the length of `school` and cleaned team
abbreviations if they are included in `team_name_mapping`. Non matches
may be replaced with `NA` (depending on the value of
`keep_non_matches`).

## Examples

``` r

x <- c("utah", "San Jose State", "Hawaii", "UTSA", "SLC", "USC")
# use current location and keep non matches
clean_school_names(x)
#> Warning: Abbreviations not found in `team_name_mapping`: SLC
#> [1] "Utah"           "San José State" "Hawai'i"        "UTSA"          
#> [5] "SLC"            "USC"           

# replace non matches
clean_school_names(x, keep_non_matches = FALSE)
#> Warning: Abbreviations not found in `team_name_mapping`: SLC
#> [1] "Utah"           "San José State" "Hawai'i"        "UTSA"          
#> [5] NA               "USC"           
```
