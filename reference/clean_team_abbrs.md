# Clean CFB team abbreviations

Alias of
[`clean_school_names()`](https://cfbplotR.sportsdataverse.org/reference/clean_school_names.md)
for naming parity with the nflplotR/nbaplotR family.

## Usage

``` r
clean_team_abbrs(school, keep_non_matches = TRUE)
```

## Arguments

- school:

  a character vector of names.

- keep_non_matches:

  If `TRUE` (the default) an element of `school` that can't be matched
  to any of the internal mapping vectors will be kept as is. Otherwise
  it will be replaced with `NA`.

## Value

A character vector of cleaned team names.
