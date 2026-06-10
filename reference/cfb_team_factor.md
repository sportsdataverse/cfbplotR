# Order CFB teams as a factor

Creates a factor of cleaned, valid CFB team names. Levels are restricted
to teams present in
[`valid_team_names()`](https://cfbplotR.sportsdataverse.org/reference/valid_team_names.md)
so that downstream ggplot2 scales drop invalid entries gracefully.

## Usage

``` r
cfb_team_factor(teams)
```

## Arguments

- teams:

  character vector of team names.

## Value

An ordered `factor` of the cleaned, valid team names.

## Examples

``` r
cfb_team_factor(c("Georgia", "Alabama", "ohio state"))
#> [1] Georgia    Alabama    Ohio State
#> Levels: Alabama Georgia Ohio State
```
