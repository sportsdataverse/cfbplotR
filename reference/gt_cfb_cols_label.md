# Render CFB logos in gt column labels

Replace gt column label text with CFB team logos. Each bare column name
passed via `...` is treated as a team name; the label for that column is
replaced with the corresponding logo image.

## Usage

``` r
gt_cfb_cols_label(gt_object, ..., height = 30)
```

## Arguments

- gt_object:

  a `gt_tbl`.

- ...:

  `<tidy-select>`-style bare column names whose labels are team names to
  replace with logos.

- height:

  image height in px.

## Value

A `gt_tbl`.

## Examples

``` r
# \donttest{
library(gt)
library(cfbplotR)

df <- data.frame(Georgia = 1:3, Alabama = 4:6)
gt(df) |> gt_cfb_cols_label(Georgia, Alabama)


  
```
