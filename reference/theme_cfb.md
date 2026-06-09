# Theme for CFB Team Logos

These functions are convenience wrappers around a theme call that
activates markdown in x-axis and y-axis labels using
[`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html).

## Usage

``` r
theme_x_cfb()

theme_y_cfb()
```

## Details

These functions are a wrapper around the function calls
`ggplot2::theme(axis.text.x = ggtext::element_markdown())` as well as
`ggplot2::theme(axis.text.y = ggtext::element_markdown())`. They are
made to be used in conjunction with
[`scale_x_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
and
[`scale_y_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
respectively.

## See also

`theme_x_cfb()`, `theme_y_cfb()`

## Examples

``` r
library(cfbplotR)
library(ggplot2)

team_abbr <- valid_team_names()
# remove conference logos from this example
team_abbr <- team_abbr[1:16]

df <- data.frame(
  random_value = runif(length(team_abbr), 0, 1),
  teams = team_abbr
)

if (utils::packageVersion("gridtext") > "0.1.4"){
  ggplot(df, aes(x = teams, y = random_value)) +
    geom_col(aes(color = teams, fill = teams), width = 0.5) +
    scale_color_cfb(alt_colors = team_abbr) +
    scale_fill_cfb(alpha = 0.4) +
    scale_x_cfb() +
    theme_minimal() +
    # theme_*_cfb requires gridtext version > 0.1.4
    theme_x_cfb()
}
```
