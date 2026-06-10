# **cfbplotR**

[**`cfbplotR`**](https://cfbplotR.sportsdataverse.org/) is an R package
for plotting College Football (CFB) team logos, player headshots,
wordmarks, and conference logos in
[**`ggplot2`**](https://ggplot2.tidyverse.org/) graphics and
[**`gt`**](https://gt.rstudio.com/) tables. It is built on the
[**`ggpath`**](https://mrcaseb.github.io/ggpath/) package and follows
the conventions established by
[nflplotR](https://nflplotr.nflverse.com/) — porting that proven
approach to college football.

Part of the [SportsDataverse](https://sportsdataverse.org/) family of R
packages for sports analytics.

## **Installation**

You can install the development version of
[**`cfbplotR`**](https://github.com/sportsdataverse/cfbplotR) from
[GitHub](https://github.com/sportsdataverse/cfbplotR) with:

``` r

# using the pak package (recommended):
if (!requireNamespace('pak', quietly = TRUE)){
  install.packages('pak')
}
pak::pak("sportsdataverse/cfbplotR")
```

``` r

# or using the devtools package:
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github(repo = "sportsdataverse/cfbplotR")
```

``` r
# or clone and install locally
git clone https://github.com/sportsdataverse/cfbplotR
cd cfbplotR
Rscript -e "pak::local_install()"  # or: Rscript -e "devtools::install()"
```

## **Usage**

[You can follow the package tutorial for several detailed
examples.](https://cfbplotR.sportsdataverse.org/articles/tutorial.html)

The core function
[`geom_cfb_logos()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_logos.md)
adds CFB team logos to a ggplot. Axis helpers
([`scale_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md),
[`scale_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md))
replace axis labels with team logos via
[`element_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/element.md).
The
[`gt_fmt_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)
helper adds logos inside `gt` table cells.

``` r

library(cfbplotR)
library(ggplot2)

# grab the first 32 valid team abbreviations
team <- valid_team_names()[1:32]
df <- data.frame(
  a     = rep(1:8, 4),
  b     = sort(rep(1:4, 8), decreasing = TRUE),
  teams = team
)

ggplot(df, aes(x = a, y = b)) +
  geom_cfb_logos(aes(team = teams), width = 0.075) +
  geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
  theme_void()
```

``` r

library(cfbplotR)
library(ggplot2)

# logos on the x-axis via scale_x_cfb() + element_cfb_logo()
df2 <- data.frame(
  team  = c("Alabama", "Georgia", "Ohio State", "Michigan"),
  score = c(42, 38, 35, 30)
)

ggplot(df2, aes(x = team, y = score)) +
  geom_col(aes(fill = team), show.legend = FALSE) +
  scale_x_cfb(labels = "logo") +
  theme_minimal() +
  theme(
    axis.text.x = element_cfb_logo(size = 1)
  )
```

``` r

library(cfbplotR)
library(gt)

# logos inside a gt table
data.frame(
  team  = c("Alabama", "Georgia", "Ohio State"),
  wins  = c(13, 12, 11)
) |>
  gt() |>
  gt_fmt_cfb_logo(columns = "team")
```

## **Documentation**

For more information on the package and function reference, please see
the [**`cfbplotR`** documentation
website](https://cfbplotR.sportsdataverse.org).

## **The SportsDataverse**

`cfbplotR` is part of the
[**SportsDataverse**](https://sportsdataverse.org/), a family of
open-source R, Python, and Node.js packages for sports data.

| Package | Sport / Scope |
|----|----|
| [**cfbfastR**](https://cfbfastR.sportsdataverse.org/) | College football |
| [**hoopR**](https://hoopR.sportsdataverse.org/) | Men’s basketball (NBA & NCAA) |
| [**wehoop**](https://wehoop.sportsdataverse.org/) | Women’s basketball (WNBA & NCAA) |
| [**fastRhockey**](https://fastRhockey.sportsdataverse.org/) | Hockey (NHL & PWHL) |
| [**baseballr**](https://billpetti.github.io/baseballr/) | Baseball (MLB, MiLB, NCAA) |
| [**oddsapiR**](https://oddsapiR.sportsdataverse.org/) | Sports betting odds (The Odds API) |
| [**sportsdataverse-R**](https://r.sportsdataverse.org/) | Umbrella R metapackage |
| [**sportsdataverse-py**](https://py.sportsdataverse.org/) · [**sportsdataverse.js**](https://js.sportsdataverse.org/) | Python & Node.js |

See the full ecosystem at
[sportsdataverse.org](https://sportsdataverse.org/).

## Follow [cfbplotR](https://twitter.com/cfbfastR) and the [SportsDataverse](https://twitter.com/SportsDataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/JaredDLee?color=blue&label=%40JaredDLee&logo=twitter&style=for-the-badge)](https://twitter.com/JaredDLee)

[![Twitter
Follow](https://img.shields.io/twitter/follow/SportsDataverse?color=blue&label=%40SportsDataverse&logo=twitter&style=for-the-badge)](https://twitter.com/SportsDataverse)

[![GitHub
stars](https://img.shields.io/github/stars/sportsdataverse/cfbplotR.svg?color=eee&logo=github&style=for-the-badge&label=Star%20cfbplotR&maxAge=2592000)](https://github.com/sportsdataverse/cfbplotR/stargazers/)

## **Our Authors**

- [Jared Lee](https://twitter.com/JaredDLee)
  [![@JaredDLee](https://img.shields.io/twitter/follow/JaredDLee?color=blue&label=%40JaredDLee&logo=twitter&style=for-the-badge)](https://twitter.com/JaredDLee)
  [![@Kazink36](https://img.shields.io/github/followers/Kazink36?color=eee&logo=Github&style=for-the-badge)](https://github.com/Kazink36)

- [Saiem Gilani](https://twitter.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge)](https://twitter.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge)](https://github.com/saiemgilani)

- [Sebastian Carl](https://twitter.com/mrcaseb)
  [![@mrcaseb](https://img.shields.io/twitter/follow/mrcaseb?color=blue&label=%40mrcaseb&logo=twitter&style=for-the-badge)](https://twitter.com/mrcaseb)
  [![@mrcaseb](https://img.shields.io/github/followers/mrcaseb?color=eee&logo=Github&style=for-the-badge)](https://github.com/mrcaseb)

## **Citations**

To cite the [**`cfbplotR`**](https://cfbplotR.sportsdataverse.org/) R
package in publications, use:

BibTex Citation

``` bibtex
@misc{lee_carl_gilani_cfbplotR,
  author = {Lee, Jared and Gilani, Saiem and Carl, Sebastian},
  title = {cfbplotR: The SportsDataverse's R Package for College Football Plotting.},
  url = {https://cfbplotR.sportsdataverse.org},
  year = {2021}
}
```
