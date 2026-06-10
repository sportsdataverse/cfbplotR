# Axis Scales for CFB Team Logos

These functions map CFB team names to their team logos and make them
available as axis labels

## Usage

``` r
scale_x_cfb(
  ...,
  expand = ggplot2::waiver(),
  guide = ggplot2::waiver(),
  position = "bottom",
  size = 12
)

scale_y_cfb(
  ...,
  expand = ggplot2::waiver(),
  guide = ggplot2::waiver(),
  position = "left",
  size = 12
)

scale_x_cfb_headshots(
  ...,
  expand = ggplot2::waiver(),
  guide = ggplot2::waiver(),
  position = "bottom",
  size = 20
)

scale_y_cfb_headshots(
  ...,
  expand = ggplot2::waiver(),
  guide = ggplot2::waiver(),
  position = "left",
  size = 30
)
```

## Arguments

- ...:

  Arguments passed on to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (the scale limits)

      - A character vector of breaks

      - A function that takes the limits as input and returns breaks as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `na.value`

  :   If `na.translate = TRUE`, what aesthetic value should the missing
      values be displayed as? Does not apply to position scales where
      `NA` is always placed at the far right.

  `aesthetics`

  :   The names of the aesthetics that this scale works with.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

- size:

  The logo size in pixels. It is applied as height for an x-scale and as
  width for an y-scale.

## Details

The scale translates the CFB team names into raw image html and places
the html as axis labels. Because of the way ggplots are constructed, it
is necessary to adjust the
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
after calling this scale. This can be done by calling
[`theme_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md)
or
[`theme_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md)
or alternatively by manually changing the relevant `axis.text` to
[`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html).

## See also

[`theme_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md),
[`theme_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md)

## Examples

``` r
library(ggplot2)


#############################################################################
# Headshot Example
#############################################################################


dfh <- data.frame(
  random_value = runif(9, 0, 1),
  player_id = c("4361182",
                  "4426385",
                  "4567048",
                  "4372519",
                  "4429013",
                  "4240069",
                  "4360932",
                  "4362874",
                  "4429299")
)

  # use headshots for y-axis
  ggplot(dfh, aes(y = player_id, x = random_value)) +
    geom_col(width = 0.5) +
    scale_y_cfb_headshots() +
    theme_minimal() +
    theme_y_cfb()

```
