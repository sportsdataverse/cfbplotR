# ggplot2 Layer for Visualizing Images from URLs or Local Paths

This geom is used to plot images instead of points in a ggplot. It
requires x, y aesthetics as well as a path. This is copied directly from
`nflplotR`.

## Usage

``` r
geom_from_path(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer,
  either as a `ggproto` `Geom` subclass or as a string naming the stat
  stripped of the `stat_` prefix (e.g. `"count"` rather than
  `"stat_count"`)

- position:

  Position adjustment, either as a string naming the adjustment (e.g.
  `"jitter"` to use `position_jitter`), or the result of a call to a
  position adjustment function. Use the latter if you need to change the
  settings of the adjustment.

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  These are often aesthetics, used to set an aesthetic to a fixed value.
  See the below section "Aesthetics" for a full list of possible
  arguments.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A ggplot2 layer
([`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html))
that can be added to a plot created with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Aesthetics

`geom_from_path()` understands the following aesthetics (required
aesthetics are in bold):

- **x** - The x-coordinate.

- **y** - The y-coordinate.

- **path** - a file path, url, raster object or bitmap array. See
  [`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html)
  for further information.

- `alpha = NULL` - The alpha channel, i.e. transparency level, as a
  numerical value between 0 and 1.

- `colour = NULL` - The image will be colorized with this colour. Use
  the special character `"b/w"` to set it to black and white. For more
  information on valid colour names in ggplot2 see
  <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>

- `angle = 0` - The angle of the image as a numerical value between 0°
  and 360°.

- `hjust = 0.5` - The horizontal adjustment relative to the given x
  coordinate. Must be a numerical value between 0 and 1.

- `vjust = 0.5` - The vertical adjustment relative to the given y
  coordinate. Must be a numerical value between 0 and 1.

- `width = 1.0` - The desired width of the image in `npc` (Normalised
  Parent Coordinates). The default value is set to 1.0 which is *big*
  but it is necessary because all used values are computed relative to
  the default. A typical size is `width = 0.1` (see below examples).

- `height = 1.0` - The desired height of the image in `npc` (Normalised
  Parent Coordinates). The default value is set to 1.0 which is *big*
  but it is necessary because all used values are computed relative to
  the default. A typical size is `height = 0.1` (see below examples).

## Examples

``` r
# \donttest{
library(ggplot2)
library(cfbplotR)

# create x-y-coordinates of a pentagon and add sportsdataverse logo urls
df <- data.frame(
  a = c(sin(2 * pi * (0:4) / 5), 0),
  b = c(cos(2 * pi * (0:4) / 5), 0),
  url = c(
    "https://raw.githubusercontent.com/sportsdataverse/cfbfastR/main/man/figures/logo.png",
    "https://raw.githubusercontent.com/sportsdataverse/hoopR/main/man/figures/logo.png",
    "https://raw.githubusercontent.com/sportsdataverse/cfb4th/main/man/figures/logo.png",
    "https://raw.githubusercontent.com/sportsdataverse/wehoop/main/man/figures/logo.png",
    "https://raw.githubusercontent.com/sportsdataverse/cfbplotR/main/man/figures/logo.png",
    "https://raw.githubusercontent.com/sportsdataverse/sportsdataverse-R/main/logo.png"
  )
)

# plot images directly from url
ggplot(df, aes(x = a, y = b)) +
  geom_from_path(aes(path = url), width = 0.15) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1.3, 1.5)) +
  theme_void()


# plot images directly from url and apply transparency
ggplot(df, aes(x = a, y = b)) +
  geom_from_path(aes(path = url), width = 0.15, alpha = 0.5) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1.3, 1.5)) +
  theme_void()


# It is also possible and recommended to use the underlying Geom inside a
# ggplot2 annotation
ggplot() +
  annotate(
    GeomFromPath,
    x = 0,
    y = 0,
    path = "https://raw.githubusercontent.com/sportsdataverse/cfbplotR/main/man/figures/logo.png",
    width = 0.4
  ) +
  theme_minimal()

# }
```
