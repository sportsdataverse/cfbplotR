# Preview ggplot in Specified Dimensions

This function previews a ggplot in its actual dimensions in order to see
how it will look when saved. It is also significantly faster than the
default preview in RStudio for ggplots created using cfbplotR. This is
copied directly from `nflplotR`

## Usage

``` r
ggpreview(
  plot = ggplot2::last_plot(),
  width = NA,
  height = NA,
  asp = NULL,
  dpi = 300,
  device = "png",
  units = c("in", "cm", "mm", "px"),
  scale = 1,
  limitsize = TRUE,
  bg = NULL,
  ...
)
```

## Arguments

- plot:

  Plot to save, defaults to last plot displayed.

- width, height, units:

  Plot size in `units` ("in", "cm", "mm", or "px"). If not supplied,
  uses the size of current graphics device.

- asp:

  The aspect ratio of the plot calculated as `width / height`. If this
  is a numeric value (and not `NULL`) the `height` of the plot will be
  recalculated to `height = width / asp`.

- dpi:

  Plot resolution. Also accepts a string input: "retina" (320), "print"
  (300), or "screen" (72). Applies only to raster output types.

- device:

  Device to use. Can either be a device function (e.g.
  [png](https://rdrr.io/r/grDevices/png.html)), or one of "eps", "ps",
  "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
  (windows only).

- scale:

  Multiplicative scaling factor.

- limitsize:

  When `TRUE` (the default),
  [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) will
  not save images larger than 50x50 inches, to prevent the common error
  of specifying dimensions in pixels.

- bg:

  Background colour. If `NULL`, uses the `plot.background` fill value
  from the plot theme.

- ...:

  Other arguments passed on to the graphics device function, as
  specified by `device`.

## Examples

``` r
library(cfbplotR)
library(ggplot2)

team <- valid_team_names()
# remove conference logos from this example
team <- team[1:20]

df <- data.frame(
  random_value = runif(length(team), 0, 1),
  teams = team
)

# use logos for x-axis
# note that the plot is assigned to the object "p"
p <- ggplot(df, aes(x = teams, y = random_value)) +
  geom_col(aes(color = teams, fill = teams), width = 0.5) +
  scale_color_cfb(alt_colors = valid_team_names()) +
  scale_fill_cfb(alpha = 0.7) +
  scale_x_cfb() +
  theme_minimal() +
  theme_x_cfb()

# preview p with defined width and aspect ratio (only available in RStudio)
if (rstudioapi::isAvailable()){
  ggpreview(p, width = 5, asp = 16/9)
}
```
