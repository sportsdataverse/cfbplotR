# Package index

## Logos, Wordmarks & Headshots

ggplot2 geoms and ggproto objects for rendering CFB team logos,
wordmarks, and player headshots on plots.

- [`GeomCFBheads`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_logos.md)
  [`geom_cfb_logos()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_logos.md)
  [`GeomCFBlogo`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_logos.md)
  : ggplot2 Layer for Visualizing CFB Team Logos
- [`geom_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_headshots.md)
  : ggplot2 Layer for Visualizing CFB Player Headshots
- [`geom_cfb_wordmarks()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_wordmarks.md)
  : ggplot2 Layer for Visualizing CFB Team Wordmarks

## Theme Elements

Image-based ggplot2 theme elements for axis text — replace tick labels
with CFB logos, wordmarks, or headshots.

- [`element_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  [`element_cfb_wordmark()`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  [`element_cfb_headshot()`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  [`element_grob(`*`<element_cfb_logo>`*`)`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  [`element_grob(`*`<element_cfb_wordmark>`*`)`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  [`element_grob(`*`<element_cfb_headshot>`*`)`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  : Theme Elements for Image Grobs

## Scales & Axes

CFB color/fill scales and logo/headshot axis annotation scales.

- [`scale_color_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_color_cfb.md)
  [`scale_colour_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_color_cfb.md)
  [`scale_fill_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_color_cfb.md)
  : Scale for college football team colors
- [`scale_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  [`scale_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  [`scale_x_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  [`scale_y_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  : Axis Scales for CFB Team Logos
- [`theme_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md)
  [`theme_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md)
  : Theme for CFB Team Logos

## gt Table Helpers

Embed CFB team logos, wordmarks, and player headshots inside gt table
cells or column labels.

- [`gt_fmt_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)
  [`gt_fmt_cfb_wordmark()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)
  [`gt_fmt_cfb_headshot()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)
  :

  Add logos into rows of a `gt` table

- [`gt_cfb_cols_label()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb_cols_label.md)
  : Render CFB logos in gt column labels

- [`gt_merge_stack_team_color()`](https://cfbplotR.sportsdataverse.org/reference/gt_stack_team.md)
  :

  Merge and stack text from two columns in `gt` and color one with
  school colors

## Team Utilities

Validate, clean, and factor-order CFB team names and abbreviations; look
up athlete IDs; control the image cache.

- [`valid_team_names()`](https://cfbplotR.sportsdataverse.org/reference/valid_team_names.md)
  : Output Valid CFB Team Names and Abbreviations
- [`clean_school_names()`](https://cfbplotR.sportsdataverse.org/reference/clean_school_names.md)
  : Standardize NCAA School Names
- [`clean_team_abbrs()`](https://cfbplotR.sportsdataverse.org/reference/clean_team_abbrs.md)
  : Clean CFB team abbreviations
- [`cfb_team_factor()`](https://cfbplotR.sportsdataverse.org/reference/cfb_team_factor.md)
  : Order CFB teams as a factor
- [`add_athlete_id_col()`](https://cfbplotR.sportsdataverse.org/reference/add_athlete_id_col.md)
  : Add Athlete ID's to data frame
- [`.cfbplotR_clear_cache()`](https://cfbplotR.sportsdataverse.org/reference/dot-cfbplotR_clear_cache.md)
  : Clear the cfbplotR (ggpath) image cache

## Premade Plots

High-level functions that build complete CFB-branded plots.

- [`cfb_team_tiers()`](https://cfbplotR.sportsdataverse.org/reference/cfb_team_tiers.md)
  : Create CFB Team Tiers

## Plot Titles & Preview

Add images to ggplot2 titles and preview plots at exact output
dimensions.

- [`ggtitle_image()`](https://cfbplotR.sportsdataverse.org/reference/ggtitle_image.md)
  [`theme_title_image()`](https://cfbplotR.sportsdataverse.org/reference/ggtitle_image.md)
  : Functions for adding an image to the title of a ggplot
- [`ggpreview()`](https://cfbplotR.sportsdataverse.org/reference/ggpreview.md)
  : Preview ggplot in Specified Dimensions

## Package Data

Built-in reference datasets shipped with cfbplotR.

- [`logo_ref`](https://cfbplotR.sportsdataverse.org/reference/data.md)
  [`team_name_mapping`](https://cfbplotR.sportsdataverse.org/reference/data.md)
  :

  **Data in the package for reference**

## Re-exported from ggpath

Generic image geoms and theme elements provided by the ggpath backend.

- [`reexports`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`geom_from_path`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`GeomFromPath`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`element_path`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`element_raster`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`geom_mean_lines`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`geom_median_lines`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  [`GeomRefLines`](https://cfbplotR.sportsdataverse.org/reference/reexports.md)
  : Objects exported from other packages
