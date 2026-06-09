# Changelog

## cfbplotR (development version)

- Added the
  [`geom_cfb_logos()`](https://cfbplotr.sportsdataverse.org/reference/geom_cfb_logos.md)
  geom.
- Added the
  [`geom_mean_lines()`](https://cfbplotr.sportsdataverse.org/reference/geom_lines.md)
  and
  [`geom_median_lines()`](https://cfbplotr.sportsdataverse.org/reference/geom_lines.md)
  geoms.
- Added the
  [`scale_color_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_color_cfb.md)
  and
  [`scale_fill_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_color_cfb.md)
  functions.
- Added FCS team logos. (v0.0.0.9002)
- Added the `gt_fmt_cfb()` function. (this function is now
  [`gt_fmt_cfb_logo()`](https://cfbplotr.sportsdataverse.org/reference/gt_cfb.md))
  (v0.0.0.9003)
- Added the axis scales
  [`scale_x_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
  and
  [`scale_y_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
  in combination with the theme update functions
  [`theme_x_cfb()`](https://cfbplotr.sportsdataverse.org/reference/theme_cfb.md)
  and
  [`theme_y_cfb()`](https://cfbplotr.sportsdataverse.org/reference/theme_cfb.md).
  (v0.0.0.9004)
- Fixed bug where `gt_fmt_cfb()` wouldn’t work with Texas A&M (this
  function is now
  [`gt_fmt_cfb_logo()`](https://cfbplotr.sportsdataverse.org/reference/gt_cfb.md))
  (v0.0.0.9005)
- Added NCAA logo as default for inputs not in
  [`valid_team_names()`](https://cfbplotr.sportsdataverse.org/reference/valid_team_names.md)(v0.0.0.9006)
- Fixed defaults for
  [`scale_y_cfb()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
  to fix error.
- Added FCS colors (v0.0.0.9007)
- Added DII & DIII Logos
- Added input to
  [`valid_team_names()`](https://cfbplotr.sportsdataverse.org/reference/valid_team_names.md)
  to allow filtering by division (FBS, FCS, DII, DIII, or Conference
  logos) (v0.0.0.9008)
- Added the function
  [`cfb_team_tiers()`](https://cfbplotr.sportsdataverse.org/reference/cfb_team_tiers.md)
  that build an NFL team tiers ggplot, thanks to [Timo
  Riske](https://twitter.com/PFF_Moo) for the suggestion.
- Fixed a bug in
  [`geom_median_lines()`](https://cfbplotr.sportsdataverse.org/reference/geom_lines.md)
  and
  [`geom_mean_lines()`](https://cfbplotr.sportsdataverse.org/reference/geom_lines.md)
  that caused `alpha` to not work properly.
- Added the
  [`geom_cfb_headshots()`](https://cfbplotr.sportsdataverse.org/reference/geom_cfb_headshots.md)
  geom that plots headshots for valid ESPN player IDs. (v0.0.9009)
- Added cleaning function
  [`clean_school_names()`](https://cfbplotr.sportsdataverse.org/reference/clean_school_names.md)
  which attempts to correct common name issues to names in
  [`valid_team_names()`](https://cfbplotr.sportsdataverse.org/reference/valid_team_names.md).
  The plotting functions call this internally to automatically attempt
  to fix errors.
- Added experimental function
  [`add_athlete_id_col()`](https://cfbplotr.sportsdataverse.org/reference/add_athlete_id_col.md)
  that adds a column to a data frame called `athlete_id` from the
  rosters data from the cfbfastR-data repo based on a player name. This
  ID can be used with headshot plotting functions
- Added the axis scales
  [`scale_x_cfb_headshots()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
  and
  [`scale_y_cfb_headshots()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md).
- Added the
  [`gt_fmt_cfb_headshot()`](https://cfbplotr.sportsdataverse.org/reference/gt_cfb.md)
  function. (v0.0.0.9010)
- Added colleges from `hoopR` package (v0.0.0.9011)
- Brought package up to date with nflplotR v1.0.0
- Added the
  [`geom_from_path()`](https://cfbplotr.sportsdataverse.org/reference/geom_from_path.md)
  geom that plots images from urls, local paths and more.
- Added the ggplot2 theme-elements
  [`element_cfb_logo()`](https://cfbplotr.sportsdataverse.org/reference/element.md),
  [`element_cfb_headshot()`](https://cfbplotr.sportsdataverse.org/reference/element.md),
  and
  [`element_path()`](https://cfbplotr.sportsdataverse.org/reference/element.md)
  which translate college team names or player IDs into team logos and
  player headshots. These elements feature a major speed improvement
  over the axis scales
  [`scale_x_cfb_headshots()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
  and
  [`scale_y_cfb_headshots()`](https://cfbplotr.sportsdataverse.org/reference/scale_axes_cfb.md)
  and make the package less dependent on an underlying package.
- added `P5` and `G5` catagories to
  [`valid_team_names()`](https://cfbplotr.sportsdataverse.org/reference/valid_team_names.md)
- added the
  [`gt_merge_stack_team_color()`](https://cfbplotr.sportsdataverse.org/reference/gt_stack_team.md)
  function (v0.0.0.9012)
- added the
  [`ggtitle_image()`](https://cfbplotr.sportsdataverse.org/reference/ggtitle_image.md)
  and
  [`theme_title_image()`](https://cfbplotr.sportsdataverse.org/reference/ggtitle_image.md)
  functions (v0.0.0.9013)
- added \>350 wordmarks for all FBS teams and many other schools and
  conferences from sportslogos.net
- added
  [`geom_cfb_wordmarks()`](https://cfbplotr.sportsdataverse.org/reference/geom_cfb_wordmarks.md)
  geom,
  [`element_cfb_wordmark()`](https://cfbplotr.sportsdataverse.org/reference/element.md)
  element, and
  [`gt_fmt_cfb_wordmark()`](https://cfbplotr.sportsdataverse.org/reference/gt_cfb.md)
  function to implement new wordmarks. (v0.0.0.9014)
- added PFF team names (thanks to Tej Seth) to
  [`clean_school_names()`](https://cfbplotr.sportsdataverse.org/reference/clean_school_names.md)
  which is run internally for plotting (v0.0.0.9015)
