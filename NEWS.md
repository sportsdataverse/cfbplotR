# cfbplotR (development version)

* Added the `geom_cfb_logos()` geom.
* Added the `geom_mean_lines()` and `geom_median_lines()` geoms.
* Added the `scale_color_cfb()` and `scale_fill_cfb()` functions. 
* Added FCS team logos. (v0.0.0.9002)
* Added the `gt_fmt_cfb()` function. (this function is now `gt_fmt_cfb_logo()`) (v0.0.0.9003)
* Added the axis scales `scale_x_cfb()` and `scale_y_cfb()` in combination with the theme update functions `theme_x_cfb()` and `theme_y_cfb()`. (v0.0.0.9004)
* Fixed bug where `gt_fmt_cfb()` wouldn't work with Texas A&M (this function is now `gt_fmt_cfb_logo()`) (v0.0.0.9005) 
* Added NCAA logo as default for inputs not in `valid_team_names()`(v0.0.0.9006)
* Fixed defaults for `scale_y_cfb()` to fix error.
* Added FCS colors (v0.0.0.9007)
* Added DII & DIII Logos
* Added input to `valid_team_names()` to allow filtering by division (FBS, FCS, DII, DIII, or Conference logos) (v0.0.0.9008)
* Added the function `cfb_team_tiers()` that build an NFL team tiers ggplot, thanks to [Timo Riske](https://twitter.com/PFF_Moo) for the suggestion.
* Fixed a bug in `geom_median_lines()` and `geom_mean_lines()` that caused `alpha` to not work properly.
* Added the `geom_cfb_headshots()` geom that plots headshots for valid ESPN player IDs. (v0.0.9009)
* Added cleaning function `clean_school_names()` which attempts to correct common name issues to names in `valid_team_names()`. The plotting functions call this internally to automatically attempt to fix errors.
* Added experimental function `add_athlete_id_col()` that adds a column to a data frame called `athlete_id` from the rosters data from the cfbfastR-data repo based on a player name. This ID can be used with headshot plotting functions
* Added the axis scales `scale_x_cfb_headshots()` and `scale_y_cfb_headshots()`.
* Added the `gt_fmt_cfb_headshot()` function. (v0.0.0.9010)
