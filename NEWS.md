<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [cfbplotR 0.1.0](#cfbplotr-010)
- [cfbplotR 0.0.1 (pre-release history)](#cfbplotr-001-pre-release-history)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# cfbplotR 0.1.0

* **Rebuilt on the [ggpath](https://mrcaseb.github.io/ggpath/) foundation** (the nflplotR/nbaplotR pattern). cfbplotR now resolves a CFB identifier to an image path and delegates all rendering — caching, aspect ratio, alpha, colorization — to ggpath.
* Dropped the `magick`, `RCurl`, `base64enc`, and `purrr` dependencies; added `ggpath (>= 1.0.0)`. Raised the R requirement to `R (>= 4.1.0)`.
* The public API is preserved: `geom_from_path()`, `element_path()`, `geom_mean_lines()`/`geom_median_lines()`, and related generics are now re-exported from ggpath.
* gt image helpers (`gt_fmt_cfb_logo()`/`gt_fmt_cfb_headshot()`/`gt_fmt_cfb_wordmark()`) now embed via `gt::web_image()`.
* New family-parity conveniences: `gt_cfb_cols_label()`, `cfb_team_factor()`, `clean_team_abbrs()`, and `.cfbplotR_clear_cache()`.
* Added repository community-health files (CLAUDE.md, Copilot instructions, issue/PR templates, CONTRIBUTING, Code of Conduct), normalized badges, a standard SportsDataverse README, a `bibentry()` citation, and a richer pkgdown reference.

# cfbplotR 0.0.1 (pre-release history)

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
* Added colleges from `hoopR` package (v0.0.0.9011)
* Brought package up to date with nflplotR v1.0.0
* Added the `geom_from_path()` geom that plots images from urls, local paths and more.
* Added the ggplot2 theme-elements `element_cfb_logo()`, `element_cfb_headshot()`, 
and `element_path()` which translate college team names or player IDs into team logos and player headshots. These elements feature a major speed improvement over the axis scales `scale_x_cfb_headshots()` and `scale_y_cfb_headshots()` and make the package less dependent on an underlying package.
* added `P5` and `G5` catagories to `valid_team_names()`
* added the `gt_merge_stack_team_color()` function (v0.0.0.9012)
* added the `ggtitle_image()` and `theme_title_image()` functions (v0.0.0.9013)
* added >350 wordmarks for all FBS teams and many other schools and conferences from sportslogos.net
* added `geom_cfb_wordmarks()` geom, `element_cfb_wordmark()` element, and `gt_fmt_cfb_wordmark()` function to implement new wordmarks. (v0.0.0.9014)
* added PFF team names (thanks to Tej Seth) to `clean_school_names()` which is run internally for plotting (v0.0.0.9015)
