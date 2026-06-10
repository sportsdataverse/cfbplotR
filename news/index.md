# Changelog

## cfbplotR 0.1.0

- **Rebuilt on the [ggpath](https://mrcaseb.github.io/ggpath/)
  foundation** (the nflplotR/nbaplotR pattern). cfbplotR now resolves a
  CFB identifier to an image path and delegates all rendering — caching,
  aspect ratio, alpha, colorization — to ggpath.
- Dropped the `magick`, `RCurl`, `base64enc`, and `purrr` dependencies;
  added `ggpath (>= 1.1.0)`. Raised the requirements to `R (>= 4.1.0)`
  and `ggplot2 (>= 4.0.0)` (matching ggpath’s S7 theme elements).
- The public API is preserved:
  [`geom_from_path()`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.html),
  [`element_path()`](https://mrcaseb.github.io/ggpath/reference/element_path.html),
  [`ggpreview()`](https://cfbplotR.sportsdataverse.org/reference/ggpreview.md),
  and related generics remain available
  ([`geom_from_path()`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.html)
  and friends are now re-exported from ggpath).
- **Breaking (minor):**
  [`geom_mean_lines()`](https://mrcaseb.github.io/ggpath/reference/geom_lines.html)
  /
  [`geom_median_lines()`](https://mrcaseb.github.io/ggpath/reference/geom_lines.html)
  are now re-exported from ggpath and use ggpath’s aesthetics `x0` /
  `y0` (a vertical line at `x0`, a horizontal line at `y0`) instead of
  cfbplotR’s former `v_var` / `h_var`. Update calls accordingly,
  e.g. `geom_median_lines(aes(x0 = pass_epa, y0 = rush_epa))`.
- gt image helpers
  ([`gt_fmt_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)/[`gt_fmt_cfb_headshot()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)/[`gt_fmt_cfb_wordmark()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md))
  now embed via
  [`gt::web_image()`](https://gt.rstudio.com/reference/web_image.html).
- New family-parity conveniences:
  [`gt_cfb_cols_label()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb_cols_label.md),
  [`cfb_team_factor()`](https://cfbplotR.sportsdataverse.org/reference/cfb_team_factor.md),
  [`clean_team_abbrs()`](https://cfbplotR.sportsdataverse.org/reference/clean_team_abbrs.md),
  and
  [`.cfbplotR_clear_cache()`](https://cfbplotR.sportsdataverse.org/reference/dot-cfbplotR_clear_cache.md).
- Added repository community-health files (CLAUDE.md, Copilot
  instructions, issue/PR templates, CONTRIBUTING, Code of Conduct),
  normalized badges, a standard SportsDataverse README, a
  [`bibentry()`](https://rdrr.io/r/utils/bibentry.html) citation, and a
  richer pkgdown reference.

## cfbplotR 0.0.1 (pre-release history)

- Added the
  [`geom_cfb_logos()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_logos.md)
  geom.
- Added the
  [`geom_mean_lines()`](https://mrcaseb.github.io/ggpath/reference/geom_lines.html)
  and
  [`geom_median_lines()`](https://mrcaseb.github.io/ggpath/reference/geom_lines.html)
  geoms.
- Added the
  [`scale_color_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_color_cfb.md)
  and
  [`scale_fill_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_color_cfb.md)
  functions.
- Added FCS team logos. (v0.0.0.9002)
- Added the `gt_fmt_cfb()` function. (this function is now
  [`gt_fmt_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md))
  (v0.0.0.9003)
- Added the axis scales
  [`scale_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  and
  [`scale_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  in combination with the theme update functions
  [`theme_x_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md)
  and
  [`theme_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/theme_cfb.md).
  (v0.0.0.9004)
- Fixed bug where `gt_fmt_cfb()` wouldn’t work with Texas A&M (this
  function is now
  [`gt_fmt_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md))
  (v0.0.0.9005)
- Added NCAA logo as default for inputs not in
  [`valid_team_names()`](https://cfbplotR.sportsdataverse.org/reference/valid_team_names.md)(v0.0.0.9006)
- Fixed defaults for
  [`scale_y_cfb()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  to fix error.
- Added FCS colors (v0.0.0.9007)
- Added DII & DIII Logos
- Added input to
  [`valid_team_names()`](https://cfbplotR.sportsdataverse.org/reference/valid_team_names.md)
  to allow filtering by division (FBS, FCS, DII, DIII, or Conference
  logos) (v0.0.0.9008)
- Added the function
  [`cfb_team_tiers()`](https://cfbplotR.sportsdataverse.org/reference/cfb_team_tiers.md)
  that build an NFL team tiers ggplot, thanks to [Timo
  Riske](https://twitter.com/PFF_Moo) for the suggestion.
- Fixed a bug in
  [`geom_median_lines()`](https://mrcaseb.github.io/ggpath/reference/geom_lines.html)
  and
  [`geom_mean_lines()`](https://mrcaseb.github.io/ggpath/reference/geom_lines.html)
  that caused `alpha` to not work properly.
- Added the
  [`geom_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_headshots.md)
  geom that plots headshots for valid ESPN player IDs. (v0.0.9009)
- Added cleaning function
  [`clean_school_names()`](https://cfbplotR.sportsdataverse.org/reference/clean_school_names.md)
  which attempts to correct common name issues to names in
  [`valid_team_names()`](https://cfbplotR.sportsdataverse.org/reference/valid_team_names.md).
  The plotting functions call this internally to automatically attempt
  to fix errors.
- Added experimental function
  [`add_athlete_id_col()`](https://cfbplotR.sportsdataverse.org/reference/add_athlete_id_col.md)
  that adds a column to a data frame called `athlete_id` from the
  rosters data from the cfbfastR-data repo based on a player name. This
  ID can be used with headshot plotting functions
- Added the axis scales
  [`scale_x_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  and
  [`scale_y_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md).
- Added the
  [`gt_fmt_cfb_headshot()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)
  function. (v0.0.0.9010)
- Added colleges from `hoopR` package (v0.0.0.9011)
- Brought package up to date with nflplotR v1.0.0
- Added the
  [`geom_from_path()`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.html)
  geom that plots images from urls, local paths and more.
- Added the ggplot2 theme-elements
  [`element_cfb_logo()`](https://cfbplotR.sportsdataverse.org/reference/element.md),
  [`element_cfb_headshot()`](https://cfbplotR.sportsdataverse.org/reference/element.md),
  and
  [`element_path()`](https://mrcaseb.github.io/ggpath/reference/element_path.html)
  which translate college team names or player IDs into team logos and
  player headshots. These elements feature a major speed improvement
  over the axis scales
  [`scale_x_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  and
  [`scale_y_cfb_headshots()`](https://cfbplotR.sportsdataverse.org/reference/scale_axes_cfb.md)
  and make the package less dependent on an underlying package.
- added `P5` and `G5` catagories to
  [`valid_team_names()`](https://cfbplotR.sportsdataverse.org/reference/valid_team_names.md)
- added the
  [`gt_merge_stack_team_color()`](https://cfbplotR.sportsdataverse.org/reference/gt_stack_team.md)
  function (v0.0.0.9012)
- added the
  [`ggtitle_image()`](https://cfbplotR.sportsdataverse.org/reference/ggtitle_image.md)
  and
  [`theme_title_image()`](https://cfbplotR.sportsdataverse.org/reference/ggtitle_image.md)
  functions (v0.0.0.9013)
- added \>350 wordmarks for all FBS teams and many other schools and
  conferences from sportslogos.net
- added
  [`geom_cfb_wordmarks()`](https://cfbplotR.sportsdataverse.org/reference/geom_cfb_wordmarks.md)
  geom,
  [`element_cfb_wordmark()`](https://cfbplotR.sportsdataverse.org/reference/element.md)
  element, and
  [`gt_fmt_cfb_wordmark()`](https://cfbplotR.sportsdataverse.org/reference/gt_cfb.md)
  function to implement new wordmarks. (v0.0.0.9014)
- added PFF team names (thanks to Tej Seth) to
  [`clean_school_names()`](https://cfbplotR.sportsdataverse.org/reference/clean_school_names.md)
  which is run internally for plotting (v0.0.0.9015)
