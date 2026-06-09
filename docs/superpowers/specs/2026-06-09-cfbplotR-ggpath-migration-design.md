# cfbplotR → ggpath/nbaplotR Architecture Migration — Design

**Date:** 2026-06-09
**Status:** Approved (design)
**Templates:** [mrcaseb/ggpath](https://github.com/mrcaseb/ggpath), [mrcaseb/nbaplotR](https://github.com/mrcaseb/nbaplotR), [nflverse/nflplotR](https://github.com/nflverse/nflplotR)

## Goal

Re-platform cfbplotR so it sits on top of the **ggpath** foundation package — exactly as nbaplotR/nflplotR do — instead of carrying its own image-grob machinery and heavy image dependencies. The public API is preserved (zero breaking changes); the internals get smaller, faster, and inherit ggpath's caching/aspect-ratio/alpha handling. As a side effect this removes `magick`, which eliminates the libtiff CI fragility.

## Background — the gap

ggpath (extracted from nflplotR) owns the **generic** image machinery and exports:

```
GeomFromPath, GeomRefLines, element_path, element_raster,
geom_from_path, geom_mean_lines, geom_median_lines
+ S3: grobHeight.ggpath_element, grobWidth.ggpath_element
```

nbaplotR/nflplotR depend on ggpath and implement only **sport-specific** code (team→logo resolution, scales, theme elements, gt helpers, valid team names). They `Imports: ggpath` and carry **no** `magick`/`RCurl`/`base64enc`.

cfbplotR predates ggpath and re-implements the generic machinery itself (`geom_from_path.R`, `ggpreview.R`, `build_grobs.R`, `geom_lines.R`) with heavy deps `magick`, `RCurl`, `base64enc`, `purrr`.

## Decisions (confirmed)

1. **Full migration** — add ggpath to Imports, delete cfbplotR's own generic machinery, delegate all rendering to ggpath.
2. **Re-export for back-compat** — cfbplotR re-exports ggpath's generics so existing user code keeps working (the nflplotR pattern). Zero breaking changes.
3. **Keep all features + add family-parity conveniences** — preserve every cfbplotR feature; add the nflplotR conveniences cfbplotR lacks.
4. **Keep `ggtitle_image`/`theme_title_image`** as cfbplotR-specific extras (no nflplotR equivalent).
5. **Version bump** to `0.1.0` on release (pre-1.0; public API preserved via re-exports, so not a breaking semver change — internal overhaul + raised `R (>= 4.1.0)` documented in NEWS).

## Architecture

cfbplotR = thin CFB layer over ggpath. Every cfbplotR geom/element **resolves a CFB identifier → image path**, then delegates rendering to ggpath's grob builder (the `GeomNBAlogo` / `element_grob.element_nba_logo` pattern). cfbplotR owns: CFB team/logo/color data, ESPN headshot URL building, CFB scales, gt helpers, tiers, cleaning helpers, image-title extras.

### A. Re-exports (define nothing; re-export from ggpath; delete the source files)

| Re-exported from ggpath | cfbplotR file deleted |
|---|---|
| `geom_from_path`, `GeomFromPath` | `R/geom_from_path.R` |
| `ggpreview` | `R/ggpreview.R` |
| `geom_mean_lines`, `geom_median_lines`, `GeomRefLines` | `R/geom_lines.R` |
| `element_path`, `element_raster` (newly surfaced) | (grob machinery) `R/build_grobs.R` |
| `grobHeight.ggpath_element`, `grobWidth.ggpath_element` (S3) | cfbplotR's `*.axisImageGrob` S3 methods |

Re-exports live in `R/cfbplotR-package.R` via `@importFrom ggpath <fn>` + `@export <fn>` (roxygen `#' @export` re-export idiom), matching nflplotR. `element_raster` and `ggpreview` were exported before (ggpreview yes; element_raster is new) — `element_raster` is an **addition** (allowed, non-breaking).

### B. CFB-specific code — kept, internals re-platformed onto ggpath

- `geom_cfb_logos` / `GeomCFBlogo`, `geom_cfb_wordmarks` / `GeomCFBwordmark`, `geom_cfb_headshots` / `GeomCFBheads`: keep the public signatures; the ggproto `draw_key`/`draw_panel` resolve CFB team (or build the ESPN headshot URL) to an image path and hand off to ggpath's path-rendering (mirror `GeomNBAlogo`).
- `element_cfb_logo` / `element_cfb_wordmark` / `element_cfb_headshot` + their `element_grob.*` S3 methods: resolve team/id → path, then build the grob via ggpath so it is a `ggpath_element` (so ggpath's `grobHeight/Width.ggpath_element` apply). Mirror `element_grob.element_nba_logo`.
- `scale_color_cfb` / `scale_colour_cfb` / `scale_fill_cfb`: unchanged (CFB color data).
- `scale_x_cfb` / `scale_y_cfb` / `scale_x_cfb_headshots` / `scale_y_cfb_headshots`: set the axis text element to the cfbplotR element above (which now renders via ggpath).
- `theme_x_cfb` / `theme_y_cfb` (`R/theme_cfb.R`): unchanged (set `axis.text` to the cfbplotR elements).
- `gt_fmt_cfb_logo` / `gt_fmt_cfb_headshot` / `gt_fmt_cfb_wordmark`, `gt_merge_stack_team_color`: keep; embed images via gt's native image helpers (`gt::web_image`/`gt::local_image`) rather than `base64enc`+`magick`, matching `gt_nfl_logos`. (This is what lets `base64enc`/`magick` be dropped from Imports.)
- `cfb_team_tiers`, `clean_school_names`, `add_athlete_id_col`, `valid_team_names`: keep.
- `ggtitle_image` / `theme_title_image`: keep as cfbplotR extras; ensure they build on `element_path`/ggpath, not the deleted machinery.
- CFB team data in `R/sysdata.rda`: kept as-is.

### C. Parity additions (nflplotR conveniences cfbplotR lacks)

- `gt_cfb_cols_label` — gt column labels rendered as CFB logos (mirror `gt_nfl_cols_label`).
- `cfb_team_factor` — order a team vector as a factor by conference/standard order (mirror `nfl_team_factor`).
- `.cfbplotR_clear_cache` — clears ggpath's image cache (mirror `.nflplotR_clear_cache`).
- `clean_team_abbrs` — alias to `clean_school_names` for family-name parity (both exported; one wraps the other).

### D. DESCRIPTION / dependencies

- **Add:** `ggpath (>= 1.0.0)`. **Depends:** `R (>= 4.1.0)`.
- **Remove:** `magick`, `RCurl`, `base64enc`, `purrr`.
- **Keep (verify still used by CFB code, drop if not):** `cli`, `ggplot2 (>= 3.3.0)`, `gt`, `rlang`, `scales`, `grid`, `dplyr`, `glue`, `magrittr`.
- Run `usethis::use_tidy_description()`.
- **Bonus:** with `magick` gone, the cfbplotR pkgdown + R-CMD-check `ubuntu-22.04` pins (added earlier to dodge the jammy `libtiff.so.5`/`libMagick++` mismatch) can revert to `ubuntu-latest`.

### E. Target file layout (mirrors nbaplotR, adapted for cfbplotR's richer surface)

```
R/cfbplotR-package.R   # package doc + ALL ggpath re-exports + clear_cache
R/geom_cfb_logos.R     # GeomCFBlogo + geom_cfb_logos
R/geom_cfb_wordmarks.R # GeomCFBwordmark + geom_cfb_wordmarks
R/geom_cfb_headshots.R # GeomCFBheads + geom_cfb_headshots (ESPN URL build)
R/theme-elements.R     # element_cfb_logo/wordmark/headshot + element_grob.* S3
R/scales.R             # scale_color/colour/fill_cfb + scale_x/y_cfb(+_headshots)
R/theme_cfb.R          # theme_x_cfb / theme_y_cfb
R/gt_cfb.R             # gt_fmt_cfb_* + gt_cfb_cols_label + gt_merge_stack_team_color
R/cfb_team_tiers.R     # cfb_team_tiers + cfb_team_factor
R/cleaning_helpers.R   # clean_school_names + clean_team_abbrs + add_athlete_id_col + valid_team_names
R/ggtitle_image.R      # ggtitle_image + theme_title_image
R/utils.R              # %||%, internal helpers, .cfbplotR_clear_cache
R/data.R               # team data doc
R/sysdata.rda          # CFB team data (kept)
```
Deleted: `R/geom_from_path.R`, `R/ggpreview.R`, `R/build_grobs.R`, `R/geom_lines.R`. (`R/gt_stack_team.R` folds into `R/gt_cfb.R`.)

## Testing

- Keep the existing testthat suite; update assertions affected by the re-platforming.
- Add vdiffr/snapshot tests for the re-platformed geoms (logos/wordmarks/headshots) and the new conveniences (`gt_cfb_cols_label`, `cfb_team_factor`).
- Confirm the re-exports are reachable (`cfbplotR::geom_from_path`, etc.).
- Regenerate roxygen docs + NAMESPACE (`devtools::document()`); the dead-URL example fix already landed stays.
- Green gate: `devtools::check()` (0 errors/warnings) + pkgdown build.

## Release

- Bump to `0.1.0`; `NEWS.md`: built on ggpath; dropped magick/RCurl/base64enc/purrr; `R (>= 4.1.0)`; public API preserved via re-exports; new `gt_cfb_cols_label`/`cfb_team_factor`/`.cfbplotR_clear_cache`/`clean_team_abbrs`; runner pins reverted.
- Update `_pkgdown.yml` reference index for the new exports; `cran-comments.md` for the dependency change.

## Out of scope

- Changing CFB team data / colors / logo sources.
- The recruitR/cfb4th issues from the prior task (tracked separately).
- Any new plotting features beyond the listed parity conveniences.
