<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [CLAUDE.md -- cfbplotR Development Guide](#claudemd----cfbplotr-development-guide)
- [Package Overview](#package-overview)
- [Architecture](#architecture)
- [Function Families](#function-families)
- [The ggpath Re-export Pattern](#the-ggpath-re-export-pattern)
- [Internal Resolvers](#internal-resolvers)
- [Team Data in `sysdata.rda`](#team-data-in-sysdatarda)
- [Build & Development Commands](#build-development-commands)
- [Testing](#testing)
- [Documentation Maintenance](#documentation-maintenance)
- [Commit Convention](#commit-convention)
- [Common Pitfalls](#common-pitfalls)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLAUDE.md -- cfbplotR Development Guide

## Package Overview

`cfbplotR` provides ggplot2 geoms, theme elements, scales, and gt table helpers for visualizing college-football logos, wordmarks, and player headshots — the CFB analogue of [`nflplotR`](https://nflplotr.nflverse.com). It is built on top of the [`ggpath`](https://github.com/mrcaseb/ggpath) package (the nflplotR/nbaplotR pattern), which handles image fetching, caching, colorizing, and alpha-transparency rendering. Companion package: [`cfbfastR`](https://cfbfastR.sportsdataverse.org) (Suggests) for CFB data.

- **Version**: 0.1.0 (per `DESCRIPTION`)
- **R Requirement**: >= 4.1.0; `ggplot2 (>= 4.0.0)`, `ggpath (>= 1.1.0)` (S7 theme elements)
- **License**: MIT
- **Distribution**: r-universe (`sportsdataverse.r-universe.dev`); CRAN badges in `README.Rmd` are commented out (not yet on CRAN)
- **Repository**: <https://github.com/sportsdataverse/cfbplotR>
- **Maintainer**: Jared Lee (`13jaredlee@gmail.com`)
- **Branch**: `main` is the default and release branch.

Install the development version:

```r
# pak (recommended):
pak::pkg_install("sportsdataverse/cfbplotR")

# or devtools:
devtools::install_github("sportsdataverse/cfbplotR")
```

## Architecture

`cfbplotR` sits as a thin domain layer on top of `ggpath`:

1. **ID resolution** (`R/utils.R`): `logo_from_school()`, `wordmark_from_school()`, and `headshot_from_id()` translate a CFB team name or ESPN player ID into an image URL or path.
2. **Geoms** (`R/geom_cfb_logos.R`, `R/geom_cfb_wordmarks.R`, `R/geom_cfb_headshots.R`): each geom resolves the `team` or `player_id` aesthetic to a path via the resolver helpers, then delegates all rendering to `ggpath::GeomFromPath$draw_panel()`.
3. **Theme elements** (`R/theme-elements.R`): `element_cfb_logo()`, `element_cfb_wordmark()`, and `element_cfb_headshot()` are S3 element objects (class `element_cfb_*`). Their `element_grob.*` methods resolve the label to a path/URL and then call the internal helper `.cfb_element_to_path_grob()`, which constructs a proper `ggpath::element_path` S7 object (via `ggpath::element_path(...)`) and calls `ggplot2::element_grob()` on it.
   - **Critical**: `ggpath::element_path` is an **S7 object** -- the helper must construct it via `ggpath::element_path(alpha=..., colour=..., hjust=..., vjust=..., size=...)`. Do NOT try to `structure()` a plain list and re-class it; S7 property access would break.
4. **Scales** (`R/scale_cfb.R`): `scale_color_cfb()`, `scale_fill_cfb()`, `scale_x_cfb()`, `scale_y_cfb()` -- map team names to CFB primary/alt colors or logo positions.
5. **gt helpers** (`R/gt_fmt_cfb.R`, `R/gt_stack_team.R`): `gt_fmt_cfb_logo()`, `gt_fmt_cfb_wordmark()`, `gt_fmt_cfb_headshot()`, `gt_cfb_cols_label()`, `gt_merge_stack_team_color()` -- format cells in a gt table with logos, wordmarks, headshots, or team-colored stacked text.
6. **Team utilities** (`R/cfb_team_tiers.R`): `cfb_team_tiers()` (premade tier plot), `cfb_team_factor()` (factor-orders teams), plus `add_athlete_id_col()` (`R/utils.R`).
7. **Cleaning helpers** (`R/cleaning_helpers.R`): `clean_school_names()`, `clean_team_abbrs()` -- standardize user-supplied team name strings to cfbplotR's internal keys.
8. **Re-exports** (`R/reexports.R`): a set of ggpath symbols (`geom_from_path`, `GeomFromPath`, `element_path`, `element_raster`, `geom_mean_lines`, `geom_median_lines`, `GeomRefLines`) are re-exported so users do not need to attach ggpath directly.
9. **Team data**: internal named lists `logo_list` and `wordmark_list` (in `R/sysdata.rda`) map school names to image URLs. **Exported** `data/` objects — `logo_ref` (`school`, `color`, `alt_color`, logo URLs; powers `valid_team_names()`), `team_name_mapping`, and `team_colors` — are documented in `R/data.R`. All regenerated via `data-raw/build_logo_list.R` / `build_wordmark_list.R` / `build_team_colors.R`.

## Function Families

All names below are verified against `NAMESPACE`.

| Family | Functions |
|--------|-----------|
| Geoms | `geom_cfb_logos()`, `geom_cfb_wordmarks()`, `geom_cfb_headshots()` (ggproto: `GeomCFBlogo`, `GeomCFBwordmark`, `GeomCFBheads`) |
| Theme elements | `element_cfb_logo()`, `element_cfb_wordmark()`, `element_cfb_headshot()` |
| Color/fill scales | `scale_color_cfb()` / `scale_colour_cfb()`, `scale_fill_cfb()` |
| Axis logo/headshot scales | `scale_x_cfb()`, `scale_y_cfb()`, `scale_x_cfb_headshots()`, `scale_y_cfb_headshots()`, `theme_x_cfb()`, `theme_y_cfb()` |
| gt formatters | `gt_fmt_cfb_logo()`, `gt_fmt_cfb_wordmark()`, `gt_fmt_cfb_headshot()`, `gt_cfb_cols_label()`, `gt_merge_stack_team_color()` |
| Team utilities | `cfb_team_tiers()`, `cfb_team_factor()`, `add_athlete_id_col()` |
| Name cleaners | `clean_school_names()`, `clean_team_abbrs()`, `valid_team_names()` |
| Title / preview helpers | `ggtitle_image()`, `theme_title_image()`, `ggpreview()` |
| ggpath re-exports | `geom_from_path`, `GeomFromPath`, `element_path`, `element_raster`, `geom_mean_lines`, `geom_median_lines`, `GeomRefLines` |
| Cache | `.cfbplotR_clear_cache()` |

## The ggpath Re-export Pattern

`R/reexports.R` re-exports ggpath symbols using the standard roxygen2 pattern:

```r
#' @importFrom ggpath geom_from_path
#' @export
ggpath::geom_from_path
```

This lets package users call `geom_from_path()` without attaching ggpath. Do not duplicate the implementation -- always re-export the canonical ggpath object.

## Internal Resolvers

All three resolver helpers live in `R/utils.R` and are **not exported**:

- `logo_from_school(team)`: calls `clean_school_names()` to normalize, warns on invalid names and falls back to `"NCAA"`, then looks up `logo_list[[t]]`.
- `wordmark_from_school(team)`: same pattern, falls back to the NCAA wordmark.
- `headshot_from_id(player_id)`: wraps `headshot_id_to_url()` which builds `http://a.espncdn.com/i/headshots/college-football/players/full/<id>.png`.

These return character vectors of image URLs/paths. ggpath fetches, caches, and renders those paths.

## Team Data in `sysdata.rda`

Two storage tiers, both built by `data-raw/` scripts:

- **Internal** (`R/sysdata.rda`, `usethis::use_data(..., internal = TRUE)`): `logo_list` and `wordmark_list` -- named character vectors `name -> image URL`. Accessed as bare names inside the package.
- **Exported** (`data/*.rda`, lazy-loaded, documented in `R/data.R`): `logo_ref` (`school`, `color`, `alt_color`, logo URLs; powers `valid_team_names()`), `team_name_mapping`, and `team_colors`.

Regenerate after updating team data (`build_logo_list.R` writes both `logo_ref` and the internal lists):

```r
source("data-raw/build_logo_list.R")
devtools::document()
```

**Never edit `sysdata.rda` or `data/*.rda` by hand.**

## Build & Development Commands

```r
# Load package for interactive development
devtools::load_all()

# Regenerate man/ + NAMESPACE after changing roxygen2 comments
devtools::document()

# Run the full test suite
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-geom-cfb-logos.R")

# Full R CMD check (run before opening a PR)
devtools::check()

# Re-render README.md from README.Rmd
devtools::build_readme()

# Install locally
devtools::install()

# Build pkgdown site locally
pkgdown::build_site()
```

## Testing

### Test types

- **Snapshot tests** (via [vdiffr](https://vdiffr.r-lib.org/)): visual geoms and theme elements are tested with `vdiffr::expect_doppelganger()`. Run `vdiffr::manage_cases()` to review/update snapshots after intentional visual changes.
- **Data unit tests**: resolver functions, cleaning helpers, and scale/gt functions are tested with standard `expect_*` assertions (no rendering required).
- **Network guard**: any test that hits a live URL (e.g. ESPN headshot CDN) must be wrapped in `skip_if_offline()` and `skip_on_cran()`.

### Test pattern

```r
test_that("geom_cfb_logos renders without error", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  p <- ggplot2::ggplot(
    data.frame(x = 1, y = 1, team = "Alabama"),
    ggplot2::aes(x = x, y = y, team = team)
  ) +
    geom_cfb_logos(width = 0.1)

  vdiffr::expect_doppelganger("geom_cfb_logos-basic", p)
})
```

### Column/resolver assertions

```r
test_that("logo_from_school returns character vector", {
  result <- cfbplotR:::logo_from_school(c("Alabama", "Auburn"))
  expect_type(result, "character")
  expect_length(result, 2L)
})
```

Use **subset-direction** assertions for column checks (upstream data can add columns):

```r
expect_in(sort(core_cols), sort(colnames(df)))  # expected ⊆ actual
```

## Documentation Maintenance

### Roxygen2 / NAMESPACE / man/

Never hand-edit `NAMESPACE` or anything in `man/`. Regenerate after any roxygen2 change:

```r
devtools::document()
```

### Markdown TOCs (doctoc)

`CLAUDE.md`, `CONTRIBUTING.md`, `.github/copilot-instructions.md`, and `.github/pull_request_template.md` carry a doctoc-generated TOC inside the standard marker comments. After editing any of those files, regenerate:

```sh
Rscript tools/run_doctoc.R --maxlevel 2 \
  CLAUDE.md CONTRIBUTING.md \
  .github/copilot-instructions.md .github/pull_request_template.md
```

`tools/run_doctoc.R` is a no-Node.js R replacement for the npm `doctoc` CLI. It is idempotent.

### README.md

`README.md` is rendered from `README.Rmd`. Never hand-edit `README.md`. After editing `README.Rmd`:

```r
devtools::build_readme()
```

Commit `README.Rmd` and the regenerated `README.md` together.

### DESCRIPTION

After adding/removing packages or bumping versions:

```r
usethis::use_tidy_description()
```

### NEWS.md / _pkgdown.yml

- **`NEWS.md`**: add bullets under the current unreleased version heading.
- **`_pkgdown.yml`**: new exported functions whose names match `starts_with("geom_cfb")` / `starts_with("element_cfb")` / `starts_with("scale_")` / `starts_with("gt_")` / `starts_with("cfb_")` / `starts_with("clean_")` are picked up by the existing selectors; otherwise add them to the `reference:` section manually.

## Commit Convention

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add geom_cfb_logos() clip-circle option
fix: correct wordmark fallback for unknown school names
docs: update README with new scale examples
test: add vdiffr snapshot for element_cfb_headshot
refactor: extract resolver helpers to utils.R
chore: regenerate sysdata.rda with 2025 team data
ci: update R CMD check workflow to ubuntu-latest
```

Prefer scoped subjects when useful (`feat(geom):`, `fix(elements):`, `docs(readme):`, etc.). Use `type!:` or a `BREAKING CHANGE:` footer for breaking changes. Keep unrelated work in separate commits.

**Never add AI tools (Claude, Copilot, etc.) as commit co-authors.**

## Common Pitfalls

- `ggpath::element_path` is an **S7 object**. Always construct it via `ggpath::element_path(alpha=..., colour=..., ...)`. Never `structure(list(...), class="element_path")`.
- `logo_list` and `wordmark_list` live in `sysdata.rda` (internal, not exported). Access them as bare names inside the package; in tests use `cfbplotR:::logo_list`. `logo_ref` / `team_name_mapping` / `team_colors` ARE exported (`cfbplotR::logo_ref`).
- `.cfbplotR_clear_cache()` only delegates to `ggpath::clear_cache()` when that symbol is exported by the installed ggpath (`getNamespaceExports("ggpath")` guard) — image caching itself is owned by ggpath, not cfbplotR.
- `valid_team_names()` calls `clean_school_names()` internally -- do not call both in sequence or you will double-normalize.
- `geom_cfb_*` geoms delegate to `ggpath::GeomFromPath$draw_panel()`; do not re-implement rendering logic inside cfbplotR.
- Never hand-edit `NAMESPACE`, `man/`, or `R/sysdata.rda`.
- vdiffr snapshots live in `tests/testthat/_snaps/`. After intentional visual changes, run `vdiffr::manage_cases()` to accept the new snapshot; do not delete `.svg` files manually.
- `devtools::build_readme()` must be re-run after any edit to `README.Rmd`.
