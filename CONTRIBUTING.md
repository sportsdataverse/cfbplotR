<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Contributing to cfbplotR](#contributing-to-cfbplotr)
- [Ways to Contribute](#ways-to-contribute)
- [Getting Set Up](#getting-set-up)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Contributing to cfbplotR

Thanks for your interest in improving `cfbplotR`, the SportsDataverse ggplot2 extension for
college-football logos, wordmarks, and player headshots!

By participating you agree to abide by the [Code of Conduct](CODE_OF_CONDUCT.md).

## Ways to Contribute

- **Report a bug** -- open an issue using the Bug Report template.
- **Request a feature** -- open an issue using the Feature Request template.
- **Improve docs** -- typo fixes, clearer examples, and updated screenshots are always welcome.
- **Add or fix a geom/element/scale/gt helper** -- see the conventions below.
- **Update team data** -- logo and wordmark URLs go stale; PRs that refresh `data-raw/` are valuable.

## Getting Set Up

1. Fork and clone the repository.
2. Install development dependencies (pak recommended):

   ```r
   # recommended:
   install.packages("pak")
   pak::local_install_dev_deps()

   # or with devtools:
   install.packages("devtools")
   devtools::install_dev_deps()
   ```

3. Load the package for interactive development:

   ```r
   devtools::load_all()
   ```

## Development Workflow

```r
devtools::load_all()      # load the package
devtools::document()      # regenerate man/ + NAMESPACE after changing roxygen2
devtools::test()          # run the full test suite
devtools::check()         # full R CMD check -- run before opening a PR
devtools::build_readme()  # re-render README.md from README.Rmd
pkgdown::build_site()     # preview the pkgdown site locally
```

## Coding Conventions

`cfbplotR` follows the conventions documented in [CLAUDE.md](CLAUDE.md). The essentials:

### Architecture

cfbplotR is a thin domain layer on top of [ggpath](https://github.com/mrcaseb/ggpath).
ggpath owns image fetching, caching, colorizing, and rendering. cfbplotR:

1. Resolves a CFB team name or ESPN player ID to an image URL/path (via `logo_from_school()`,
   `wordmark_from_school()`, `headshot_from_id()` in `R/utils.R`).
2. Delegates rendering to ggpath (`ggpath::GeomFromPath$draw_panel()` for geoms;
   `ggplot2::element_grob(ggpath::element_path(...))` for theme elements).

**Never re-implement ggpath rendering inside cfbplotR.**

### ggpath S7 note

`ggpath::element_path` is an **S7 object**. The internal helper `.cfb_element_to_path_grob()`
must construct it via `ggpath::element_path(alpha=..., colour=..., hjust=..., vjust=..., size=...)`.
Do NOT `structure()` a plain list and re-class it -- S7 property access will break.

### Naming

| Family | Pattern | Example |
|--------|---------|---------|
| Geoms | `geom_cfb_*` | `geom_cfb_logos()` |
| Theme elements | `element_cfb_*` | `element_cfb_logo()` |
| Color/fill scales | `scale_color_cfb`, `scale_fill_cfb` | |
| Position scales | `scale_x_cfb`, `scale_y_cfb` | |
| gt formatters | `gt_fmt_cfb_*`, `gt_cfb_*` | `gt_fmt_cfb_logo()` |
| Team utilities | `cfb_*` | `cfb_team_tiers()` |
| Name cleaners | `clean_*` | `clean_school_names()` |

### Internal resolvers

Use the three resolver helpers -- do not inline URL construction:

- `logo_from_school(team)` -- team name -> logo URL (falls back to NCAA logo)
- `wordmark_from_school(team)` -- team name -> wordmark URL (falls back to NCAA wordmark)
- `headshot_from_id(player_id)` -- ESPN player ID -> ESPN CDN headshot URL

### Re-exporting ggpath

Add new ggpath re-exports to `R/reexports.R` using the standard roxygen2 pattern:

```r
#' @importFrom ggpath new_symbol
#' @export
ggpath::new_symbol
```

### Team data

`R/sysdata.rda` is generated from `data-raw/`. After updating team data, re-source the
relevant script and run `devtools::document()`. Never edit `sysdata.rda` by hand.

### Messaging

Use `cli::cli_warn()` for user-facing warnings, `cli::cli_abort()` for errors, and
`cli::cli_inform()` for informational messages. Do not use bare `warning()` or `message()`.

## Testing

### Test types

- **Snapshot tests**: visual geoms and theme elements use `vdiffr::expect_doppelganger()`.
  After intentional visual changes, run `vdiffr::manage_cases()` to review and accept new
  snapshots. Do not delete `.svg` files manually.
- **Data/resolver tests**: cleaning helpers and resolver functions use plain `expect_*`.
- **Network tests**: guard with `skip_if_offline()` and `skip_on_cran()`.

### Test pattern

```r
test_that("geom_cfb_logos renders a known team", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  p <- ggplot2::ggplot(
    data.frame(x = 1, y = 1, team = "Alabama"),
    ggplot2::aes(x = x, y = y, team = team)
  ) +
    geom_cfb_logos(width = 0.1)

  vdiffr::expect_doppelganger("geom-cfb-logos-alabama", p)
})
```

### Column assertions (subset direction)

Use subset-direction assertions so that upstream data additions never break the suite:

```r
expect_in(sort(required_cols), sort(colnames(df)))   # expected ⊆ actual
```

Never write `expect_equal(sort(colnames(df)), sort(required_cols))` -- that fails if the
upstream data gains a new column.

### Adding a test file

Name it `tests/testthat/test-<function-slug>.R`. Gate network tests as described above.

## Documentation

- Never hand-edit `NAMESPACE` or anything in `man/` -- regenerate with `devtools::document()`.
- Re-render `README.md` with `devtools::build_readme()` and commit `README.Rmd` + `README.md`
  together. Never hand-edit `README.md`.
- Add new exported functions to the `reference:` section of `_pkgdown.yml` if they are not
  already covered by the existing `starts_with()` selectors.
- For user-visible changes, update `NEWS.md`.
- Regenerate doctoc TOCs after editing this file, `CLAUDE.md`, `.github/copilot-instructions.md`,
  or `.github/pull_request_template.md`:

  ```sh
  Rscript tools/run_doctoc.R --maxlevel 2 \
    CONTRIBUTING.md CLAUDE.md \
    .github/copilot-instructions.md .github/pull_request_template.md
  ```

## Commit & PR Conventions

- Use [Conventional Commits](https://www.conventionalcommits.org/):
  `feat(geom):`, `fix(elements):`, `docs:`, `test:`, `refactor:`, `chore:`, `ci:`.
  Mark breaking changes with `type!:` or a `BREAKING CHANGE:` footer.
- Keep unrelated changes in separate commits.
- **Do not add AI tools (Claude, Copilot, etc.) as commit co-authors.**
- Open your PR against `main`, fill in the PR template, and confirm `devtools::check()` and
  `devtools::test()` pass before requesting review.

Thanks again for contributing!
