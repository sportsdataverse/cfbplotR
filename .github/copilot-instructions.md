<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [GitHub Copilot Instructions -- cfbplotR](#github-copilot-instructions----cfbplotr)
- [Golden Rules](#golden-rules)
- [Architecture](#architecture)
- [Function Families](#function-families)
- [Geom Pattern](#geom-pattern)
- [Theme Element Pattern](#theme-element-pattern)
- [Testing](#testing)
- [After Changing Code](#after-changing-code)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GitHub Copilot Instructions -- cfbplotR

These instructions tell GitHub Copilot (and other AI coding assistants) how to write code
that fits this repository. For the authoritative, fuller guide see [CLAUDE.md](../CLAUDE.md)
and [CONTRIBUTING.md](../CONTRIBUTING.md).

`cfbplotR` is a **GitHub-only** ggplot2 extension for visualizing college-football logos,
wordmarks, and player headshots. It is built on top of [`ggpath`](https://github.com/mrcaseb/ggpath),
which handles image fetching, caching, colorizing, and rendering.

## Golden Rules

1. **Never re-implement ggpath rendering.** Delegate to `ggpath::GeomFromPath$draw_panel()` for geoms and to `ggplot2::element_grob(ggpath::element_path(...))` for theme elements.
2. **`ggpath::element_path` is an S7 object.** Always construct it via `ggpath::element_path(alpha=..., colour=..., hjust=..., vjust=..., size=...)`. Never `structure()` a plain list with that class.
3. **Use the three resolver helpers** (`logo_from_school`, `wordmark_from_school`, `headshot_from_id`) from `R/utils.R`. Do not duplicate URL-building logic.
4. **Re-export ggpath symbols** in `R/reexports.R` using the `@importFrom ggpath <symbol> / @export / ggpath::<symbol>` pattern. Do not re-implement.
5. **Never hand-edit** `NAMESPACE`, `man/`, or `R/sysdata.rda`.
6. **Tests use subset-direction assertions**: `expect_in(sort(core_cols), sort(colnames(df)))`.
7. **Never add AI assistants as commit co-authors.**

## Architecture

```
Team name / player ID
      |
      v
Resolver helpers (R/utils.R)
  logo_from_school()       -- looks up logo_list[[school]]
  wordmark_from_school()   -- looks up wordmark_list[[school]]
  headshot_from_id()       -- builds ESPN CDN URL
      |
      v
image URL / path
      |
      v
ggpath renders
  GeomFromPath$draw_panel()     (for geom_cfb_* geoms)
  element_path S7 object        (for element_cfb_* theme elements)
```

## Function Families

| Family | Prefix / Names |
|--------|----------------|
| Geoms | `geom_cfb_logos`, `geom_cfb_wordmarks`, `geom_cfb_headshots` |
| Theme elements | `element_cfb_logo`, `element_cfb_wordmark`, `element_cfb_headshot` |
| Color/fill scales | `scale_color_cfb`, `scale_fill_cfb` |
| Position scales | `scale_x_cfb`, `scale_y_cfb` |
| gt formatters | `gt_fmt_cfb_*`, `gt_cfb_cols_label`, `gt_stack_team_column` |
| Team utilities | `cfb_team_tiers`, `cfb_team_factor` |
| Name cleaners | `clean_school_names`, `clean_team_abbrs`, `valid_team_names` |
| Title helpers | `ggtitle_image`, `theme_title_image` |
| ggpath re-exports | `geom_from_path`, `GeomFromPath`, `element_path`, `element_raster`, `geom_mean_lines`, `geom_median_lines`, `GeomRefLines` |

## Geom Pattern

```r
geom_cfb_logos <- function(...) {
  ggplot2::layer(
    geom = GeomCFBLogos, ...
  )
}

GeomCFBLogos <- ggplot2::ggproto("GeomCFBLogos", ggpath::GeomFromPath,
  draw_panel = function(data, panel_params, coord, ...) {
    data$path <- logo_from_school(data$team)
    ggpath::GeomFromPath$draw_panel(data, panel_params, coord, ...)
  }
)
```

## Theme Element Pattern

```r
element_cfb_logo <- function(alpha = NULL, colour = NA, hjust = NULL,
                              vjust = NULL, color = NULL, size = 0.5) {
  if (!is.null(color)) colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cfb_logo", "element_text", "element")
  )
}

element_grob.element_cfb_logo <- function(element, label = "", x = NULL, y = NULL,
                                           alpha = NULL, colour = NULL,
                                           hjust = NULL, vjust = NULL,
                                           size = NULL, ...) {
  if (is.null(label)) return(ggplot2::zeroGrob())
  label <- logo_from_school(label)
  .cfb_element_to_path_grob(element, label, x, y, alpha, colour, hjust, vjust, size, ...)
}

# .cfb_element_to_path_grob() constructs ggpath::element_path() as S7 object,
# then calls ggplot2::element_grob() on it.
```

## Testing

- Snapshot tests for visual output: `vdiffr::expect_doppelganger("name", plot)`.
- Data/resolver tests: plain `expect_*` assertions.
- Network tests: always guard with `skip_if_offline()` and `skip_on_cran()`.
- Column assertions (subset direction): `expect_in(sort(required_cols), sort(colnames(x)))`.
- Update snapshots: `vdiffr::manage_cases()`.

## After Changing Code

- `devtools::document()` -- regenerate `man/` + `NAMESPACE`.
- `devtools::test()` -- run the full test suite.
- `devtools::check()` -- full R CMD check before opening a PR.
- `devtools::build_readme()` -- if `README.Rmd` changed.
- Add new exports to `_pkgdown.yml` `reference:` if not covered by existing `starts_with()` selectors.
- Update `NEWS.md` for user-visible changes.
- Commit with Conventional Commits (`feat(geom):`, `fix(elements):`, `docs:`, `test:`, ...). No AI co-authors.

## Cheat sheet

There is a printable one-page reference for this package at
<https://sportsdataverse.org/cheatsheets/cfbplotR-cfb4th-cfbseedR.pdf>, one of [a set covering every SportsDataverse package](https://sportsdataverse.org/cheatsheets).
Keep it in mind when adding or renaming an exported function: the sheet is a
hand-built canvas, so a surface change means the sheet needs a revision too.
