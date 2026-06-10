# cfbplotR → ggpath/nbaplotR Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Re-platform cfbplotR onto the ggpath foundation package (the nbaplotR/nflplotR pattern), preserving the public API via re-exports, then bring the repo to the SDV professional standard (community-health files, badges, README, rich pkgdown).

**Architecture:** cfbplotR becomes a thin CFB layer over ggpath. Each geom resolves a CFB id → image path then calls `ggpath::GeomFromPath$draw_panel()`; each theme element resolves id → path, re-classes itself to ggpath's `element_path`, and delegates to `ggplot2::element_grob()`. ggpath owns all image reading/caching/colorizing/alpha. cfbplotR drops `magick`/`RCurl`/`base64enc`/`purrr`.

**Tech Stack:** R, ggplot2, ggpath (>= 1.0.0), gt, cli, rlang, scales, grid; roxygen2/devtools/testthat/vdiffr; usethis; pkgdown.

**Reference:** spec at `docs/superpowers/specs/2026-06-09-cfbplotR-ggpath-migration-design.md`. Repo root: `c:/Users/saiem/Documents/GitHub-Data/sdv-dev/cfbplotR`. Run all R from the package root. The repo is clean on `main`; commit after each task; do NOT skip hooks; no AI co-author trailers.

**R-package testing note:** cfbplotR's visual geoms/elements are verified with `vdiffr` snapshot tests and a build-green gate (`devtools::check()`), not classic TDD. Pure-data helpers (resolution helpers, `cfb_team_factor`, `clean_team_abbrs`, re-export reachability) DO get fast unit tests written test-first. Each task ends by regenerating docs (`devtools::document()`) when roxygen changed, then committing.

---

## File structure (target)

```
R/cfbplotR-package.R   # "_PACKAGE" doc + usethis namespace block (unchanged shell)
R/reexports.R          # NEW — re-export ggpath generics (geom_from_path, element_path, ...)
R/utils.R              # %||%; NEW internal resolvers logo_from_school/wordmark_from_school/headshot_from_id; .cfbplotR_clear_cache
R/geom_cfb_logos.R     # GeomCFBlogo$draw_panel -> ggpath delegation
R/geom_cfb_wordmarks.R # GeomCFBwordmark$draw_panel -> ggpath delegation
R/geom_cfb_headshots.R # GeomCFBheads$draw_panel -> ggpath delegation
R/theme-elements.R     # element_cfb_logo/wordmark/headshot + element_grob.* -> re-class to element_path + delegate
R/scale_cfb.R          # scale_color/colour/fill_cfb + scale_x/y_cfb(+_headshots) (unchanged logic)
R/theme_cfb.R          # theme_x_cfb / theme_y_cfb (unchanged)
R/gt_fmt_cfb.R         # gt_fmt_cfb_logo/headshot/wordmark via gt native image; + gt_cfb_cols_label
R/gt_stack_team.R      # gt_merge_stack_team_color (unchanged)
R/cfb_team_tiers.R     # cfb_team_tiers + NEW cfb_team_factor
R/cleaning_helpers.R   # clean_school_names + NEW clean_team_abbrs alias + add_athlete_id_col + valid_team_names
R/ggtitle_image.R      # ggtitle_image + theme_title_image (on ggpath element_path)
R/data.R               # data docs (unchanged)
R/sysdata.rda          # CFB team data: logo_list, wordmark_list, etc. (unchanged)
```
Deleted: `R/build_grobs.R`, `R/geom_from_path.R`, `R/ggpreview.R`, `R/geom_lines.R`.

---

# PART 1 — ggpath migration

## Task 1: Add ggpath, create re-exports, delete generic machinery

**Files:**
- Modify: `DESCRIPTION`
- Create: `R/reexports.R`
- Delete: `R/geom_from_path.R`, `R/ggpreview.R`, `R/geom_lines.R`, `R/build_grobs.R`
- Modify: `NAMESPACE` (regenerated)

- [ ] **Step 1: Add ggpath to Imports, raise R, drop heavy deps in `DESCRIPTION`**

In `DESCRIPTION`, set the `Depends`/`Imports` to:
```
Depends:
    R (>= 4.1.0)
Imports:
    cli (>= 3.0.0),
    ggpath (>= 1.0.0),
    ggplot2 (>= 3.3.0),
    glue,
    gt,
    magrittr (>= 2.0.0),
    rlang (>= 0.4.11),
    scales (>= 1.1.0)
```
Remove `base64enc`, `magick`, `RCurl`, `purrr`, and `dplyr` from Imports for now (re-add `dplyr` only if Step in Task 7/8 needs it; verify with `grep -rn "dplyr::" R/`). Keep `Suggests` as-is but add `vdiffr` and `ggpath`-free; ensure `cfbfastR`, `knitr`, `rmarkdown`, `testthat (>= 3.0.0)` remain.

- [ ] **Step 2: Create `R/reexports.R` with the ggpath re-exports**

```r
#' @importFrom ggpath geom_from_path
#' @export
ggpath::geom_from_path

#' @importFrom ggpath GeomFromPath
#' @export
ggpath::GeomFromPath

#' @importFrom ggpath element_path
#' @export
ggpath::element_path

#' @importFrom ggpath element_raster
#' @export
ggpath::element_raster

#' @importFrom ggpath ggpreview
#' @export
ggpath::ggpreview

#' @importFrom ggpath geom_mean_lines
#' @export
ggpath::geom_mean_lines

#' @importFrom ggpath geom_median_lines
#' @export
ggpath::geom_median_lines

#' @importFrom ggpath GeomRefLines
#' @export
ggpath::GeomRefLines
```

- [ ] **Step 3: Delete the now-redundant generic source files**

```bash
git rm R/geom_from_path.R R/ggpreview.R R/geom_lines.R R/build_grobs.R
```
(`element_path` currently lives in `R/theme-elements.R` and is handled in Task 6; `build_grobs`/`axisImageGrob` deletion is completed across Tasks 3–6 — this step removes the standalone files.)

- [ ] **Step 4: Regenerate docs/NAMESPACE**

Run: `Rscript -e "roxygen2::roxygenise()"` (write to a temp file if inline fails under Git Bash).
Expected: `NAMESPACE` now has `export(geom_from_path)`, `export(element_path)`, `export(ggpreview)`, `export(geom_mean_lines)`, `export(geom_median_lines)`, `export(element_raster)`, `export(GeomFromPath)`, `export(GeomRefLines)` plus `importFrom(ggpath,...)` lines; the old `export(GeomRefLines)`/`geom_mean_lines` from cfbplotR's deleted files are now sourced from ggpath. No errors about missing `geom_from_path`/`ggpreview` definitions (they come from ggpath).

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION NAMESPACE R/reexports.R
git commit -m "refactor!: depend on ggpath; re-export generics, drop own image machinery"
```

---

## Task 2: Internal CFB path-resolution helpers

**Files:**
- Modify: `R/utils.R`
- Test: `tests/testthat/test-resolvers.R`

These replace the resolution half of the deleted `build_grobs()`/`axisImageGrob()`. They return a character vector of image paths/URLs; ggpath does the rendering.

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-resolvers.R
test_that("logo_from_school resolves valid + invalid teams", {
  out <- cfbplotR:::logo_from_school(c("Alabama", "Georgia"))
  expect_type(out, "character")
  expect_length(out, 2)
  expect_false(any(is.na(out)))
  # invalid falls back to the NCAA logo, with a warning
  expect_warning(bad <- cfbplotR:::logo_from_school("Not A Team"))
  expect_equal(bad, cfbplotR:::logo_from_school("NCAA"))
})

test_that("headshot_from_id builds ESPN urls", {
  out <- cfbplotR:::headshot_from_id("4361182")
  expect_match(out, "espncdn\\.com")
})
```

- [ ] **Step 2: Run to confirm failure**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-resolvers.R')"`
Expected: FAIL — `logo_from_school`/`headshot_from_id` not found.

- [ ] **Step 3: Implement the resolvers in `R/utils.R`**

```r
# Resolve CFB team names to logo image paths (vectorised). Invalid names warn
# and fall back to the generic NCAA logo. ggpath renders the returned paths.
logo_from_school <- function(team) {
  team <- clean_school_names(as.character(team))
  valid <- valid_team_names()
  bad <- !is.na(team) & !(team %in% valid)
  if (any(bad)) {
    cli::cli_warn("{.val {unique(team[bad])}} is/are not valid team name(s); using the NCAA logo.")
    team[bad] <- "NCAA"
  }
  unname(vapply(team, function(t) if (is.na(t)) NA_character_ else logo_list[[t]], character(1)))
}

# Resolve CFB team names to wordmark image paths (vectorised).
wordmark_from_school <- function(team) {
  team <- clean_school_names(as.character(team))
  bad <- !is.na(team) & !(team %in% names(wordmark_list))
  if (any(bad)) {
    cli::cli_warn("{.val {unique(team[bad])}} do(es) not have a wordmark; using the NCAA wordmark.")
    team[bad] <- "NCAA"
  }
  unname(vapply(team, function(t) if (is.na(t)) NA_character_ else wordmark_list[[t]], character(1)))
}

# Build ESPN headshot URLs from player ids (vectorised). ggpath fetches/caches.
headshot_from_id <- function(player_id) {
  player_id <- as.character(player_id)
  url <- headshot_id_to_url(player_id)
  url[is.na(player_id)] <- NA_character_
  url
}
```
(`headshot_id_to_url`, `logo_list`, `wordmark_list`, `clean_school_names`, `valid_team_names` already exist. The previous live `RCurl::url.exists()` per-id check is intentionally dropped — ggpath handles a missing image gracefully, and the check made plots slow + network-bound.)

- [ ] **Step 4: Run tests to confirm pass**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-resolvers.R')"`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
git add R/utils.R tests/testthat/test-resolvers.R
git commit -m "feat: add ggpath-friendly CFB path resolvers (logo/wordmark/headshot)"
```

---

## Task 3: Migrate `geom_cfb_logos` to ggpath delegation

**Files:**
- Modify: `R/geom_cfb_logos.R:119-129` (the `draw_panel`)
- Test: `tests/testthat/test-geom-cfb-logos.R` (vdiffr)

- [ ] **Step 1: Replace `GeomCFBlogo$draw_panel`**

Replace the `draw_panel`/`draw_key` body (currently the `build_grobs` lapply) with:
```r
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data$path <- logo_from_school(data$team)
    ggpath::GeomFromPath$draw_panel(
      data = data, panel_params = panel_params, coord = coord, na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
```
Keep `required_aes = c("x", "y", "team")` and `default_aes` unchanged so the public aesthetics (`alpha`, `colour`, `angle`, `hjust`, `vjust`, `width`, `height`) still work — ggpath's `GeomFromPath` consumes the same aesthetics.

- [ ] **Step 2: Add a vdiffr snapshot test**

```r
# tests/testthat/test-geom-cfb-logos.R
test_that("geom_cfb_logos renders", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(a = 1:3, b = 1, teams = c("Alabama", "Georgia", "Oregon"))
  p <- ggplot(df, aes(a, b)) + geom_cfb_logos(aes(team = teams), width = 0.1) + theme_void()
  vdiffr::expect_doppelganger("geom_cfb_logos-basic", p)
})
```

- [ ] **Step 3: Verify load + snapshot**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-geom-cfb-logos.R')"`
Expected: PASS (first run writes the snapshot). Manually eyeball the generated `tests/testthat/_snaps/geom-cfb-logos/geom-cfb-logos-basic.svg` shows three logos.

- [ ] **Step 4: Commit**

```bash
git add R/geom_cfb_logos.R tests/testthat/test-geom-cfb-logos.R tests/testthat/_snaps/geom-cfb-logos
git commit -m "refactor(geom): render geom_cfb_logos via ggpath"
```

---

## Task 4: Migrate `geom_cfb_wordmarks` to ggpath delegation

**Files:**
- Modify: `R/geom_cfb_wordmarks.R` (the `GeomCFBwordmark$draw_panel`)
- Test: `tests/testthat/test-geom-cfb-wordmarks.R`

- [ ] **Step 1: Replace `GeomCFBwordmark$draw_panel`** with:
```r
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data$path <- wordmark_from_school(data$team)
    ggpath::GeomFromPath$draw_panel(
      data = data, panel_params = panel_params, coord = coord, na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
```
Keep `required_aes`/`default_aes` unchanged.

- [ ] **Step 2: Snapshot test**
```r
# tests/testthat/test-geom-cfb-wordmarks.R
test_that("geom_cfb_wordmarks renders", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(a = 1:2, b = 1, teams = c("Oregon", "UCLA"))
  p <- ggplot(df, aes(a, b)) + geom_cfb_wordmarks(aes(team = teams), width = 0.2) + theme_void()
  vdiffr::expect_doppelganger("geom_cfb_wordmarks-basic", p)
})
```

- [ ] **Step 3: Verify** — `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-geom-cfb-wordmarks.R')"` → PASS.

- [ ] **Step 4: Commit**
```bash
git add R/geom_cfb_wordmarks.R tests/testthat/test-geom-cfb-wordmarks.R tests/testthat/_snaps/geom-cfb-wordmarks
git commit -m "refactor(geom): render geom_cfb_wordmarks via ggpath"
```

---

## Task 5: Migrate `geom_cfb_headshots` to ggpath delegation

**Files:**
- Modify: `R/geom_cfb_headshots.R` (the `GeomCFBheads$draw_panel`; required aes is `player_id`)
- Test: `tests/testthat/test-geom-cfb-headshots.R`

- [ ] **Step 1: Replace `GeomCFBheads$draw_panel`** with:
```r
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data$path <- headshot_from_id(data$player_id)
    ggpath::GeomFromPath$draw_panel(
      data = data, panel_params = panel_params, coord = coord, na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
```
Keep `required_aes = c("x", "y", "player_id")` and `default_aes` unchanged.

- [ ] **Step 2: Snapshot test** (network-dependent → guard)
```r
# tests/testthat/test-geom-cfb-headshots.R
test_that("geom_cfb_headshots renders", {
  skip_on_cran(); skip_if_offline(); skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(a = 1:2, b = 1, id = c("4361182", "4426385"))
  p <- ggplot(df, aes(a, b)) + geom_cfb_headshots(aes(player_id = id), width = 0.15) + theme_void()
  vdiffr::expect_doppelganger("geom_cfb_headshots-basic", p)
})
```

- [ ] **Step 3: Verify** — `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-geom-cfb-headshots.R')"` → PASS (or skip if offline).

- [ ] **Step 4: Commit**
```bash
git add R/geom_cfb_headshots.R tests/testthat/test-geom-cfb-headshots.R tests/testthat/_snaps/geom-cfb-headshots
git commit -m "refactor(geom): render geom_cfb_headshots via ggpath"
```

---

## Task 6: Migrate theme elements to ggpath delegation

**Files:**
- Modify: `R/theme-elements.R` (replace the three `element_grob.*` methods + delete `element_path` constructor [now re-exported from ggpath in Task 1], `axisImageGrob`, `grobHeight.axisImageGrob`, `grobWidth.axisImageGrob`)
- Test: `tests/testthat/test-elements.R`

The element constructors `element_cfb_logo`/`element_cfb_wordmark`/`element_cfb_headshot` stay (lines 135–166). The migration rewrites each `element_grob.*` to resolve id → path, re-class to ggpath's `element_path`, and delegate — exactly nbaplotR's pattern.

- [ ] **Step 1: Delete cfbplotR's own `element_path()` constructor + `element_grob.element_path`**

Remove lines 169–177 (`element_path` constructor) and 302–341 (`element_grob.element_path`) — both now come from ggpath via the Task 1 re-export. Also delete `axisImageGrob` (343–430) and `grobHeight.axisImageGrob`/`grobWidth.axisImageGrob` (432–441).

- [ ] **Step 2: Replace `element_grob.element_cfb_logo`** with the resolve→re-class→delegate form:
```r
#' @export
#' @rdname element
element_grob.element_cfb_logo <- function(element, label = "", x = NULL, y = NULL,
                                          alpha = NULL, colour = NULL,
                                          hjust = NULL, vjust = NULL,
                                          size = NULL, ...) {
  if (is.null(label)) return(ggplot2::zeroGrob())
  label <- logo_from_school(label)
  class(element) <- c("element_path", "element_text", "element")
  ggplot2::element_grob(
    element, label = label, x = x, y = y, alpha = alpha, colour = colour,
    hjust = hjust, vjust = vjust, size = size, ...
  )
}
```

- [ ] **Step 3: Replace `element_grob.element_cfb_wordmark`** identically but with `label <- wordmark_from_school(label)`.

- [ ] **Step 4: Replace `element_grob.element_cfb_headshot`** identically but with `label <- headshot_from_id(label)`.

- [ ] **Step 5: Snapshot test**
```r
# tests/testthat/test-elements.R
test_that("element_cfb_logo axis renders", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(teams = c("Alabama","Georgia","Oregon"), v = c(1,2,3))
  p <- ggplot(df, aes(teams, v)) + geom_col() + theme_minimal() +
    theme(axis.text.x = element_cfb_logo())
  vdiffr::expect_doppelganger("element_cfb_logo-axis", p)
})
```

- [ ] **Step 6: Verify load + snapshot + that re-exported `element_path` still works**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-elements.R'); stopifnot(is.function(element_path))"`
Expected: PASS; `element_path` resolves (from ggpath).

- [ ] **Step 7: Regenerate docs + commit**
```bash
Rscript -e "roxygen2::roxygenise()"
git add R/theme-elements.R NAMESPACE man tests/testthat/test-elements.R tests/testthat/_snaps/elements
git commit -m "refactor(elements): delegate element_cfb_* axis grobs to ggpath"
```

---

## Task 7: gt logo/headshot/wordmark formatters via gt-native image embedding

**Files:**
- Modify: `R/gt_fmt_cfb.R` (replace `base64enc`/`magick` embedding with gt's image helpers)
- Test: `tests/testthat/test-gt.R`

cfbplotR's `gt_fmt_cfb_logo`/`gt_fmt_cfb_headshot`/`gt_fmt_cfb_wordmark` currently base64-encode via `magick`. Rework to resolve id → path (Task 2 resolvers) and embed with `gt::web_image()`/`gt::local_image()` (mirrors `gt_nfl_logos`). This is what lets `base64enc`/`magick` leave Imports.

- [ ] **Step 1: Rewrite `gt_fmt_cfb_logo`** to use `gt::text_transform` + `gt::web_image`:
```r
gt_fmt_cfb_logo <- function(gt_object, columns, height = 30, ...) {
  gt::text_transform(
    gt_object,
    locations = gt::cells_body(columns = {{ columns }}),
    fn = function(x) {
      paths <- logo_from_school(x)
      gt::web_image(url = paths, height = height)
    }
  )
}
```
Apply the same shape to `gt_fmt_cfb_wordmark` (`wordmark_from_school`) and `gt_fmt_cfb_headshot` (`headshot_from_id`). Preserve each function's existing exported signature/args where they differ; only swap the embedding internals.

- [ ] **Step 2: Test (data-level — the transform returns html)**
```r
# tests/testthat/test-gt.R
test_that("gt_fmt_cfb_logo embeds an <img>", {
  skip_if_not_installed("gt")
  tab <- gt::gt(data.frame(team = c("Alabama","Georgia")))
  out <- gt_fmt_cfb_logo(tab, team)
  html <- gt::as_raw_html(out)
  expect_match(html, "<img")
})
```

- [ ] **Step 3: Verify** — `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-gt.R')"` → PASS.

- [ ] **Step 4: Commit**
```bash
git add R/gt_fmt_cfb.R tests/testthat/test-gt.R
git commit -m "refactor(gt): embed cfb images via gt::web_image (drop magick/base64enc)"
```

---

## Task 8: Parity conveniences

**Files:**
- Modify: `R/cleaning_helpers.R` (add `clean_team_abbrs`), `R/cfb_team_tiers.R` (add `cfb_team_factor`), `R/utils.R` (add `.cfbplotR_clear_cache`), `R/gt_fmt_cfb.R` (add `gt_cfb_cols_label`)
- Test: `tests/testthat/test-conveniences.R`

- [ ] **Step 1: Failing tests**
```r
# tests/testthat/test-conveniences.R
test_that("clean_team_abbrs aliases clean_school_names", {
  expect_equal(clean_team_abbrs("Bama"), clean_school_names("Bama"))
})
test_that("cfb_team_factor returns an ordered factor of valid teams", {
  f <- cfb_team_factor(c("Georgia","Alabama"))
  expect_s3_class(f, "factor")
  expect_true(all(levels(f) %in% valid_team_names()))
})
test_that(".cfbplotR_clear_cache runs", {
  expect_invisible(.cfbplotR_clear_cache())
})
```

- [ ] **Step 2: Run to confirm failure** — `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-conveniences.R')"` → FAIL (not found).

- [ ] **Step 3: Implement**

`R/cleaning_helpers.R`:
```r
#' Clean CFB team abbreviations
#' @description Alias of [clean_school_names()] for naming parity with the
#'   nflplotR/nbaplotR family.
#' @inheritParams clean_school_names
#' @return A character vector of cleaned team names.
#' @export
clean_team_abbrs <- function(...) clean_school_names(...)
```
`R/cfb_team_tiers.R`:
```r
#' Order CFB teams as a factor
#' @param teams character vector of team names.
#' @return An ordered `factor` of the cleaned team names.
#' @export
cfb_team_factor <- function(teams) {
  teams <- clean_school_names(as.character(teams))
  factor(teams, levels = sort(unique(teams[teams %in% valid_team_names()])))
}
```
`R/utils.R`:
```r
#' Clear the cfbplotR (ggpath) image cache
#' @return Invisibly `NULL`.
#' @export
.cfbplotR_clear_cache <- function() {
  if (requireNamespace("ggpath", quietly = TRUE) &&
      "clear_cache" %in% getNamespaceExports("ggpath")) {
    ggpath::clear_cache()
  }
  invisible(NULL)
}
```
`R/gt_fmt_cfb.R`:
```r
#' Render CFB logos in gt column labels
#' @param gt_object a `gt_tbl`.
#' @param ... `<tidy-select>` columns whose labels are team names to replace
#'   with logos.
#' @param height image height in px.
#' @return A `gt_tbl`.
#' @export
gt_cfb_cols_label <- function(gt_object, ..., height = 30) {
  cols <- rlang::ensyms(...)
  for (col in cols) {
    nm <- rlang::as_string(col)
    img <- gt::web_image(url = logo_from_school(nm), height = height)
    gt_object <- gt::cols_label(gt_object, !!nm := gt::html(img))
  }
  gt_object
}
```

- [ ] **Step 4: Verify** — re-run the test file → PASS (3 tests). Confirm `.cfbplotR_clear_cache` exports despite the leading dot (roxygen `@export` handles it).

- [ ] **Step 5: Regenerate docs + commit**
```bash
Rscript -e "roxygen2::roxygenise()"
git add R/cleaning_helpers.R R/cfb_team_tiers.R R/utils.R R/gt_fmt_cfb.R NAMESPACE man tests/testthat/test-conveniences.R
git commit -m "feat: add family-parity conveniences (clean_team_abbrs, cfb_team_factor, gt_cfb_cols_label, .cfbplotR_clear_cache)"
```

---

## Task 9: Re-platform `ggtitle_image`/`theme_title_image` onto ggpath

**Files:**
- Modify: `R/ggtitle_image.R`

These cfbplotR extras build a title from an image path. Ensure they construct the grob via the re-exported `element_path`/ggpath rather than the deleted `axisImageGrob`.

- [ ] **Step 1: Audit** — `grep -nE "axisImageGrob|build_grobs|magick|base64enc|reader_function" R/ggtitle_image.R`. For each hit, replace the image-building call with `element_path()`-based construction (the element returns a grob via `ggplot2::element_grob()`), or `ggpath::geom_from_path` semantics for inline title images.

- [ ] **Step 2: Implement the replacement** so `theme_title_image()` sets the relevant theme element to `element_path(...)` and `ggtitle_image()` injects the image URL as the label consumed by that element. (If `ggtitle_image` already only stores a URL + label and relies on `theme_title_image`'s element, no grob code remains — just confirm it points at the re-exported `element_path`.)

- [ ] **Step 3: Snapshot test**
```r
# tests/testthat/test-ggtitle-image.R
test_that("theme_title_image builds", {
  skip_on_cran(); skip_if_offline(); skip_if_not_installed("vdiffr")
  library(ggplot2)
  url <- "https://raw.githubusercontent.com/sportsdataverse/cfbplotR/main/man/figures/logo.png"
  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point() +
    labs(title = url) + theme_title_image()
  vdiffr::expect_doppelganger("theme-title-image", p)
})
```

- [ ] **Step 4: Verify** — `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-ggtitle-image.R')"` → PASS/skip.

- [ ] **Step 5: Commit**
```bash
git add R/ggtitle_image.R tests/testthat/test-ggtitle-image.R tests/testthat/_snaps/ggtitle-image
git commit -m "refactor: build ggtitle_image/theme_title_image on ggpath element_path"
```

---

## Task 10: Final Part-1 cleanup — deps, docs, CI pins, check

**Files:**
- Modify: `DESCRIPTION` (confirm), `.github/workflows/pkgdown.yaml`, `.github/workflows/R-CMD-check.yaml`

- [ ] **Step 1: Confirm no stray references to removed packages**

Run: `grep -rnE "magick::|RCurl::|base64enc::|purrr::" R/` → expect **no output**. If any remain, replace (magick→ggpath; RCurl→drop; base64enc→gt::web_image; purrr→base `lapply`/`vapply`).

- [ ] **Step 2: Revert the libtiff workaround pins now that magick is gone**

In `.github/workflows/pkgdown.yaml` and `.github/workflows/R-CMD-check.yaml`, change the `ubuntu-22.04` runner(s) added earlier back to `ubuntu-latest`, and remove the explanatory libtiff comments. (magick was the only consumer of the jammy `libtiff`/`libMagick++` binary.)

- [ ] **Step 3: `usethis::use_tidy_description()` + document**

Run: `Rscript -e "usethis::use_tidy_description(); roxygen2::roxygenise()"`

- [ ] **Step 4: Full check**

Run: `Rscript -e "devtools::check(args = c('--no-manual'), error_on = 'warning')"`
Expected: `0 errors | 0 warnings | <=N notes`. Investigate/fix any error or warning (common: an example still referencing a deleted function; a man page for a deleted topic — delete the stale `man/*.Rd`).

- [ ] **Step 5: Commit**
```bash
git add DESCRIPTION NAMESPACE man .github/workflows
git commit -m "chore: finalize ggpath migration; revert libtiff runner pins; tidy deps"
```

---

# PART 2 — Repo professionalization (after Part 1 is green)

> Mirror the SDV standard already applied to oddsapiR this session. Reference siblings: `c:/Users/saiem/Documents/GitHub-Data/sdv-dev/oddsapiR-dev/oddsapiR` and `c:/Users/saiem/Documents/GitHub-Data/sdv-dev/cfbfastR-dev/cfbfastR`. cfbplotR is **GitHub-only** (no CRAN badge).

## Task 11: CLAUDE.md

**Files:** Create `CLAUDE.md`

- [ ] **Step 1: Write `CLAUDE.md`** with a doctoc-marker TOC and these sections: Package Overview (cfbplotR = CFB ggplot2 logo/headshot/wordmark plotting built on ggpath; version 0.1.0 dev; GitHub-only); Architecture (resolve CFB id → path → `ggpath::GeomFromPath`/`element_path`; ggpath owns rendering/caching); Function families (`geom_cfb_*`, `element_cfb_*`, `scale_*_cfb`, `scale_x/y_cfb`, `gt_fmt_cfb_*`, `cfb_team_tiers`/`cfb_team_factor`, cleaning helpers, `ggtitle_image`); the ggpath **re-export** pattern (`R/reexports.R`); CFB team data lives in `R/sysdata.rda` (`logo_list`/`wordmark_list`); ESPN headshot URL building (`headshot_id_to_url`); Build/Dev commands (`devtools::document()/test()/check()`, `pkgdown::build_site()`); Testing (vdiffr snapshots + data unit tests); Docs maintenance (doctoc on README/CONTRIBUTING/CLAUDE/PR template; `devtools::build_readme()`; `usethis::use_tidy_description()`); Commit convention (Conventional Commits); **no AI co-author trailers**.

- [ ] **Step 2: Generate the TOC** — `Rscript tools/run_doctoc.R --maxlevel 2 CLAUDE.md` (copy `tools/run_doctoc.R` from a sibling such as wehoop if cfbplotR lacks it; otherwise run the npm `doctoc` equivalent). Verify idempotent (second run = no diff).

- [ ] **Step 3: Commit** — `git add CLAUDE.md tools/run_doctoc.R; git commit -m "docs: add CLAUDE.md development guide"`

## Task 12: Copilot instructions

**Files:** Create `.github/copilot-instructions.md`

- [ ] **Step 1: Write** a condensed mirror of CLAUDE.md (same architecture/conventions, shorter), with a doctoc TOC.
- [ ] **Step 2: TOC** — `Rscript tools/run_doctoc.R --maxlevel 2 .github/copilot-instructions.md`.
- [ ] **Step 3: Commit** — `git commit -am "docs: add Copilot instructions"`

## Task 13: Issue templates

**Files:** Create `.github/ISSUE_TEMPLATE/bug_report.md`, `.github/ISSUE_TEMPLATE/feature_request.md`, `.github/ISSUE_TEMPLATE/config.yml`

- [ ] **Step 1:** Copy the three files from a sibling (`oddsapiR/.github/ISSUE_TEMPLATE/`), then sed-replace `oddsapiR`→`cfbplotR` and the repo URL. `bug_report.md`: front-matter (name/about/labels: bug) + sections "Describe the bug / Reprex / Expected / `sessionInfo()` / Screenshots". `feature_request.md`: front-matter (labels: enhancement) + "Problem / Desired solution / Alternatives / Context". `config.yml`: `blank_issues_enabled: false` + a contact link to the SportsDataverse Discord/discussions.
- [ ] **Step 2: Commit** — `git add .github/ISSUE_TEMPLATE; git commit -m "docs: add issue templates"`

## Task 14: Pull request template

**Files:** Create `.github/pull_request_template.md`

- [ ] **Step 1:** Copy from oddsapiR; sed names/URLs. Sections: Summary, Type of change (checklist), Checklist (`devtools::document()`, tests pass, NEWS updated), Related issues. Include doctoc markers.
- [ ] **Step 2: TOC** — `Rscript tools/run_doctoc.R --maxlevel 2 .github/pull_request_template.md`.
- [ ] **Step 3: Commit** — `git add .github/pull_request_template.md; git commit -m "docs: add pull request template"`

## Task 15: CONTRIBUTING.md

**Files:** Create `CONTRIBUTING.md`

- [ ] **Step 1:** Copy from a sibling; sed names/URLs; tailor: branch/PR workflow, `devtools::document()/test()/check()` before PR, Conventional Commits, vdiffr snapshot workflow, the no-AI-coauthor rule, doctoc TOC.
- [ ] **Step 2: TOC** — `Rscript tools/run_doctoc.R --maxlevel 2 CONTRIBUTING.md`.
- [ ] **Step 3: Commit** — `git add CONTRIBUTING.md; git commit -m "docs: add CONTRIBUTING guide"`

## Task 16: CODE_OF_CONDUCT.md

**Files:** Create `CODE_OF_CONDUCT.md`

- [ ] **Step 1:** Run `Rscript -e "usethis::use_code_of_conduct(contact = 'saiem.gilani@gmail.com')"` (Contributor Covenant). This also adds the CoC link to README — accept it (or fold into the README rebuild in Task 18).
- [ ] **Step 2: Commit** — `git add CODE_OF_CONDUCT.md; git commit -m "docs: add Contributor Covenant code of conduct"`

## Task 17: Badges via usethis

**Files:** Modify `README.Rmd` (badges block)

- [ ] **Step 1:** Between `<!-- badges: start -->` / `<!-- badges: end -->`, run usethis badge helpers so they also populate the pkgdown sidebar:
```r
Rscript -e 'usethis::use_lifecycle_badge("experimental")'
```
Then ensure these badges are present (hand-add the shields URLs usethis doesn't generate, matching oddsapiR's style): R-package version, R-CMD-check workflow status, pkgdown deploy status, **r-universe** (`https://sportsdataverse.r-universe.dev/badges/cfbplotR`), contributors, Twitter (maintainer + `@SportsDataverse`). Keep the CRAN version/downloads badges **commented out** (uncomment on first CRAN release).
- [ ] **Step 2:** Re-render is done in Task 18; for now verify the badge block has no duplicate/stale entries (remove the old hand-written `Kazink36` commented badges at lines 25–26).
- [ ] **Step 3: Commit** — `git add README.Rmd; git commit -m "docs(badges): normalize badges via usethis"`

## Task 18: README rebuild (SDV sections)

**Files:** Modify `README.Rmd`; regenerate `README.md`

- [ ] **Step 1:** Restructure `README.Rmd` body to the SDV section set (mirror oddsapiR): logo + badges; one-paragraph description ("built on ggpath"); Installation (pak/devtools/local); Usage (3 runnable chunks: `geom_cfb_logos`, axis `element_cfb_logo` via `scale_x_cfb`, `gt_fmt_cfb_logo`); Documentation (pkgdown link); **SportsDataverse package-network table** (copy the canonical table from oddsapiR's README — already network-swept this session); Our Authors; Citations (BibTeX block matching `inst/CITATION`); Follow/star.
- [ ] **Step 2: Render** — `Rscript -e "devtools::build_readme()"`. Verify `README.md` regenerates without error and the chunks evaluate (network chunks may need `eval` guards/`\donttest`-style `try()`).
- [ ] **Step 3: Commit** — `git add README.Rmd README.md man/figures; git commit -m "docs(readme): adopt standard SDV sections + citations/links"`

## Task 19: inst/CITATION → bibentry

**Files:** Modify `inst/CITATION`

- [ ] **Step 1:** Replace with a `bibentry()` (drop any deprecated `citEntry`), self-dating from `meta$Date`/build year, authors from `DESCRIPTION`, URL = pkgdown site — mirror oddsapiR's `inst/CITATION`.
- [ ] **Step 2: Verify** — `Rscript -e "print(utils::readCitationFile('inst/CITATION', meta = list(Version='0.1.0')))"` runs without error.
- [ ] **Step 3: Commit** — `git add inst/CITATION; git commit -m "docs: modernize CITATION to bibentry()"`

## Task 20: Rich pkgdown reference

**Files:** Modify `_pkgdown.yml`

- [ ] **Step 1:** Rebuild the `reference:` index into organized sections (each with `title` + `desc` + `contents`):
  - **Logos, Wordmarks & Headshots** — `geom_cfb_logos`, `geom_cfb_wordmarks`, `geom_cfb_headshots`, `GeomCFBlogo`, `GeomCFBwordmark`, `GeomCFBheads`
  - **Theme elements** — `element_cfb_logo`, `element_cfb_wordmark`, `element_cfb_headshot`, `element`
  - **Scales & axes** — `scale_color_cfb`, `scale_colour_cfb`, `scale_fill_cfb`, `scale_x_cfb`, `scale_y_cfb`, `scale_x_cfb_headshots`, `scale_y_cfb_headshots`, `theme_x_cfb`, `theme_y_cfb`
  - **gt helpers** — `gt_fmt_cfb_logo`, `gt_fmt_cfb_headshot`, `gt_fmt_cfb_wordmark`, `gt_cfb_cols_label`, `gt_merge_stack_team_color`
  - **Team utilities** — `valid_team_names`, `clean_school_names`, `clean_team_abbrs`, `cfb_team_factor`, `add_athlete_id_col`, `.cfbplotR_clear_cache`
  - **Tiers** — `cfb_team_tiers`
  - **Image titles** — `ggtitle_image`, `theme_title_image`
  - **ggpath re-exports** — `geom_from_path`, `geom_mean_lines`, `geom_median_lines`, `element_path`, `element_raster`, `ggpreview`, `GeomFromPath`, `GeomRefLines`
  Keep the existing (canonicalized) SDV network navbar and Bootstrap-5 template/opengraph/authors.
- [ ] **Step 2:** Ensure the getting-started vignette is in `articles:`; add a short `vignettes/plotting-with-cfbplotR.Rmd` if rich docs are wanted (optional within this task — at minimum list the existing article).
- [ ] **Step 3: Build** — `Rscript -e "pkgdown::build_site()"` → no missing-topic errors (every exported topic is in a section).
- [ ] **Step 4: Commit** — `git add _pkgdown.yml vignettes; git commit -m "docs(pkgdown): rich reference index reflecting ggpath migration"`

## Task 21: Version bump, NEWS, doctoc, final gate

**Files:** Modify `DESCRIPTION`, `NEWS.md`, run doctoc

- [ ] **Step 1: Bump version** — set `Version: 0.1.0` in `DESCRIPTION`.
- [ ] **Step 2: NEWS.md** — add a `# cfbplotR 0.1.0` section: "Rebuilt on the ggpath foundation (the nflplotR/nbaplotR pattern); dropped magick/RCurl/base64enc/purrr; public API preserved via re-exports of ggpath generics; new conveniences `gt_cfb_cols_label`, `cfb_team_factor`, `clean_team_abbrs`, `.cfbplotR_clear_cache`; raised `R (>= 4.1.0)`; added community-health files, badges, SDV README, rich pkgdown."
- [ ] **Step 3: doctoc** — `Rscript tools/run_doctoc.R --maxlevel 2 NEWS.md CLAUDE.md CONTRIBUTING.md .github/copilot-instructions.md .github/pull_request_template.md` (idempotent).
- [ ] **Step 4: Final gate** — `Rscript -e "usethis::use_tidy_description(); devtools::check(args=c('--no-manual'), error_on='warning')"` → 0 errors/0 warnings; `Rscript -e "pkgdown::build_site()"` builds.
- [ ] **Step 5: Commit + push**
```bash
git add DESCRIPTION NEWS.md CLAUDE.md CONTRIBUTING.md .github
git commit -m "chore(release): cfbplotR 0.1.0 — ggpath migration + repo professionalization"
git push origin HEAD:main
```

---

## Final verification (whole plan)

- [ ] `devtools::check()` → 0 errors, 0 warnings.
- [ ] `grep -rnE "magick|RCurl|base64enc|purrr" R/ DESCRIPTION` → no output.
- [ ] `Rscript -e "library(cfbplotR); stopifnot(all(c('geom_from_path','element_path','ggpreview','geom_cfb_logos','gt_cfb_cols_label','cfb_team_factor','clean_team_abbrs') %in% getNamespaceExports('cfbplotR')))"` → no error (API preserved + additions present).
- [ ] CI green on `main`: pkgdown + R-CMD-check (now on `ubuntu-latest`).
- [ ] pkgdown reference index has no missing topics; README renders with the SDV sections; all community-health files present.
