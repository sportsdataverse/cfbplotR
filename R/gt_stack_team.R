#' @name gt_stack_team
#' @title
#' Merge and stack text from two columns in `gt` and color one with school colors
#'
#' @description
#' The `gt_merge_stack_team_color()` function takes an existing `gt` table and merges
#' column 1 and column 2, stacking column 1's text on top of column 2's.
#' Top text is in all caps with black bold text, while the lower text is smaller
#' and colored by the team name in another column.
#' This is a slightly modified version of [`gtExtras::gt_merge_stack()`](https://jthomasmock.github.io/gtExtras/reference/gt_merge_stack.html)  written by Tom Mock.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param col1 The column to stack on top. Will be converted to all caps, with black and bold text.
#' @param col2 The column to merge and place below. Will be smaller and the school color that corresponds to `team_col`.
#' @param team_col The column of team names that match `valid_team_names()` for the color of the bottom.
#' @param font_size_top the font size for the top text.
#' @param font_size_bottom the font size for the bottom text.
#' @param color The color for the top text.
#' @return An object of class `gt_tbl`.
#' @importFrom gt %>%
#' @importFrom glue glue
#' @export
#' @import dplyr
#' @import gt
#' @examples
#' library(gt)
#' teams <- "https://github.com/saiemgilani/cfbfastR-data/raw/master/team_info/rds/cfb_team_info_2020.rds"
#' team_df <- readRDS(url(teams))
#'
#' stacked_tab <- team_df %>%
#'   transmute(logo = school,school,mascot,conference,city,state) %>%
#'   head(8) %>%
#'   gt::gt() %>%
#'   gt_merge_stack_team_color(school,mascot,school) %>%
#'   cfbplotR::gt_fmt_cfb_logo(columns = c("logo","conference"))
#'
#' @section Figures:
#' \if{html}{\figure{merge-stack.png}{options: width=50\%}}
#'

gt_merge_stack_team_color <- function (gt_object, col1, col2, team_col, font_size_top = 14, font_size_bottom = 12, color = "black")
{
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))


  team <- rlang::enexpr(team_col) %>% rlang::as_string()
  team_bare <- gt_object[["_data"]][[team]]
  if(is.null(team_bare)) {
    cli::cli_abort("Must include a column of team names, `team_col` is NULL")
  }
  team_color <- dplyr::left_join(
    data.frame(team = team_bare),
    cfbplotR::logo_ref %>%
      dplyr::select(team = school,color),
    by = "team") %>%
    dplyr::mutate(color = ifelse(is.na(color),"grey",color)) %>%
    dplyr::pull(color)


  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] ==
                                                           "stub")]
  col2_bare <- rlang::enexpr(col2) %>% rlang::as_string()
  data_in <- gt_object[["_data"]][[col2_bare]]
  gt_object %>% text_transform(locations = if (isTRUE(row_name_var ==
                                                      col1_bare)) {
    cells_stub(rows = gt::everything())
  }
  else {
    cells_body(columns = {
      {
        col1
      }
    })
  }, fn = function(x) {
    glue::glue("<div style='line-height:{font_size_top-2}px'><span style='font-weight:bold;font-variant:small-caps;color:{color};font-size:{font_size_top}px'>{x}</div>\n        <div style='line-height:{font_size_bottom-2}px'><span style ='font-weight:bold;color:{team_color};font-size:{font_size_bottom}px'>{data_in}</span></div>")
  }) %>% cols_hide(columns = {
    {
      col2
    }
  })
}
