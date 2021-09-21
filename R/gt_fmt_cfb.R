#' Add logos into rows of a `gt` table
#' @description
#' The `gt_fmt_cfb` function takes an existing `gt_tbl` object and
#' converts college football team names from `valid_team_names()` into inline team logos. This is a wrapper around [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html) written by Tom Mock, which is a wrapper
#' around `gt::text_transform()` + `gt::web_image()`/`gt::local_image()` with
#' the necessary boilerplate already applied.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param columns The columns wherein changes to cell data colors should occur.
#' @inheritParams gt::web_image
#' @inheritParams gt::local_image
#' @return An object of class `gt_tbl`.
#' @importFrom gt %>%
#' @export

#' @examples
#' library(gt)
#' library(cfbplotR)
#'
#'df <- data.frame(team = valid_team_names()[1:8],logo = valid_team_names()[1:8])
#'
#'table <- df %>%
#'  gt() %>%
#'  gt_fmt_cfb(columns = "logo")
#'
#' @section Figures:
#' \if{html}{\figure{fmt_cfb.png}{options: width=30\%}}
#'


gt_fmt_cfb <- function(gt_object, columns, height = 30){

  # convert tidyeval column to bare string
  column_names <- gt:::resolve_cols_c(
    expr = {{ columns }},
    data = gt_object
  )

  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="row_group")]

  # stopifnot("img_source must be 'web' or 'local'" = img_source %in% c("web", "local"))
  img_source <- "web"

  # need to correct for rownames
  gt_object %>%
    gt::text_transform(
      locations = if(isTRUE(grp_var %in% column_names)){
        gt::cells_row_groups()
      } else if(isTRUE(stub_var %in% column_names)){
        gt::cells_stub(rows = gt::everything())
      } else {
        gt::cells_body({{ columns }})
      },
      fn = function(x){
        if(img_source == "web"){
          #Fix Texas A&M
          x <- str_replace(x,"&amp;","&")
          x[which(!x%in%valid_team_names())] <- "NCAA"
          gt::web_image(url = logo_list[x], height = height)
        } else {
          gt::local_image(filename = x, height = height)
        }
      }
    )

}
