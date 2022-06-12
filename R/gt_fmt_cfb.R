#' @name gt_cfb
#' @title
#' Add logos into rows of a `gt` table
#' @description
#' The `gt_fmt_cfb_logo` and `gt_fmt_cfb_headshot` functions take an existing `gt_tbl` object and
#' converts college football team names from `valid_team_names()` into inline team logos or ESPN
#' player ID's (or `headshot_url` from `cfbfastR::cfbd_team_rosters()` function) into inline
#' player headshots. This is a wrapper around
#' [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
#' written by Tom Mock, which is a wrapper around `gt::text_transform()` + `gt::web_image()`/
#' `gt::local_image()` with the necessary boilerplate already applied.
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
#' df <- data.frame(team = valid_team_names()[1:8],
#'                  logo = valid_team_names()[1:8],
#'                  wordmark = valid_team_names()[1:8])
#'
#' table <- df %>%
#'  gt() %>%
#'  gt_fmt_cfb_logo(columns = "logo") %>%
#'  gt_fmt_cfb_wordmark(columns = "wordmark")
#'
#'df <- data.frame(
#'  player = c("Britain Covey", "Cameron Rising","Non.Match"),
#'  team = c("Utah","Utah","BYU")
#') %>%
#'  cfbplotR::add_athlete_id_col(player)
#'table_2 <- df %>%
#'  gt() %>%
#'  gt_fmt_cfb_headshot(athlete_id)
#'
#' @section Figures:
#' \if{html}{\figure{fmt_cfb.png}{options: width=30\%}}
#' \if{html}{\figure{fmt_cfb_2.png}{options: width=30\%}}
#'


gt_fmt_cfb_logo <- function(gt_object, columns, height = 30){

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
          x <- sub("&amp;","&",x)
          x <- cfbplotR::clean_school_names(as.character(x))
          x[which(!x%in%valid_team_names())] <- "NCAA"
          gt::web_image(url = logo_list[x], height = height)
        } else {
          gt::local_image(filename = x, height = height)
        }
      }
    )

}


#' @rdname gt_cfb
#' @export

gt_fmt_cfb_wordmark <- function(gt_object, columns, height = 30) {

  column_names <- gt:::resolve_cols_c(
    expr = {{ columns }},
    data = gt_object
  )

  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="row_group")]

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
        #Fix Texas A&M
        x <- sub("&amp;","&",x)
        x <- cfbplotR::clean_school_names(as.character(x))
        x[which(!x%in%names(wordmark_list))] <- "NCAA"
        gt::web_image(url = wordmark_list[x], height = height)
      }
    )

}


#' @rdname gt_cfb
#' @export

gt_fmt_cfb_headshot <- function(gt_object, columns, height = 30) {

  column_names <- gt:::resolve_cols_c(
    expr = {{ columns }},
    data = gt_object
  )

  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="row_group")]

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
        for (i in 1:length(x)) {
          url_or_id <- headshot_id_or_url(x[i])
          if(url_or_id == "ID") {
            x[i] <- headshot_id_to_url(x[i])
          } else if(url_or_id == "NA"){
            x[i] <- "http://a.espncdn.com/i/headshots/nophoto.png"
          }
        }

        if (is.numeric(height)) {
          height <- paste0(height, "px")
        }

        x <- paste0("<img src=\"", x, "\" style=\"height:", height,
                    ";\" onerror=\"this.onerror=null;this.src='http://a.espncdn.com/i/headshots/nophoto.png';\" />")
      }
    )

}



