#' @name scale_color_cfb
#' @aliases scale_color_cfb scale_fill_cfb
#' @title
#' Scale for college football team colors
#'
#' @description These functions allows you to map college football team names as levels to the color and fill aesthetics
#'
#' @param alt_colors Vector of team names to use the alternate color of.
#' @param ... Arguments passed on to scale_color_manual
#' @export
#'
#' @examples
#' library(cfbplotR)
#' library(ggplot2)
#'
#' df <- data.frame(
#'   y = 6:9,
#'   teams = c("Alabama","Florida State","Oregon","Utah")
#'  )
#'  ggplot(df, aes(x = teams, y = y)) +
#'    geom_col(aes(color = teams, fill = teams), size = 2) +
#'    scale_color_cfb(alt_colors = df$teams) +
#'    scale_fill_cfb() +
#'    theme_minimal()


scale_color_cfb <- function(alt_colors = NULL,...) {
  values <-  dplyr::mutate(team_colors, value = ifelse(school %in% alt_colors,alt_color,color))
  values <- dplyr::pull(values, value)
  names(values) <- team_colors$school

  ggplot2::scale_color_manual(values = values,...,guide = NULL)
}

#' @rdname scale_color_cfb
#' @export

scale_fill_cfb <- function(alt_colors = NULL,...) {
  values <-  dplyr::mutate(team_colors, value = ifelse(school %in% alt_colors,alt_color,color))
  values <- dplyr::pull(values, value)
  names(values) <- team_colors$school

  ggplot2::scale_fill_manual(values = values,...,guide = NULL)
}
