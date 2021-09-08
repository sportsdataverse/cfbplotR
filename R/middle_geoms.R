#' @name middle_geoms
#' @aliases median_geom mean_geom
#' @title
#' **Plot Middle Lines**
#'
#' @description These functions plot lines representing the median or mean of the data for ggplot. From Thomas Mock (https://github.com/nflverse/nflplotR/issues/1)
#'
#' @param x The X data
#' @param y The Y data
#' @param color The color of the line
#' @param linetype The linetype of the line
#' @export
#'
#'
#' @examples
#' \donttest{
#' ggplot(mtcars) +
#'  geom_point(aes(x = disp, y = mpg)) +
#'  median_geom(disp, mpg)
#'
#'
#' ggplot(mtcars) +
#'  geom_point(aes(x = disp, y = mpg)) +
#'  mean_geom(disp, mpg)
#'  }

median_geom <- function(x,y, color = "red", linetype = "dashed"){
  list(
    ggplot2::geom_vline(ggplot2::aes(xintercept = median({{x}})), color = color, linetype = linetype),
    ggplot2::geom_hline(ggplot2::aes(yintercept = median({{y}})), color = color, linetype = linetype)
  )
}
#' @rdname middle_geoms
#' @param x The X data
#' @param y The Y data
#' @param color The color of the line
#' @param linetype The linetype of the line
#' @export
mean_geom <- function(x,y, color = "red", linetype = "dashed"){
  list(
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean({{x}})), color = color, linetype = linetype),
    ggplot2::geom_hline(ggplot2::aes(yintercept = mean({{y}})), color = color, linetype = linetype)
  )
}


