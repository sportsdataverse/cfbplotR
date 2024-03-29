#' Preview ggplot in Specified Dimensions
#'
#' This function previews a ggplot in its actual dimensions in order to see how
#' it will look when saved. It is also significantly faster than the default
#' preview in RStudio for ggplots created using cfbplotR. This is copied directly from `nflplotR`
#'
#' @inheritParams ggplot2::ggsave
#' @param asp The aspect ratio of the plot calculated as `width / height`. If
#'   this is a numeric value (and not `NULL`) the `height` of the plot will be
#'   recalculated to `height = width / asp`.
#' @export
#' @examples
#' library(cfbplotR)
#' library(ggplot2)
#'
#' team <- valid_team_names()
#' # remove conference logos from this example
#' team <- team[1:20]
#'
#' df <- data.frame(
#'   random_value = runif(length(team), 0, 1),
#'   teams = team
#' )
#'
#' # use logos for x-axis
#' # note that the plot is assigned to the object "p"
#' p <- ggplot(df, aes(x = teams, y = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_cfb(alt_colors = valid_team_names()) +
#'   scale_fill_cfb(alpha = 0.7) +
#'   scale_x_cfb() +
#'   theme_minimal() +
#'   theme_x_cfb()
#'
#' # preview p with defined width and aspect ratio (only available in RStudio)
#' if (rstudioapi::isAvailable()){
#'   ggpreview(p, width = 5, asp = 16/9)
#' }
ggpreview <- function(plot = ggplot2::last_plot(),
                      width = NA,
                      height = NA,
                      asp = NULL,
                      dpi = 300,
                      device = "png",
                      units = c("in", "cm", "mm", "px"),
                      scale = 1,
                      limitsize = TRUE,
                      bg = NULL,
                      ...){
  rlang::check_installed("rstudioapi", reason = "to preview a ggplot file")
  file <- tempfile()
  if (is.numeric(asp)) height <- width / asp
  ggplot2::ggsave(
    file,
    plot = plot,
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = limitsize,
    bg = bg,
    ...
  )
  rstudioapi::viewer(file)
}
