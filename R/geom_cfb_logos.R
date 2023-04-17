#' ggplot2 Layer for Visualizing CFB Team Logos
#' @rdname geom_cfb_logos
#' @description This geom is used to plot college football team and conference logos instead
#'   of points in a ggplot. It requires x, y aesthetics as well as a valid CFB
#'   team name or abbreviation. The latter can be checked with [`valid_team_names()`].
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_cfb_logos()` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**team**}{ - The team name or abbreviation. Must be one of [`valid_team_names()`].}
#'   \item{`alpha = NULL`}{ - The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{ - The colour of the image. Managed with scale_color_*}
#'   \item{`angle = 0`}{ - The angle of the image as a numerical value between 0° and 360°.}
#'   \item{`hjust = 0.5`}{ - The horizontal adjustment relative to the given x coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`vjust = 0.5`}{ - The vertical adjustment relative to the given y coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`width = 1.0`}{ - The desired width of the image in `npc` (Normalised Parent Coordinates).
#'                           The default value is set to 1.0 which is *big* but it is necessary
#'                           because all used values are computed relative to the default.
#'                           A typical size is `width = 0.075` (see below examples).}
#'   \item{`height = 1.0`}{ - The desired height of the image in `npc` (Normalised Parent Coordinates).
#'                            The default value is set to 1.0 which is *big* but it is necessary
#'                            because all used values are computed relative to the default.
#'                            A typical size is `height = 0.1` (see below examples).}
#' }
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value. See the below
#'   section "Aesthetics" for a full list of possible arguments.
#' @export
#' @examples
#' \donttest{
#' library(cfbplotR)
#' library(ggplot2)
#'
#' team <- valid_team_names()
#' team <- team[1:32]
#'
#' df <- data.frame(
#'   a = rep(1:8, 4),
#'   b = sort(rep(1:4, 8), decreasing = TRUE),
#'   teams = team
#' )
#'
#' # keep alpha == 1 for all teams including an "A"
#' matches <- grepl("A", team)
#' df$alpha <- ifelse(matches, 1, 0.2)
#'
#' # scatterplot of all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_cfb_logos(aes(team = teams), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' # apply alpha via an aesthetic from inside the dataset `df`
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_cfb_logos(aes(team = teams, alpha = alpha), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_alpha_identity() +
#'   theme_void()
#'
#' # apply alpha as constant for all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_cfb_logos(aes(team = teams), width = 0.075, alpha = 0.6) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' # it's also possible to plot conference logos
#' conf <- data.frame(a = 1:2, b = 0, teams = c("Pac-12", "ACC"))
#' ggplot(conf, aes(x = a, y = b)) +
#'   geom_cfb_logos(aes(team = teams), width = 0.3) +
#'   geom_label(aes(label = teams), nudge_y = -0.4, alpha = 0.5) +
#'   coord_cartesian(xlim = c(0.5,2.5), ylim = c(-0.75,.75)) +
#'   theme_void()
#'
#'
#' # it's also possible to color the logos
#' # make teams with an A red
#' df$color <- ifelse(matches, "red", NA)
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_cfb_logos(aes(team = teams, color = color), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_color_identity() +
#'   theme_void()
#'}
#'
geom_cfb_logos <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCFBlogo,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
#' @rdname geom_cfb_logos
#' @export
GeomCFBlogo <- ggplot2::ggproto(
  "GeomCFBlogo", ggplot2::Geom,
  required_aes = c("x", "y", "team"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(seq_along(data$team), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "teams")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)










