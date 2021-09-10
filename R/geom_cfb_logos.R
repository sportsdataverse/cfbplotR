#' ggplot2 Layer for Visualizing CFB Team Logos
#'
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
    geom = GeomCFB,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomCFB <- ggplot2::ggproto(
  "GeomCFB", ggplot2::Geom,
  required_aes = c("x", "y", "team"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0,
    colour = NULL
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(seq_along(data$team), function(i, urls, alpha, colour, data) {
      team <- data$team[i]

      if (!is.null(colour)) {
        if (!is.na(colour[i])) {
          img <- magick::image_read(logo_list[[team]])
          new <- color_image(img,color = colour[i],alpha = alpha[i])

          grid <- grid::rasterGrob(new)
        } else if (is.null(alpha)) {
          grid <- grid::rasterGrob(magick::image_read(logo_list[[team]]))
        } else if (length(alpha) == 1L) {
          if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
            cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
          }
          img <- magick::image_read(logo_list[[team]])
          new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
          grid <- grid::rasterGrob(new)
        } else {
          if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
            cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
          }
          img <- magick::image_read(logo_list[[team]])
          new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
          grid <- grid::rasterGrob(new)
        }
      } else if (is.null(alpha)) {
        grid <- grid::rasterGrob(magick::image_read(logo_list[[team]]))
      } else if (length(alpha) == 1L) {
        if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
          cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
        }
        img <- magick::image_read(logo_list[[team]])
        new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
        grid <- grid::rasterGrob(new)
      } else {
        if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
          cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
        }
        img <- magick::image_read(logo_list[[team]])
        new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
        grid <- grid::rasterGrob(new)
      }


      grid$vp <- grid::viewport(
        x = grid::unit(data$x[i], "native"),
        y = grid::unit(data$y[i], "native"),
        width = grid::unit(data$width[i], "npc"),
        height = grid::unit(data$height[i], "npc"),
        just = c(data$hjust[i], data$vjust[i]),
        angle = data$angle[i],
        name = paste("geom_cfb.panel", data$PANEL[i],
                     "row", i,
                     sep = "."
        )
      )

      grid$name <- paste("cfb.grob", i, sep = ".")

      grid
    }, urls = urls, alpha = data$alpha,colour = data$colour, data = data)

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)



color_image <- function(img, color, alpha = NULL) {
  if (is.null(color))
    return(img)

  if (length(color) > 1) {
    stop("color should be a vector of length 1")
  }

  bitmap <- img[[1]]
  col <- col2rgb(color)
  bitmap[1,,] <- as.raw(col[1])
  bitmap[2,,] <- as.raw(col[2])
  bitmap[3,,] <- as.raw(col[3])

  if (!is.null(alpha) && alpha != 1)
    bitmap[4,,] <- as.raw(as.integer(bitmap[4,,]) * alpha)

  magick::image_read(bitmap)
}








