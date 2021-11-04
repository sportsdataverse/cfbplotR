#' @name scale_color_cfb
#' @aliases scale_color_cfb scale_colour_cfb scale_fill_cfb
#' @title
#' Scale for college football team colors
#'
#' @description These functions allows you to map college football team names as levels to the color and fill aesthetics
#' @inheritParams ggplot2::scale_fill_manual
#' @param alt_colors Vector of team names to use the alternate color of.
#' @param ... Arguments passed on to scale_color_manual
#' @param values If `NULL` (the default) use the internal team color vectors. Otherwise
#'   a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with `breaks` if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that don't
#'   match will be given `na.value`.
#' @param guide A function used to create a guide or its name. If `NULL` (the default)
#'   no guide will be plotted for this scale. See [ggplot2::guides()] for more information.
#' @param alpha Factor to modify color transparency via a call to [`scales::alpha()`].
#'   If `NA` (the default) no transparency will be applied. Can also be a vector of
#'   alphas. All alpha levels must be in range `[0,1]`.
#' @importFrom rlang .data
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


scale_color_cfb <- function(alt_colors = NULL,
                            values = NULL,
                            ...,
                            aesthetics = "colour",
                            breaks = ggplot2::waiver(),
                            na.value = "grey50",
                            guide = NULL,
                            alpha = NA) {
  if(is.null(values)){
    values <-  dplyr::mutate(cfbplotR::logo_ref,
                             value = ifelse(.data$school %in% alt_colors,
                                            .data$alt_color,
                                            .data$color))
    values <- dplyr::pull(values, .data$value)
    names(values) <- cfbplotR::logo_ref$school
  }


  if(!is.na(alpha)) values <- scales::alpha(values, alpha = alpha)

  ggplot2::scale_color_manual(
    ...,
    values = values,
    aesthetics = aesthetics,
    breaks = breaks,
    na.value = na.value,
    guide = guide
  )
}

#' @rdname  scale_color_cfb
#' @export
scale_colour_cfb <- scale_color_cfb

#' @rdname scale_color_cfb
#' @export

scale_fill_cfb <- function(alt_colors = NULL,
                           values = NULL,
                           ...,
                           aesthetics = "fill",
                           breaks = ggplot2::waiver(),
                           na.value = "grey50",
                           guide = NULL,
                           alpha = NA) {
  if(is.null(values)){
    values <-  dplyr::mutate(cfbplotR::logo_ref,
                             value = ifelse(.data$school %in% alt_colors,
                                            .data$alt_color,
                                            .data$color))
    values <- dplyr::pull(values, .data$value)
    names(values) <- cfbplotR::logo_ref$school
  }

  if(!is.na(alpha)) values <- scales::alpha(values, alpha = alpha)

  ggplot2::scale_fill_manual(
    ...,
    values = values,
    aesthetics = aesthetics,
    breaks = breaks,
    na.value = na.value,
    guide = guide
  )
}





# Axis Scales -------------------------------------------------------------

#' Axis Scales for CFB Team Logos
#'
#' @description These functions map CFB team names to their team logos and make
#'   them available as axis labels
#' @details The scale translates the CFB team names into raw image
#'   html and places the html as axis labels. Because of the way ggplots are
#'   constructed, it is necessary to adjust the [`theme()`] after calling this
#'   scale. This can be done by calling [`theme_x_cfb()`] or [`theme_y_cfb()`]
#'   or alternatively by manually changing the relevant `axis.text` to
#'   [`ggtext::element_markdown()`].
#' @inheritParams ggplot2::scale_x_discrete
#' @param size The logo size in pixels. It is applied as height for an x-scale
#'   and as width for an y-scale.
#' @name scale_axes_cfb
#' @aliases NULL
#' @seealso [`theme_x_cfb()`], [`theme_y_cfb()`]
#' @examples
#' library(cfbplotR)
#' library(ggplot2)
#'
#' team_abbr <- valid_team_names()
#' # remove conference logos from this example
#' team_abbr <- team_abbr[1:8]
#'
#' df <- data.frame(
#'   random_value = runif(length(team_abbr), 0, 1),
#'   teams = team_abbr
#' )
#'
#' ggplot(df, aes(x = teams, y = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_cfb(alt_colors = team_abbr) +
#'   scale_fill_cfb(alpha = 0.4) +
#'   scale_x_cfb() +
#'   theme_minimal() +
#'   theme_x_cfb()
#'
#' #############################################################################
#' # Headshot Examples
#' #############################################################################
#' library(cfbplotR)
#' library(ggplot2)
#'
#'
#' dfh <- data.frame(
#'   random_value = runif(9, 0, 1),
#'   player_id = c("4361182",
#'                   "4426385",
#'                   "4567048",
#'                   "4372519",
#'                   "4429013",
#'                   "4240069",
#'                   "4360932",
#'                   "4362874",
#'                   "4429299")
#' )
#'
#' # use headshots for x-axis
#' ggplot(dfh, aes(x = player_id, y = random_value)) +
#'   geom_col(width = 0.5) +
#'   scale_x_cfb_headshots() +
#'   theme_minimal() +
#'   theme_x_cfb()
#'
#' # use headshots for y-axis
#' ggplot(dfh, aes(y = player_id, x = random_value)) +
#'   geom_col(width = 0.5) +
#'   scale_y_cfb_headshots() +
#'   theme_minimal() +
#'   theme_y_cfb()
#'
#'
#'
NULL

#' @rdname scale_axes_cfb
#' @export
scale_x_cfb <- function(...,
                        expand = ggplot2::waiver(),
                        guide = ggplot2::waiver(),
                        position = "bottom",
                        size = 12) {
  ggplot2::scale_x_discrete(
    ...,
    labels = function(x) {
      logo_html(x, type = "height", size = size)
    },
    expand = expand,
    guide = guide,
    position = position
  )
}

#' @rdname scale_axes_cfb
#' @export
scale_y_cfb <- function(...,
                        expand = ggplot2::waiver(),
                        guide = ggplot2::waiver(),
                        position = "left",
                        size = 12) {
  ggplot2::scale_y_discrete(
    ...,
    labels = function(x) {
      logo_html(x, type = "width", size = size)
    },
    expand = expand,
    guide = guide,
    position = position
  )
}

#' @rdname scale_axes_cfb
#' @export
scale_x_cfb_headshots <- function(...,
                                  expand = ggplot2::waiver(),
                                  guide = ggplot2::waiver(),
                                  position = "bottom",
                                  size = 20) {

  position <- rlang::arg_match0(position, c("top", "bottom"))

  ggplot2::scale_x_discrete(
    ...,
    labels = function(x) {
      headshot_html(x, type = "height", size = size)
    },
    expand = expand,
    guide = guide,
    position = position
  )
}

#' @rdname scale_axes_cfb
#' @export
scale_y_cfb_headshots <- function(...,
                                  expand = ggplot2::waiver(),
                                  guide = ggplot2::waiver(),
                                  position = "left",
                                  size = 30) {

  position <- rlang::arg_match0(position, c("left", "right"))

  ggplot2::scale_y_discrete(
    ...,
    labels = function(x) {
      headshot_html(x, type = "width", size = size)
    },
    expand = expand,
    guide = guide,
    position = position
  )
}
