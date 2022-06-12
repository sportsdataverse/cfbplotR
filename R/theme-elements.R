#' Theme Elements for Image Grobs
#'
#' @description
#' In conjunction with the [ggplot2::theme] system, the following `element_`
#' functions enable images in non-data components of the plot, e.g. axis text.
#'
#'   - `element_cfb_logo()`: draws college team logos instead of their names.
#'   - `element_cfb_wordmark()`: draws college team wordmarks instead of their names.
#'   - `element_cfb_headshot()`: draws player headshots instead of their ESPN player IDs.
#'   - `element_path()`: draws images from valid image URLs instead of the URL.
#'
#' @details The elements translate CFB team names or players' ESPN IDs
#'   into logo images or player headshots, respectively.
#' @param alpha The alpha channel, i.e. transparency level, as a numerical value
#'   between 0 and 1.
#' @param colour,color The image will be colorized with this color. Use the
#'   special character `"b/w"` to set it to black and white. For more information
#'   on valid color names in ggplot2 see
#'   <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>.
#' @param hjust,vjust The horizontal and vertical adjustment respectively.
#'   Must be a numerical value between 0 and 1.
#' @param size The output grob size in `cm` (!).
#' @seealso [geom_cfb_logos()], [geom_cfb_wordmarks()], [geom_cfb_headshots()],
#'   and [geom_from_path()] for more information on valid team names,
#'   player IDs, and other parameters.
#' @return An S3 object of class `element`.
#' @examples
#' \donttest{
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
#' # use logos for x-axis
#' ggplot(df, aes(x = teams, y = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_cfb(alt_colors = team_abbr) +
#'   scale_fill_cfb(alpha = 0.4) +
#'   theme_minimal() +
#'   theme(axis.text.x = element_cfb_logo())
#'
#' # use logos for y-axis
#' ggplot(df, aes(y = teams, x = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_cfb(alt_colors = team_abbr) +
#'   scale_fill_cfb(alpha = 0.4) +
#'   theme_minimal() +
#'   theme(axis.text.y = element_cfb_logo())
#'
#' #############################################################################
#' # Headshot Examples
#' #############################################################################
#' library(cfbplotR)
#' library(ggplot2)
#'
#' # Silence an nflreadr message that is irrelevant here
#' old <- options(nflreadr.cache_warning = FALSE)
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
#'   theme_minimal() +
#'   theme(axis.text.x = element_cfb_headshot())
#'
#' # use headshots for y-axis
#' ggplot(dfh, aes(y = player_id, x = random_value)) +
#'   geom_col(width = 0.5) +
#'   theme_minimal() +
#'   theme(axis.text.y = element_cfb_headshot())
#' #############################################################################
#' # Wordmarks and other Images
#' #############################################################################
#'
#' library(ggplot2)
#'
#' df <- dplyr::mutate(mtcars,
#'   team = sample(c("Utah", "Arizona State", "Oregon", "UCLA"), nrow(mtcars), TRUE),
#'   player = sample(
#'   c("4361182", "4426385", "4567048", "4429013"),
#'   nrow(mtcars),
#'   TRUE
#'   )
#' )
#'
#' ggplot(df, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   facet_wrap(vars(team)) +
#'   labs(
#'     title = tools::toTitleCase("These are random teams and data"),
#'     subtitle = "I just want to show how the cfbplotR theme elements work",
#'     caption = "https://raw.githubusercontent.com/sportsdataverse/sportsdataverse-web/master/public/images/logo.png"
#'   ) +
#'   theme_minimal() +
#'   theme(
#'     plot.title.position = "plot",
#'     plot.title = element_text(face = "bold"),
#'     axis.title = element_blank(),
#'     # make wordmarks of team abbreviations
#'     strip.text = element_cfb_wordmark(size = 1),
#'     # load image from url in caption
#'     plot.caption = element_path(hjust = 1, size = 0.4)
#'   )
#' }
#' @name element
#' @aliases NULL
NULL

#' @export
#' @rdname element
element_cfb_logo <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                             color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cfb_logo", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_cfb_wordmark <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                 color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cfb_wordmark", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_cfb_headshot <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                 color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cfb_headshot", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_path <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                         color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_path", "element_text", "element")
  )
}

#' @export
element_grob.element_cfb_logo <- function(element, label = "", x = NULL, y = NULL,
                                          alpha = NULL, colour = NULL,
                                          hjust = NULL, vjust = NULL,
                                          size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "teams"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_cfb_wordmark <- function(element, label = "", x = NULL, y = NULL,
                                              alpha = NULL, colour = NULL,
                                              hjust = 0.5, vjust = 0.5,
                                              size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "wordmarks"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_cfb_headshot <- function(element, label = "", x = NULL, y = NULL,
                                              alpha = NULL, colour = NULL,
                                              hjust = NULL, vjust = NULL,
                                              size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "headshots"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_path <- function(element, label = "", x = NULL, y = NULL,
                                      alpha = NULL, colour = NULL,
                                      hjust = NULL, vjust = NULL,
                                      size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "path"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

axisImageGrob <- function(i, label, alpha, colour, data, x, y, hjust, vjust,
                          width = 1, height = 1,
                          type = c("teams", "headshots", "wordmarks", "path")) {
  make_null <- FALSE
  type <- rlang::arg_match(type)
  if(type == "teams") {
    team <- label[i]
    team <- cfbplotR::clean_school_names(as.character(team))
    if (!team %in% valid_team_names()) {
      cli::cli_warn("{label[i]} is not a valid team name (row {i})")
      team <- "NCAA"
    }
    if (is.na(team)) {make_null <- TRUE}
    else{image_to_read <- logo_list[[team]]}
  } else if(type == "wordmarks") {
    team <- label[i]
    team <- cfbplotR::clean_school_names(as.character(team))
    if (!team %in% names(wordmark_list)) {
      cli::cli_warn("{label[i]} does not have a wordmark")
      team <- "NCAA"
    }
    image_to_read <- wordmark_list[[team]]
    if (is.na(team)) make_null <- TRUE
  } else if (type == "path"){
    image_to_read <- label[i]
  } else {
    player_id <- label[i]
    headshot_map <- headshot_id_to_url(player_id)
    #headshot_map <- paste0("http://a.espncdn.com/i/headshots/college-football/players/full/",player_id,".png")
    if(!RCurl::url.exists(headshot_map)) {
      cli::cli_warn("{label[i]} is not a valid player id (row {i})")
      headshot_map <- "http://a.espncdn.com/i/headshots/nophoto.png"
    }
    image_to_read <- headshot_map

  }
  if (is.na(make_null)){
    return(grid::nullGrob())
  } else if (is.null(alpha[i])) {
    img <- magick::image_read(image_to_read)
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(img, colorspace = 'gray')
    } else {
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(img, opa, col)
    }
  } else if (length(alpha) == 1L) {
    if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
      cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
    }
    img <- magick::image_read(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else {
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
  } else {
    if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
      cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
    }
    img <- magick::image_read(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
  }

  grid::rasterGrob(
    new,
    x = x[i],
    y = y[i],
    width = grid::unit(width, "snpc"),
    height = grid::unit(height, "snpc"),
    hjust = hjust,
    vjust = vjust
  )
}

#' @export
grobHeight.axisImageGrob <- function(x, ...) grid::unit(x$size, "cm")

#' @export
grobWidth.axisImageGrob <- function(x, ...) grid::unit(x$size, "cm")
