#' Functions for adding an image to the title of a ggplot
#'
#' @description These functions work together to place an image to the left or right of the title in a ggplot. [`ggtitle_image`] is the main function but must be used with either theme_title_image() or setting the plot.title argument in [`ggplot2::theme()`] to [`ggtext::element_markdown()`]
#' @param title_image The URL of the image to add to the title. If one of [`valid_team_names()`], the school logo will be used automatically.
#' @param title The text for the title. The title has to be defined here as the function overwrites any current title text.
#' @param image_height The height of the image in pixels.
#' @param image_side One of "left" or "right". Places the image on either side of the title text.
#' @param subtitle Optional text for the subtitle. Does not overwrite current subtitle if left blank.
#' @param ... Other arguments passed on to [`ggtext::element_markdown()`].
#' @name ggtitle_image
#' @seealso [`ggtitle_image()`], [`theme_title_image()`]
#' @examples
#' \donttest{
#' library(cfbplotR)
#' library(ggplot2)
#'
#' logo_url <- "https://github.com/Kazink36/cfbplotR/raw/master/man/figures/logo.png"
#'
#'   p <- ggplot(mtcars, aes(x = hp, y = mpg)) +
#'     geom_point() +
#'     labs(title = "This Title will be overwritten",
#'          subtitle = "This is the Subtitle")
#'
#' if (utils::packageVersion("gridtext") > "0.1.4"){
#'   p +
#'     ggtitle_image(title_image = logo_url,
#'                   title = "This is the Title",
#'                   image_height = 40,
#'                   image_side = "right") +
#'     theme_title_image(size = 20, hjust = 0.5)
#' }
#' if (utils::packageVersion("gridtext") > "0.1.4"){
#'   p +
#'     ggtitle_image(title_image = "Utah",
#'                   title = "This it the Title",
#'                   image_height = 20,
#'                   image_side = "left",
#'                   subtitle = "This overwrites the old subtitle") +
#'     theme(plot.title = ggtext::element_markdown(size = 20, hjust = 0.5))
#' }
#'}
#'
NULL

#' @rdname ggtitle_image
#' @export
ggtitle_image <- function (title_image = ggplot2::waiver(), title = ggplot2::waiver(),
                           image_height = 15, image_side = c("left","right"),
                           subtitle = ggplot2::waiver())
{
  match.arg(image_side, c("left","right"))
  image_side <- image_side[1]
  school_check <- cfbplotR::clean_school_names(title_image,keep_non_matches = FALSE) %>%
    suppressWarnings()
  if (!is.na(school_check)){#title_image %in% valid_team_names()) {
    title_image <- logo_list[[school_check]]
  }


  title_image_tag <- paste0(
    "<img src='", title_image,
    "' height='",image_height,
    "' style='vertical-align: middle;'>"
  )
  # For potentially aligining image with title text, I don't believe this is currently possible with this image implementation
  # title <- paste0("<span style='vertical-align:top' valign ='center'>",title,"</span>")

  title <- dplyr::case_when(image_side == "right" ~ paste(title,title_image_tag),
                     TRUE ~ paste(title_image_tag,title))
  ggplot2::labs(title = title, subtitle = subtitle)
}

#' @rdname ggtitle_image
#' @export
theme_title_image <- function(...) {
  if (!is_installed("ggtext")) {
    cli::cli_abort(c(
      "Package {.val ggtext} required to create this scale.",
      'Please install it with {.var install.packages("ggtext")}'
    ))
  }
  loadNamespace("gridtext", versionCheck = list(op = ">=", version = "0.1.4"))
  ggplot2::theme(
    plot.title = ggtext::element_markdown(...),
  )

}
