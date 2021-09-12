#' Output Valid CFB Team Names and Abbreviations
#'
#' @export
#' @examples
#' # List valid team abbreviations excluding duplicates
#' valid_team_names()

valid_team_names <- function(){
  n <- names(logo_list)
  n
}


logo_html <- function(team_abbr, type = c("height", "width"), size = 15){
  type <- rlang::arg_match(type)
  url <- logo_list[team_abbr]
  sprintf("<img src='%s' %s = '%s'>", url, type, size)
}

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
