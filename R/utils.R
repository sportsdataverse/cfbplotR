#' Output Valid CFB Team Names and Abbreviations
#'
#' @export
#' @examples
#' # List valid team abbreviations excluding duplicates
#' valid_team_names()

valid_team_names <- function(){
  n <- sort(names(logo_list))
  n
}
