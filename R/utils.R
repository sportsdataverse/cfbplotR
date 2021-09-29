#' Output Valid CFB Team Names and Abbreviations
#'
#' @export
#' @examples
#' # List valid team abbreviations excluding duplicates
#' valid_team_names("FBS")
#' valid_team_names("FCS")
#' valid_team_names("DII")
#' valid_team_names("DIII")
#' valid_team_names("Conference")

valid_team_names <- function(division = c("FBS","FCS","DII","DIII","Conference")){

  cfbplotR::logo_ref %>%
    dplyr::filter(type %in% division) %>%
    dplyr::pull(school)
}


logo_html <- function(team_abbr, type = c("height", "width"), size = 15){
  type <- rlang::arg_match(type)
  url <- logo_list[team_abbr]
  sprintf("<img src='%s' %s = '%s'>", url, type, size)
}

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
