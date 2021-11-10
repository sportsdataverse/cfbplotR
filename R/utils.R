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

valid_team_names <- function(division = c("FBS","FCS","DII","DIII","Conference","hoopR")){

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


`%c%` <- function (x, y){
  ifelse(!is.na(x), x, y)
}

headshot_id_or_url <- function(x) {
  x <- x[1]
  if(suppressWarnings(is.numeric(as.numeric(x)))){
    for_return <- "ID"
  } else if(grepl("^http:",x)) {
    for_return <- "URL"
  } else {
    for_return <-"NA"
  }
  return(for_return)
}


headshot_id_to_url <- function(x) {
  paste0("http://a.espncdn.com/i/headshots/college-football/players/full/",x,".png")
}

headshot_html <- function(player_id, type = c("height", "width"), size = 25){
  type <- rlang::arg_match(type)
  url <- headshot_id_to_url(player_id)
  url <- ifelse(RCurl::url.exists(url),url,"http://a.espncdn.com/i/headshots/nophoto.png")
  # if(!RCurl::url.exists(url)) {
  #   #cli::cli_warn("{data$player_id[i]} is not a valid player id (row {i})")
  #   url <- "http://a.espncdn.com/i/headshots/nophoto.png"
  # }
  sprintf("<img src='%s' %s = '%s'>", url, type, size)
}

