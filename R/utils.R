#' Output Valid CFB Team Names and Abbreviations
#'
#' @param division Division to filter for. Options include:
#'  - FBS
#'  - P5
#'  - G5
#'  - FCS
#'  - DII
#'  - DIII
#'  - Conference
#'  - hoopR
#'  - Other
#' @export
#' @examples
#' # List valid team abbreviations excluding duplicates
#' valid_team_names("FBS")
#' valid_team_names("FCS")
#' valid_team_names("DII")
#' valid_team_names("DIII")
#' valid_team_names("Conference")

valid_team_names <- function(division = c("FBS","P5","G5","FCS","DII","DIII","Conference","hoopR","Other")){
  if (length(division) == 1) {
    if (division == "FBS") {division <- c("P5","G5")}
  }
  cfbplotR::logo_ref %>%
    dplyr::filter(.data$type %in% division) %>%
    dplyr::pull(.data$school)
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

most_recent_cfb_season <- function() {
  date <- Sys.Date()
  dplyr::case_when(
    as.double(substr(date, 6, 7)) >= 8 & as.double(substr(date, 9, 10)) >= 15  ~ as.double(substr(date, 1, 4)),
    as.double(substr(date, 6, 7)) >= 9 ~ as.double(substr(date, 1, 4)),
    TRUE ~ as.double(substr(date, 1, 4)) - 1
  )
}
