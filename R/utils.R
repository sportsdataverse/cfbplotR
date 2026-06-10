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

#' Clear the cfbplotR (ggpath) image cache
#' @description Clears the image cache used by the ggpath rendering backend.
#'   ggpath (as of its current release) does not expose a public cache-clearing
#'   function, so this is a safe no-op that is forward-compatible: if a future
#'   ggpath version exports `clear_cache()` this will call it automatically.
#' @return Invisibly `NULL`.
#' @export
.cfbplotR_clear_cache <- function() {
  if (requireNamespace("ggpath", quietly = TRUE) &&
      "clear_cache" %in% getNamespaceExports("ggpath")) {
    getExportedValue("ggpath", "clear_cache")()
  }
  invisible(NULL)
}


# Resolve CFB team names to logo image paths (vectorised). Invalid names warn
# and fall back to the generic NCAA logo. ggpath renders the returned paths.
logo_from_school <- function(team) {
  team <- clean_school_names(as.character(team))
  valid <- valid_team_names()
  bad <- !is.na(team) & !(team %in% valid)
  if (any(bad)) {
    cli::cli_warn("{.val {unique(team[bad])}} is/are not valid team name(s); using the NCAA logo.")
    team[bad] <- "NCAA"
  }
  unname(vapply(team, function(t) if (is.na(t)) NA_character_ else logo_list[[t]], character(1)))
}

# Resolve CFB team names to wordmark image paths (vectorised).
wordmark_from_school <- function(team) {
  team <- clean_school_names(as.character(team))
  bad <- !is.na(team) & !(team %in% names(wordmark_list))
  if (any(bad)) {
    cli::cli_warn("{.val {unique(team[bad])}} do(es) not have a wordmark; using the NCAA wordmark.")
    team[bad] <- "NCAA"
  }
  unname(vapply(team, function(t) if (is.na(t)) NA_character_ else wordmark_list[[t]], character(1)))
}

# Build ESPN headshot URLs from player ids (vectorised). ggpath fetches/caches.
headshot_from_id <- function(player_id) {
  player_id <- as.character(player_id)
  url <- headshot_id_to_url(player_id)
  url[is.na(player_id)] <- NA_character_
  url
}
