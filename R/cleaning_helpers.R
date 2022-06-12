#' Standardize NCAA School Names
#'
#' This function standardizes college names to cfbplotR defaults.
#' This helps for joins and plotting.
#'
#' @param school a character vector of names
#' @param keep_non_matches If `TRUE` (the default) an element of `school` that can't
#'   be matched to any of the internal mapping vectors will be kept as is. Otherwise
#'   it will be replaced with `NA`.
#'
#' @return A character vector with the length of `school` and cleaned team abbreviations
#'   if they are included in [`team_name_mapping`]. Non matches may be replaced
#'   with `NA` (depending on the value of `keep_non_matches`).
#' @export
#' @examples
#'
#' x <- c("utah", "San Jose State", "Hawaii", "UTSA", "SLC", "USC")
#' # use current location and keep non matches
#' clean_school_names(x)
#'
#' # replace non matches
#' clean_school_names(x, keep_non_matches = FALSE)


clean_school_names <- function(school, keep_non_matches = TRUE) {
  stopifnot(is.character(school))
  m <- cfbplotR::team_name_mapping
  a <- unname(m[tools::toTitleCase(school)])
  if (any(is.na(a))) {
    warning("Abbreviations not found in `cfbplotR::team_name_mapping`: ",
            paste(utils::head(school[is.na(a)], 10), collapse = " , "),
            call. = FALSE)
  }
  if (isTRUE(keep_non_matches))
    a <- a %c% school
  a
}


#' Add Athlete ID's to data frame
#'
#' This function attempts to add ESPN athlete ID's to a data frame using the roster data
#' in the cfbfastR-data repo. The function is experimental and not guaranteed to be accurate.
#'
#' @param df a data frame.
#' @param name_col the column in `df` with the player names to join with the roster data.
#' @param team_col Optional column with team names to join with the roster data to reduce
#' the chance of matching with two players from different teams with the same name. If NULL
#' and `df` has a column named "team" or "school," the function will use those as `team_col`.
#' The function also checks `df` for a column named "season" to match names to rosters going
#' back to 2009.
#' @param headshot_urls logical to return headshot URLs. If TRUE, the output has an additional
#' column called "headshot_url" with links for player headshots.
#'
#' @return the original `df` with extra columns:
#' \item{`athlete_id`}{athlete ESPN ID.}
#' \item{`headshot_url`}{url of athlete's headshot.}
#'
#' @export
#' @examples
#' \donttest{
#' x <- data.frame(
#'   player_name = c("Britain Covey","JT Daniels")
#' )
#'
#' cfbplotR::add_athlete_id_col(x, player_name)
#'
#'
#' x$season <- c(2021,2021)
#' cfbplotR::add_athlete_id_col(x, player_name)
#'
#'
#' x$team = c("Utah","Georgia")
#' cfbplotR::add_athlete_id_col(x, player_name, team, headshot_urls = TRUE)
#'}


add_athlete_id_col <- function(df, name_col,team_col = NULL, headshot_urls = FALSE) {
  name_col <- dplyr::enquo(name_col)
  team_col <- dplyr::enquo(team_col)

  if ("season" %in% names(df)) {
    season_col_present <- TRUE
    seasons <- df %>%
      dplyr::filter(season >= 2009, season <= 2021) %>%
      dplyr::distinct(season) %>%
      dplyr::arrange(desc(season)) %>%
      dplyr::pull(season)
    if(length(seasons) == 0){
      cli::cli_alert_info("No valid seasons (2009-2021) in season column, using 2021 rosters")
      seasons <- 2021
    }
    rosters <- purrr::map_df(seasons, function(x){
      readRDS(
        url(glue::glue("https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_{x}.rds?raw=true"))
      ) %>%
        dplyr::transmute(
          season = x,
          athlete_id,
          name = paste(first_name, last_name),
          team,
          headshot_url
        ) %>%
        return()
      }
    )
  } else {
    season_col_present <- FALSE
    cli::cli_alert_info("No season column, using 2021 rosters")
    rosters <- purrr::map_df(2021, function(x){
      readRDS(
        url(glue::glue("https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_{x}.rds?raw=true"))
      ) %>%
        dplyr::transmute(
          season = x,
          athlete_id,
          name = paste(first_name, last_name),
          team,
          headshot_url
        ) %>%
        return()
    }
    )
  }
  if(isFALSE(headshot_urls)){
    rosters <- rosters %>%
      dplyr::select(-headshot_url)
  }

  if(rlang::quo_is_null(team_col) & !"team" %in% names(df) & !"school" %in% names(df)) {
    rosters <- rosters %>%
      dplyr::select(-team)
    team_col_present <- FALSE
  } else {
    team_col_present <- TRUE
    team_col_label <- dplyr::case_when(
      !rlang::quo_is_null(team_col) ~ dplyr::as_label(team_col),
      "team" %in% names(df) ~ "team",
      "school" %in% names(df) ~ "school",
      TRUE ~ ""
    )
  }

  # Generate list for joins based on available columns
  if(team_col_present & season_col_present){
    join_list <- c("name","team","season")
    names(join_list) <- c(dplyr::as_label(name_col),team_col_label,"season")
  } else if(team_col_present & !season_col_present){
    join_list <- c("name","team")
    names(join_list) <- c(dplyr::as_label(name_col),team_col_label)
  } else if(!team_col_present & season_col_present){
    join_list <- c("name","season")
    names(join_list) <- c(dplyr::as_label(name_col),"season")
  } else {
    join_list <- c("name")
    names(join_list) <- c(dplyr::as_label(name_col))
  }

  df %>%
    dplyr::left_join(rosters, by = join_list)
}


