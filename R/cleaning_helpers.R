#' @title Standardize NCAA School Names
#' @description
#' This function standardizes college names to cfbplotR defaults.
#' This helps for joins and plotting.
#'
#' @param school a character vector of names
#' @param keep_non_matches If `TRUE` (the default) an element of `school` that can't
#'   be matched to any of the internal mapping vectors will be kept as is. Otherwise
#'   it will be replaced with `NA`.
#'
#' @return A character vector with the length of `school` and cleaned team abbreviations
#'   if they are included in ```team_name_mapping```. Non matches may be replaced
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
    warning("Abbreviations not found in `team_name_mapping`: ",
            paste(utils::head(school[is.na(a)], 10), collapse = " , "),
            call. = FALSE)
  }
  if (isTRUE(keep_non_matches))
    a <- a %c% school
  a
}


#' Clean CFB team abbreviations
#' @description Alias of [clean_school_names()] for naming parity with the
#'   nflplotR/nbaplotR family.
#' @param school a character vector of names.
#' @param keep_non_matches If `TRUE` (the default) an element of `school` that
#'   can't be matched to any of the internal mapping vectors will be kept as is.
#'   Otherwise it will be replaced with `NA`.
#' @return A character vector of cleaned team names.
#' @export
clean_team_abbrs <- function(school, keep_non_matches = TRUE) {
  clean_school_names(school = school, keep_non_matches = keep_non_matches)
}


#' @title Add Athlete ID's to data frame
#' @description
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
#' \describe{
#'   \item{`athlete_id`}{athlete ESPN ID.}
#'   \item{`headshot_url`}{url of athlete's headshot.}
#' }
#' @export
#' @examples
#' \donttest{
#' x <- data.frame(
#'   player_name = c("Britain Covey","JT Daniels")
#' )
#'
#' add_athlete_id_col(x, player_name)
#'
#'
#' x$season <- c(2021,2021)
#' add_athlete_id_col(x, player_name)
#'
#'
#' x$team = c("Utah","Georgia")
#' add_athlete_id_col(x, player_name, team, headshot_urls = TRUE)
#'}

#' Load cfbfastR-data rosters for one or more seasons
#'
#' `most_recent_cfb_season()` rolls over to the new season on August 15, but
#' cfbfastR-data does not publish that season's roster file until the season
#' actually starts. Reading the file that does not exist yet used to abort the
#' whole call with a bare `cannot open the connection` error. Skip the seasons
#' that are not published and name them, so a multi-season request still
#' returns the seasons that do exist.
#'
#' @param seasons Numeric vector of seasons.
#' @return A data frame of rosters; zero rows if no season was available.
#' @keywords internal
#' @noRd
load_cfb_rosters <- function(seasons) {
  # The documented schema, so a request whose seasons are all unpublished
  # returns a frame the caller can still select() and join() against rather
  # than the 0x0 frame bind_rows() gives for an all-NULL list.
  empty <- data.frame(
    season = numeric(0),
    athlete_id = character(0),
    name = character(0),
    team = character(0),
    headshot_url = character(0),
    stringsAsFactors = FALSE
  )

  frames <- lapply(seasons, function(x) {
    notes <- character()
    roster <- withCallingHandlers(
      tryCatch(
        readRDS(
          url(glue::glue("https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_{x}.rds?raw=true"))
        ),
        error = function(e) e
      ),
      warning = function(w) {
        notes <<- c(notes, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    if (inherits(roster, "condition")) {
      # readRDS() reports every failed connection as "cannot open the
      # connection" regardless of cause; the HTTP status reaches the warning
      # only. Treat a 404 as a season that is not published yet, and let
      # anything else - a timeout, a DNS failure, a GitHub 5xx, a corrupt
      # file - surface, so a transient outage is never silently mistaken for
      # missing data and quietly answered with another season's rosters.
      if (!any(grepl("404 Not Found", notes, fixed = TRUE))) {
        stop(roster)
      }
      return(NULL)
    }

    roster %>%
      dplyr::transmute(
        season = x,
        .data$athlete_id,
        name = paste(.data$first_name, .data$last_name),
        .data$team,
        .data$headshot_url
      )
  })

  unavailable <- seasons[vapply(frames, is.null, logical(1))]
  if (length(unavailable) > 0) {
    cli::cli_alert_warning(
      "No published cfbfastR-data roster file for {cli::qty(length(unavailable))}season{?s} {.val {unavailable}}"
    )
  }

  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0) {
    return(empty)
  }

  dplyr::bind_rows(frames)
}

add_athlete_id_col <- function(df, name_col,team_col = NULL, headshot_urls = FALSE) {
  name_col <- dplyr::enquo(name_col)
  team_col <- dplyr::enquo(team_col)

  if ("season" %in% names(df)) {
    season_col_present <- TRUE
    seasons <- df %>%
      dplyr::filter(.data$season >= 2009, .data$season <= most_recent_cfb_season()) %>%
      dplyr::distinct(.data$season) %>%
      dplyr::arrange(desc(.data$season)) %>%
      dplyr::pull("season")
    if (length(seasons) == 0) {
      cli::cli_alert_info("No valid seasons (2009-{.val most_recent_cfb_season()}) in season column, using {.val most_recent_cfb_season()} rosters")
      seasons <- most_recent_cfb_season()
    }
    rosters <- load_cfb_rosters(seasons)
  } else {
    season_col_present <- FALSE
    cli::cli_alert_info("No season column, using {.val most_recent_cfb_season()} rosters")
    rosters <- load_cfb_rosters(most_recent_cfb_season())
    if (nrow(rosters) == 0) {
      fallback <- most_recent_cfb_season() - 1
      cli::cli_alert_info(
        "Rosters for {.val {most_recent_cfb_season()}} are not published yet, using {.val {fallback}}"
      )
      rosters <- load_cfb_rosters(fallback)
    }
  }
  if (isFALSE(headshot_urls)) {
    rosters <- rosters %>%
      dplyr::select(-"headshot_url")
  }

  if (rlang::quo_is_null(team_col) & !"team" %in% names(df) & !"school" %in% names(df)) {
    rosters <- rosters %>%
      dplyr::select(-"team")
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
  if (team_col_present & season_col_present) {
    join_list <- c("name", "team", "season")
    names(join_list) <- c(dplyr::as_label(name_col),team_col_label,"season")
  } else if (team_col_present & !season_col_present) {
    join_list <- c("name","team")
    names(join_list) <- c(dplyr::as_label(name_col),team_col_label)
  } else if (!team_col_present & season_col_present) {
    join_list <- c("name","season")
    names(join_list) <- c(dplyr::as_label(name_col),"season")
  } else {
    join_list <- c("name")
    names(join_list) <- c(dplyr::as_label(name_col))
  }

  df %>%
    dplyr::left_join(rosters, by = join_list)
}


