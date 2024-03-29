# INTERNAL HELPER THAT BUILD THE GROBS FOR
# GEOM LOGOS AND HEADSHOTS
build_grobs <- function(i, alpha, colour, data, type = c("teams", "headshots", "wordmarks", "path")) {
  make_null <- FALSE
  type <- rlang::arg_match(type)
  if (type == "teams") {
    team <- data$team[i]
    team <- clean_school_names(as.character(team))
    if (!(team %in% valid_team_names())) {
        cli::cli_warn("{data$team[i]} is not a valid team name (row {i})")
        team <- "NCAA"
    }
    if (is.na(team)) {
      make_null <- TRUE
    } else {
      image_to_read <- logo_list[[team]]
    }

  } else if (type == "wordmarks") {
    team <- data$team[i]
    team <- clean_school_names(as.character(team))
    if (!team %in% names(wordmark_list)) {
      cli::cli_warn("{data$team[i]} does not have a wordmark (row {i})")
      team <- "NCAA"
    }
    image_to_read <- wordmark_list[[team]]
    if (is.na(team)) make_null <- TRUE

  } else if (type == "path") {
    image_to_read <- data$path[i]
  } else {
    player_id <- data$player_id[i]
    headshot_map <- headshot_id_to_url(player_id)
    #headshot_map <- paste0("http://a.espncdn.com/i/headshots/college-football/players/full/",player_id,".png")
    if (!RCurl::url.exists(headshot_map)) {
      cli::cli_warn("{data$player_id[i]} is not a valid player id (row {i})")
      headshot_map <- "http://a.espncdn.com/i/headshots/nophoto.png"
    }
    image_to_read <- headshot_map
    #gsis <- data$player_gsis[i]
    #headshot_map <- load_headshots()
    # image_to_read <- headshot_map$headshot_cfb[headshot_map$gsis_id == gsis]
    # if(length(image_to_read) == 0) image_to_read <- na_headshot()
  }
  if (is.na(make_null)) {
    grid <- grid::nullGrob()
  } else if (is.null(alpha)) {
    img <- reader_function(image_to_read)
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w") {
      new <- magick::image_quantize(img, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(img, opa, col)
    }
    grid <- grid::rasterGrob(new)
  } else if (length(alpha) == 1L) {
    if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
      cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
    }
    img <- reader_function(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w") {
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
    grid <- grid::rasterGrob(new)
  } else {
    if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
      cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
    }
    img <- reader_function(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w") {
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
    grid <- grid::rasterGrob(new)
  }

  grid$vp <- grid::viewport(
    x = grid::unit(data$x[i], "native"),
    y = grid::unit(data$y[i], "native"),
    width = grid::unit(data$width[i], "npc"),
    height = grid::unit(data$height[i], "npc"),
    just = c(data$hjust[i], data$vjust[i]),
    angle = data$angle[i],
    name = paste("geom_cfb.panel", data$PANEL[i],
                 "row", i,
                 sep = "."
    )
  )

  grid$name <- paste("cfb.grob", i, sep = ".")

  grid
}

reader_function <- function(img){
  if (is.factor(img)) img <- as.character(img)
  if (is.raw(img) || tools::file_ext(img) != "svg") {
    magick::image_read(img)
  } else if (tools::file_ext(img) == "svg") {
    magick::image_read_svg(img)
  }
}
