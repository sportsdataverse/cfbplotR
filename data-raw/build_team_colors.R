team_info <- cfbfastR::cfbd_team_info(only_fbs = FALSE)

team_colors <- team_info %>%
  select(school,color,alt_color) %>%
  replace_na(list(alt_color = "#000000")) %>%
  # For Future color changes
  mutate(color = case_when(TRUE ~ color),
         alt_color = case_when(TRUE ~ alt_color))

team_colors

usethis::use_data(team_colors)
