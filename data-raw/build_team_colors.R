# team_info <- cfbfastR::cfbd_team_info(only_fbs = FALSE)
#
# team_colors <- team_info %>%
#   select(school,color,alt_color) %>%
#   replace_na(list(alt_color = "#000000")) %>%
#   # For Future color changes
#   mutate(color = case_when(TRUE ~ color),
#          alt_color = case_when(TRUE ~ alt_color))
#
# team_colors
#
# usethis::use_data(team_colors)



teams_csv <- read_csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv",skip = 1,col_names = c("row","team_id","school","mascot","abbreviation","alt_name1","alt_name2","alt_name3","conference","division","color","alt_color","logo","logo_dark"))

team_colors <- teams_csv %>%
  select(school, abbreviation, color, alt_color) %>%
  mutate(school = if_else(str_detect(school,"^San Jos"),"San José State",school)) %>%
  filter(school %in% valid_team_names()) %>%
  pivot_longer(school:abbreviation,values_to = "school") %>%
  group_by(school) %>%
  slice(1) %>%
  ungroup() %>%
  replace_na(replace = list(color = "#ffffff",alt_color = "#ffffff")) %>%
  select(-name)



usethis::use_data(team_colors, overwrite = TRUE)
