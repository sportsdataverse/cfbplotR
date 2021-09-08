team_info <- cfbfastR::cfbd_team_info(only_fbs = FALSE  )


temp_logos <-team_info %>%
  unnest_wider(logos,names_sep = "_") %>%
  select(school,abbreviation,logo = logos_1) %>%
  pivot_longer(school:abbreviation,values_to = "school") %>%
  select(-name) %>%
  group_by(school) %>%
  slice(1) %>%
  ungroup()

conferences <- cfbfastR::cfbd_conferences() %>%
  mutate(logo = glue::glue("https://a.espncdn.com/i/teamlogos/ncaa_conf/500/{conference_id}.png"))
temp_conf <- conferences %>%
  filter(!conference_id %in% c(213)) %>%
  pivot_longer(c(name,abbreviation),values_to = "school") %>%
  select(-long_name,-conference_id,-name) %>%
  filter(!is.na(school)) %>%
  group_by(school) %>%
  slice(1) %>%
  ungroup()

combined <- bind_rows(temp_logos,temp_conf)

logo_list <- combined$logo
names(logo_list) <- combined$school

logo_list

usethis::use_data(logo_list)
