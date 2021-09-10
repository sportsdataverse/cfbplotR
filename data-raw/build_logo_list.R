# team_info <- cfbfastR::cfbd_team_info(only_fbs = FALSE  )
#
#
# temp_logos <-team_info %>%
#   unnest_wider(logos,names_sep = "_") %>%
#   select(school,abbreviation,logo = logos_1) %>%
#   pivot_longer(school:abbreviation,values_to = "school") %>%
#   select(-name) %>%
#   group_by(school) %>%
#   slice(1) %>%
#   ungroup()
#


#
#
#
# logo_list <- lapply(combined$school, function(x){
#   url <- combined$logo[combined$school == x]
#   curl::curl_fetch_memory(url)$content
# })
# logo_list <- rlang::set_names(logo_list, combined$school)
# #logo_list <- combined$logo
# #names(logo_list) <- combined$school
#
# logo_list
#
# usethis::use_data(logo_list, internal = TRUE, overwrite = TRUE)
#









fbs_teams <- cfbfastR::cfbd_team_info()
teams_csv <- read_csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv",skip = 1,col_names = c("row","team_id","school","mascot","abbreviation","alt_name1","alt_name2","alt_name3","conference","division","color","alt_color","logo","logo_dark"))

temp_logos <- teams_csv %>%
  select(school, abbreviation, logo) %>%
  filter(logo != "NA,NA") %>%
  mutate(fbs = if_else(school %in% fbs_teams$school,1,2)) %>%
  pivot_longer(school:abbreviation,values_to = "school") %>%
  group_by(school) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(fbs,desc(name)) %>%
  select(-name,-fbs)

conferences <- cfbfastR::cfbd_conferences()
conferences<- conferences %>%
  mutate(logo = glue::glue("https://a.espncdn.com/i/teamlogos/ncaa_conf/500/{conference_id}.png")) %>%
  filter(conference_id<100)

temp_conf <- conferences %>%
  pivot_longer(c(name,abbreviation),values_to = "school") %>%
  select(-long_name,-conference_id,-name) %>%
  filter(!is.na(school)) %>%
  group_by(school) %>%
  slice(1) %>%
  ungroup()

combined <- bind_rows(temp_logos,temp_conf)

logo_list <- combined$logo
names(logo_list) <- combined$school



# Check for missing logos
ind <- NA

for (i in 1:length(logo_list)) {
tryCatch(
  expr ={
    Sys.sleep(.1)
    magick::image_read(logo_list[[i]])
    },
  error = function(e) {
    message("The following error has occured:")
    message(e)
    ind <<- append(ind,i)
  },
  finally = {}
  )
}
ind <- ind[-1]

logo_list <- logo_list[-ind]

usethis::use_data(logo_list, internal = TRUE, overwrite = TRUE)
