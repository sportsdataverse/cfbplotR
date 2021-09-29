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



#
#
#
#
#
#
fbs_teams <- cfbfastR::cfbd_team_info()
d3_teams <- list.files("./data-raw/dIII") %>%
  str_remove("\\.jpg")

fbs_logos <- fbs_teams %>%
  select(school,logos) %>%
  unnest_wider(logos,names_sep = "_") %>%
  transmute(school,logo = logos_1,type = "FBS")

d3_logos <- tibble(school = d3_teams,
       logo = glue::glue("https://raw.githubusercontent.com/Kazink36/cfbplotR/master/data-raw/dIII/{school}.jpg"),
       type = "DIII")

conferences <- cfbfastR::cfbd_conferences()
conf_logos <- conferences %>%
  filter(conference_id<100) %>%
  transmute(school = case_when(name == "Southern" ~ "Southern Conference",
                               TRUE ~ name),
            logo = glue::glue("https://a.espncdn.com/i/teamlogos/ncaa_conf/500/{conference_id}.png"),
            type = "Conference") %>%
  add_row(school = "NCAA",
          logo = "https://upload.wikimedia.org/wikipedia/en/thumb/c/cf/NCAA_football_icon_logo.svg/2560px-NCAA_football_icon_logo.svg.png",
          type = "Conference")


teams_csv <- read_csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv",skip = 1,col_names = c("row","team_id","school","mascot","abbreviation","alt_name1","alt_name2","alt_name3","conference","division","color","alt_color","logo","logo_dark"))

temp_logos <- teams_csv %>%
  filter(!school %in% fbs_logos$school, !school %in% d3_logos$school) %>%
  filter(logo != "NA,NA") %>%
  transmute(school, logo, type = "FCS")
# Check for missing logos
ind <- NA

for (i in 1:nrow(temp_logos)) {
tryCatch(
  expr ={
    Sys.sleep(.1)
    magick::image_read(temp_logos$logo[i])
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
fcs_logos <- temp_logos %>%
  filter(!row_number() %in% ind) %>%
  filter(!str_detect(school,"San Jos"))

team_ref <- bind_rows(fbs_logos,fcs_logos,d3_logos,conf_logos)

colors <- teams_csv %>%
  select(school, color, alt_color) %>%
  mutate(school = if_else(str_detect(school,"^San Jos"),"San José State",school)) %>%
  replace_na(replace = list(color = "#ffffff",alt_color = "#ffffff"))

logo_ref <- team_ref %>%
  left_join(colors,by = "school")

usethis::use_data(logo_ref, overwrite = TRUE)
logo_list <- logo_ref$logo
names(logo_list) <- logo_ref$school
usethis::use_data(logo_list, internal = TRUE, overwrite = TRUE)

#
# temp_logos <- teams_csv %>%
#   select(school, abbreviation, logo) %>%
#   filter(logo != "NA,NA") %>%
#   mutate(school = if_else(str_detect(school,"^San Jos"),"San José State",school),
#          type = case_when(school %in% fbs_teams$school ~ "FBS",
#                           school %in% d3_teams ~ "DIII",
#                           TRUE ~ "Other")) %>%
#   #pivot_longer(school:abbreviation,values_to = "school") %>%
#   # group_by(school) %>%
#   # slice(1) %>%
#   # ungroup() %>%
#   arrange(fbs,school) %>% view()
#   select(-name,-fbs)
#
# conferences <- cfbfastR::cfbd_conferences()
# conferences<- conferences %>%
#   mutate(logo = glue::glue("https://a.espncdn.com/i/teamlogos/ncaa_conf/500/{conference_id}.png")) %>%
#   filter(conference_id<100)
#
# temp_conf <- conferences %>%
#   pivot_longer(c(name,abbreviation),values_to = "school") %>%
#   select(-long_name,-conference_id,-name) %>%
#   filter(!is.na(school)) %>%
#   group_by(school) %>%
#   slice(1) %>%
#   ungroup()
#
# combined <- bind_rows(temp_logos,temp_conf,tibble(school = "NCAA",logo = "https://upload.wikimedia.org/wikipedia/en/thumb/c/cf/NCAA_football_icon_logo.svg/2560px-NCAA_football_icon_logo.svg.png"))
#
# logo_list <- combined$logo
# names(logo_list) <- combined$school
#
#
#
# # Check for missing logos
# ind <- NA
#
# for (i in 1:length(logo_list)) {
# tryCatch(
#   expr ={
#     Sys.sleep(.1)
#     magick::image_read(logo_list[[i]])
#     },
#   error = function(e) {
#     message("The following error has occured:")
#     message(e)
#     ind <<- append(ind,i)
#   },
#   finally = {}
#   )
# }
# ind <- ind[-1]
#
# logo_list <- logo_list[-ind]
#
#
# usethis::use_data(logo_list,internal = TRUE, overwrite = TRUE)


team_info
