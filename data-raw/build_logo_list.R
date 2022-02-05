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

library(rvest)

url <- "https://www.ncaa.com/scoreboard/football/d2/2021/05/all-conf"

html <- rvest::read_html(url)

xpath_logo <- '//*[contains(concat( " ", @class, " " ), concat( " ", "gamePod-game-team-logo", " " ))]'
xpath_school <- '//*[contains(concat( " ", @class, " " ), concat( " ", "gamePod-game-team-name", " " ))]'

nodeset_logo <- html %>% rvest::html_elements(xpath = xpath_logo)
nodeset_school <- html %>% rvest::html_elements(xpath = xpath_school)
d2_week5 <- tibble(school = nodeset_school %>% str_remove("<span class=\"gamePod-game-team-name\">") %>%
                     str_remove("</span>"),
                   logo = nodeset_logo %>% str_remove("<img class=\"gamePod-game-team-logo\" src=\"") %>%
                     str_remove("\">"),
                   type = "DII")

url <- "https://www.ncaa.com/scoreboard/football/d2/2021/04/all-conf"

html <- rvest::read_html(url)
nodeset_logo <- html %>% rvest::html_elements(xpath = xpath_logo)
nodeset_school <- html %>% rvest::html_elements(xpath = xpath_school)
d2_week6 <- tibble(school = nodeset_school %>% str_remove("<span class=\"gamePod-game-team-name\">") %>%
                     str_remove("</span>"),
                   logo = nodeset_logo %>% str_remove("<img class=\"gamePod-game-team-logo\" src=\"") %>%
                     str_remove("\">"),
                   type = "DII")


d2_logos <- d2_week5 %>%
  bind_rows(d2_week6) %>%
  group_by(school) %>%
  slice(1) %>%
  mutate(school = str_replace(school,"&amp;","&"))




d3_logos <- tibble(school = d3_teams,
       logo = glue::glue("https://raw.githubusercontent.com/Kazink36/cfbplotR/master/data-raw/dIII/{school}.jpg"),
       type = "DIII")

conferences <- cfbfastR::cfbd_conferences()
conf_logos <- conferences %>%
  filter(conference_id<100 | conference_id == 151) %>%
  transmute(school = case_when(name == "Southern" ~ "Southern Conference",
                               TRUE ~ name),
            logo = glue::glue("https://a.espncdn.com/i/teamlogos/ncaa_conf/500/{conference_id}.png"),
            type = "Conference") %>%
  add_row(school = "NCAA",
          logo = "https://upload.wikimedia.org/wikipedia/en/thumb/c/cf/NCAA_football_icon_logo.svg/2560px-NCAA_football_icon_logo.svg.png",
          type = "Conference")


teams_csv <- read_csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv",skip = 1,col_names = c("row","team_id","school","mascot","abbreviation","alt_name1","alt_name2","alt_name3","conference","division","color","alt_color","logo","logo_dark"))

temp_logos <- teams_csv %>%
  filter(!school %in% fbs_logos$school, !school %in% d3_logos$school, !school %in% d2_logos$school) %>%
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

team_ref <- bind_rows(fbs_logos,fcs_logos,d2_logos,d3_logos,conf_logos)

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

write_csv(logo_ref,"data-raw/logo_ref.csv")

# ATTEMPT TO FIX FCS AND ADD P5 AND G5
team_info <- read_csv(url("https://github.com/Kazink36/cfbplotR/blob/master/data-raw/logo_ref.csv?raw=true"))
cfbd_team_info <- cfbfastR::cfbd_team_info()
p5_teams <- cfbd_team_info %>%
  as_tibble() %>%
  filter((conference %in% c("Pac-12","SEC","ACC","Big Ten","Big 12"))|(school %in% c("BYU","Notre Dame"))) %>%
  pull(school)
g5_teams <- cfbd_team_info %>%
  filter(!(school %in% p5_teams)) %>%
  pull(school)
library(rvest)
url <- "https://www.espn.com/college-football/standings/_/view/fcs-i-aa"
xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "hide-mobile", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "AnchorLink", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "Table__Title", " " ))]'


html <- rvest::read_html(url)

nodes <- html %>% html_nodes(xpath = xpath)
fcs <- bind_rows(lapply(html_attrs(nodes), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
fcs <- fcs %>% mutate(orig = str_remove(nodes," "))
fcs_test<-fcs %>%
  mutate(conf = cumsum(class == "Table__Title")) %>%
  group_by(conf) %>%
  mutate(conf_name = first(orig),
         conf_name = str_remove(conf_name,'<divclass="Table__Title">'),
         conf_name = str_remove(conf_name,"</div>\\n"),
         id = str_extract(href,"[[:digit:]]{1,4}"),
         id = as.numeric(id)) %>%
  filter(class != "Table__Title")
team_info_all <- cfbfastR::cfbd_team_info(only_fbs = FALSE)
conferences <- cfbfastR::cfbd_conferences()
fcs_teams <- fcs_test %>%
  ungroup() %>%
  select(conf_name,id) %>%
  left_join(team_info_all,by = c("id" = "team_id")) %>%
  select(conf_name,id,school) %>%
  left_join(conferences %>% filter(classification == "fcs"), by = c("conf_name" = "long_name")) %>%
  select(team_id = id, school, conference_id, conference = name, long_name = conf_name) %>%
  mutate(division = case_when(conference == "SWAC" & row_number()<n()-5 ~ "East",
                              conference == "SWAC" ~ "West",
                              TRUE ~ NA_character_))





logo_ref<-team_info %>%
  mutate(type = case_when(school %in% p5_teams ~ "P5",
                          school %in% g5_teams ~ "G5",
                          (type == "FCS") & (school %in% fcs_teams$school) ~ "FCS",
                          (type == "FCS") & !(school %in% fcs_teams$school) ~ "Other",
                          TRUE ~ type))

write_csv(logo_ref, "data-raw/logo_ref.csv")
usethis::use_data(logo_ref, overwrite = TRUE)
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


# library(rvest)
# library(tidyverse)
#
#
# url <- "https://teamcolorcodes.com/ncaa-color-codes/"
# xpath <- "//h4~//h4+//p//a"
# html <- read_html(url)
# html %>% html_elements(css = "h4~ h4+ p a") %>%
#   str_replace(.,"&amp;","&") %>%
#   tibble("text" = .) %>%
#   mutate(team = str_extract(text,"([a-zA-Z]+( [a-zA-Z]+)+)<"))
#   separate(text,c("Hello","World"))





  team_info <- read_csv(url("https://github.com/Kazink36/cfbplotR/blob/master/data-raw/logo_ref.csv?raw=true"))

  state_names <- team_info %>%
    filter(str_detect(school,"State")) %>%
    transmute(school = school,
              state = str_replace(school, "State","St\\."))

  state_list <- state_names$school
  names(state_list) <- state_names$state

  team_name_mapping <- team_info$school
  names(team_name_mapping) <- team_info$school
  team_name_mapping <- append(team_name_mapping,c("UL Monroe" = "Louisiana Monroe",
                                    "San Jose State" = "San José State",
                                    "Hawaii" = "Hawai'i",
                                    "Massachusetts" = "UMass",
                                    "UTSA" = "UT San Antonio",
                                    "ASU" = "Arizona State",
                                    "Mississippi" = "Ole Miss",
                                    "N.C. State" = "NC State",
                                    "Miami FL" = "Miami",
                                    "Miami OH" = "Miami (OH)",
                                    "Seattle" = "Seattle U",
                                    "Cal Baptist" = "California Baptist",
                                    "Nicholls St." = "Nicholls",
                                    "Cal St. Fullerton" = "CSU Fullerton",
                                    "Cal St. Bakersfield" = "CSU Bakersfield",
                                    "Penn" = "Pennsylvania",
                                    "Gardner Webb" = "Gardner-Webb",
                                    "UMKC" = "Kansas City",
                                    "St. Francis PA" = "St Francis (PA)",
                                    "Illinois Chicago" = "UIC",
                                    "American" = "American University",
                                    "LIU" = "Long Island University",
                                    "Loyola MD" = "Loyola (MD)",
                                    "Cal St. Northridge" = "CSU Northridge",
                                    "FIU" = "Florida International",
                                    "Nebraska Omaha" = "Nebraska-Omaha",
                                    "San Jose St." = "San José State",
                                    "McNeese St." = "McNeese",
                                    "Grambling St." = "Grambling",
                                    "USC Upstate" = "South Carolina Upstate",
                                    "St. Francis NY" = "St. Francis (BKN)",
                                    "Texas A&M Corpus Chris" = "Texas A&M-CC",
                                    "Tennessee Martin" = "UT Martin",
                                    "Maryland Eastern Shore" = "Maryland-Eastern Shore",
                                    "Bethune Cookman" = "Bethune-Cookman",
                                    "Arkansas Pine Bluff" = "Arkansas-Pine Bluff"
                                    ))
  team_name_mapping <- append(team_name_mapping, state_list)

  usethis::use_data(team_name_mapping, overwrite = TRUE)



#Add for hoopR
  logo_ref <- team_info

  logo_ref <- logo_ref %>% dplyr::bind_rows(tribble(
    ~school, ~logo, ~type, ~color, ~alt_color,
    "American University", "https://a.espncdn.com/i/teamlogos/ncaa/500/44.png","hoopR","#c41130","#c8102e",
    "Bellarmine","https://a.espncdn.com/i/teamlogos/ncaa/500/91.png","hoopR", "#000000","#ffffff",
    "Belmont","https://a.espncdn.com/i/teamlogos/ncaa/500/2057.png","hoopR", "#182142", "#c9262d",
    "Binghamton","https://a.espncdn.com/i/teamlogos/ncaa/500/2066.png","hoopR","#00614A","#f0f0f0",
    "Boston University","https://a.espncdn.com/i/teamlogos/ncaa/500/104.png","hoopR","#cc0000", "#ffffff",
    "Bradley","https://a.espncdn.com/i/teamlogos/ncaa/500/71.png","hoopR","#b70002","#c0c0c0",
    "CSU Bakersfield","https://a.espncdn.com/i/teamlogos/ncaa/500/2934.png","hoopR","#003BAB","#ffffff",
    "CSU Fullerton" ,"https://a.espncdn.com/i/teamlogos/ncaa/500/2239.png", "hoopR","#003767","#ff8300",
    "CSU Northridge" ,"https://a.espncdn.com/i/teamlogos/ncaa/500/2463.png", "hoopR", "#b50000","#ffffff",
    "California Baptist", "https://a.espncdn.com/i/teamlogos/ncaa/500/2856.png", "hoopR","#000000","#ffffff"
  ))


  hoopR_teams <- hoopR::espn_mbb_teams()
new_teams <- hoopR_teams %>%
  filter(!team %in% cfbplotR::logo_ref$school)  %>%
  transmute(school = team, logo = logo, type = "hoopR", color = color,alt_color = alternate_color) %>%
  replace_na(replace = list(color = "ffffff",alt_color = "ffffff")) %>%
  mutate(color = paste0("#",color),
         alt_color = paste0("#",alt_color))
logo_ref <- cfbplotR::logo_ref %>%
  bind_rows(new_teams)
usethis::use_data(logo_ref, overwrite = TRUE)




