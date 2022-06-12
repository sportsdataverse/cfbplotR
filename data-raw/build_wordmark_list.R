library(tidyverse)
library(cfbplotR)
# Add watermarks and switch to sportsdataverse repo

logo_ref %>%
  #filter(str_detect(logo,"Kazink")) %>%
  mutate(logo = str_replace(logo,"Kazink36/cfbplotR/master","sportsdataverse/cfbplotR/main")) %>%
  write_csv("logo_ref.csv")

#manually match up wordmarks with proper school, since there are missing schools and were manually collected
# added order column for sorting that gets removed below and wordmark column with filenames of the wordmark png's
list.files(path = "data-raw/wordmarks", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#
logo_ref <- read_csv("data-raw/logo_ref_2.csv")
logo_ref <- logo_ref %>%
  mutate(wordmark = str_trim(wordmark)) %>%
  mutate(wordmark = ifelse(!is.na(wordmark),
                           paste0("https://raw.githubusercontent.com/sportsdataverse/cfbplotR/main/data-raw/wordmarks/",wordmark),
                           NA_character_)) %>%
  select(-Order)

logo_list <- logo_ref$logo
names(logo_list) <- logo_ref$school

wordmark_list <- logo_ref %>% filter(!is.na(wordmark)) %>% pull(wordmark)
names(wordmark_list) <- logo_ref %>% filter(!is.na(wordmark)) %>% pull(school)

usethis::use_data(logo_list,wordmark_list, internal = TRUE, overwrite = TRUE)
usethis::use_data(logo_ref, overwrite = TRUE)
write_csv(logo_ref,"data-raw/logo_ref.csv")


logo_ref %>%
  dplyr::filter(!is.na(wordmark)) %>%
  transmute(school = school,
            logo = school,
            wordmark = school
            ) %>%
  gt::gt() %>%
  gt_fmt_cfb_wordmark(wordmark) %>%
  gt_fmt_cfb_logo(logo)
