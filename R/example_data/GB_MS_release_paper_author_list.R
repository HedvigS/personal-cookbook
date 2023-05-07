library(tidyverse)


author_list <- read_csv("gb_ms_release_paper/Grambank Release paper authors - author list (1).csv")  %>% 
  rename(email_old = email) %>% 
  dplyr::select(-`has confirmed in internal form #2`)

internal_review <- read_tsv("gb_ms_release_paper/Grambank authors internal review form #2 - Formulärsvar 1.tsv") %>% 
  rename(`last name`= `Your last/family name`, email = `Your preferred email for the submission process`) %>% 
  distinct(`last name`, email) %>% 
  mutate(`has confirmed in internal form #2` = "yes")
  
new_author_list <- author_list %>% 
  left_join(internal_review) %>% 
  mutate(email = ifelse(is.na(email), email_old, email)) %>% 
  dplyr::select(-email_old)

write_tsv(new_author_list, "gb_ms_release_paper/new_author_list.tsv", na = "")