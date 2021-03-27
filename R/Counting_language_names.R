#This script counts how often words occur in language names in Glottolog. It only takes into account the "main name" and does not include dialects, proto-languages or other non-languages (artifical, unnatested etc).

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  tidyr, #for unnesting
  readr #for reading in data files
)

options(tidyverse.quiet = TRUE)

#combining the tables languages and values from glottolog-cldf into one wide dataframe
source("make_lang_values_wide_fetch_online.R.R")
glottolog <- read_tsv("cldf_wide_df.tsv")

glottolog %>% 
  filter(level == "language") %>% 
  filter(Family_ID != 'book1242') %>% #removing bookkeeping languages
  filter(Family_ID != 'unat1236') %>% #removing unattested
  filter(Family_ID != 'pidg1258') %>% #removing pidgins
  filter(Family_ID != 'arti1236') %>% #removing artifical
  filter(Family_ID != 'spee1234')  %>% #removing speech register
  mutate(Name_split = str_split(Name, " ")) %>%  #splitting by spaces
  unnest(Name_split) %>%  #making each "word" a separate line
  group_by(Name_split) %>% #grouping by words
  summarise(n = n()) %>% #counting freq of words
  arrange(desc(n)) %>% #ordering list from most frequent to least
  write_tsv("Glottolog_name_freq.tsv")