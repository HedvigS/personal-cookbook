#This scripts removes non-ascii characters from language names so that they are more compatable with certain programs, like SplitsTree

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  stringr, #for manipulating strings
  stringi, #for latin-ascii conversion
  readr #for reading in data files
)

options(tidyverse.quiet = TRUE)

#combining the tables languages and values from glottolog-cldf into one wide dataframe
source("make_lang_values_wide_fetch_online.R.R")
glottolog <- read_tsv("cldf_wide_df.tsv")

#making columns with the names of languages, but stripped so it won't cause trouble in applications like SplitsTree
glottolog$Name_stripped <- glottolog$Name %>% 
  stringi::stri_trans_general("latin-ascii") %>% 
  str_replace_all("\\(", "") %>%  
  str_replace_all("\\)", "") %>% 
  str_replace_all("\\-", "") %>% 
  str_replace_all("\\'", "?")

glottolog$Name_stripped_no_spaces <- glottolog$Name_stripped %>% 
  str_replace_all(" ", "_")  

glottolog %>% 
  dplyr::select(Language_ID, Name, Name_stripped, Name_stripped_no_spaces) %>% 
  write_tsv("output_tables/glottolog_names_stripped.tsv")