
#something is wrong with rcldf, I don't know what
#library(devtools)
#install_github("SimonGreenhill/rcldf", dependencies = TRUE)
#library(rcldf)
#cldf_object <- rcldf::cldf( "https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip")
#cldf_object <- rcldf::cldf( "output/processed_data/glottolog_5/metadata.json")

#therefore, here comes Hedvig code with tidyverse
library(tidyverse)
library(reshape2)
library(fs)

source("https://raw.githubusercontent.com/HedvigS/Oceanic_computational_ASR/main/code/fun_def_get_zenodo.R")

glottolog_5_fn  <- "https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip"
exdir <- "output/processed_data/glottolog_5/"

get_zenodo_dir(url = glottolog_5_fn, exdir = exdir)

values <- read_csv(file = paste0(exdir, "cldf/values.csv"), show_col_types = F)
languages <- read_csv(file = paste0(exdir, "cldf/languages.csv"), show_col_types = F)


#reading in data and making it wide
values <- values %>% 
  mutate(Value = ifelse(Parameter_ID == "aes", Code_ID, Value)) %>% 
  mutate(Value = ifelse(Parameter_ID == "med", Code_ID, Value)) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>%  #making long data wide %>% 
  mutate(med = str_replace(med, "med-", "")) %>% 
  mutate(aes = str_replace(aes, "aes-", "")) 

#The languages-table from glottolog-cldf contains a paramter called "Language_ID" which is NOT the same as the parameter "Language_ID" in the values tables. This parameter is in fact the language leveled parent of a dialect. In order to avoid confusion, let's rename the parameter in the languages tables to the more transparent "Language_level_ID". This set-up first test if this is indeed a problem (i.e. if this is glottolog-cldf) and only does the renaming then.

  languages <- languages %>% 
    dplyr::rename(Language_level_ID = Language_ID) %>% 
    dplyr::rename(Language_ID = ID)

cldf_wide_df <- dplyr::full_join(values,languages, by = "Language_ID") 

cldf_wide_df %>% 
  write_tsv("output/processed_data/glottolog_5_wide.tsv")
