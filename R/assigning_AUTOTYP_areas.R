#This script assigns all languages in glottolog_df to their nearest AUTOTYP area

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  fields, #for calculating distances
  tibble, #for manipulating rownames
  readr #for reading in data files
)

options(tidyverse.quiet = TRUE)

glottolog_df <- readr::read_csv("https://github.com/glottolog/glottolog-cldf/raw/refs/tags/v5.0/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = Glottocode, Longitude, Latitude) %>% 
  dplyr::filter(!is.na(Longitude))

##Adding in areas of linguistic contact from AUTOTYP

AUTOTYP <- read_csv("https://raw.githubusercontent.com/autotyp/autotyp-data/master/data/csv/Register.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = Glottocode, Area, Longitude, Latitude) %>% 
  group_by(Language_ID) %>% 
  sample_n(1) #when a language is assigned to more than one area, pick randomly.


#This next bit where we find the autotyp areas of languages was written by Seán Roberts
# We know the autotyp-area of langauges in autotyp and their long lat. We don't know the autotyp area of languages in Glottolog. We also can't be sure that the long lat of languoids with the same glottoids in autotyp and glottolog_df have the exact identical long lat. First let's make two datasets, one for autotyp languages (hence lgs where we know the area) and those that we wish to know about, the Glottolog ones.

lgs_with_known_area <- as.matrix(AUTOTYP[!is.na(AUTOTYP$Area),c("Longitude","Latitude")])
rownames(lgs_with_known_area) <- AUTOTYP[!is.na(AUTOTYP$Area),]$Language_ID

known_areas <- AUTOTYP %>% 
  dplyr::filter(!is.na(Area)) %>% 
  dplyr::select(AUTOTYP_Language_ID = Language_ID, Area) %>% 
  dplyr::distinct() 

rm(AUTOTYP)

lgs_with_unknown_area <- glottolog_df %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix()
  
# For missing, find area of closest langauge
atDist <- rdist.earth(lgs_with_known_area,lgs_with_unknown_area, miles = F)

rm(lgs_with_known_area, lgs_with_unknown_area)

df_matched_up <- as.data.frame(unlist(apply(atDist, 2, function(x){names(which.min(x))})), stringsAsFactors = F) %>% 
  rename(AUTOTYP_Language_ID = `unlist(apply(atDist, 2, function(x) {     names(which.min(x)) }))`)

glottolog_df_with_AUTOTYP <- df_matched_up %>% 
  tibble::rownames_to_column("Language_ID") %>%
  full_join(known_areas, by = "AUTOTYP_Language_ID") %>% 
  right_join(glottolog_df, relationship = "many-to-many", by = "Language_ID") %>% 
  dplyr::select(-AUTOTYP_Language_ID) %>% 
  rename(AUTOTYP_area = Area) 

glottolog_df_with_AUTOTYP %>% 
  write_tsv("output/output_tables/glottolog_AUTOTYP_areas.tsv")
