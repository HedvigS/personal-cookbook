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

#combining the tables languages and values from glottolog_df-cldf into one wide dataframe.
#this can be replaced with any list of Language_IDs, long and lat
source("make_lang_values_wide_fetch_online.R.R")
glottolog_df <- read_tsv("cldf_wide_df.tsv") %>% 
  dplyr::select(Language_ID, Longitude, Latitude)

##Adding in areas of linguistic contact from AUTOTYP

AUTOTYP <- read_csv("https://raw.githubusercontent.com/autotyp/autotyp-data/master/data/Register.csv") %>% 
  dplyr::select(Language_ID = Glottocode, Area, Longitude, Latitude) %>% 
  filter(Language_ID != "balk1252") %>% #There's a set of languages in autotyp that have more than one area, for now they're just hardcoded excluded in these lines
  filter(Language_ID != "east2295") %>% 
  filter(Language_ID != "indo1316") %>% 
  filter(Language_ID != "kyer1238") %>% 
  filter(Language_ID != "mart1256") %>% 
  filter(Language_ID != "minn1241") %>% 
  filter(Language_ID != "noga1249") %>% 
  filter(Language_ID != "oira1263") %>% 
  filter(Language_ID != "peri1253") %>% 
  filter(Language_ID != "taha1241") %>% 
  filter(Language_ID != "tibe1272") %>% 
  filter(Language_ID != "till1254") %>% 
  filter(Language_ID != "toho1245") %>% 
  filter(Language_ID != "kati1270")

#This next bit where we find the autotyp areas of languages was written by Seán Roberts
# We know the autotyp-area of langauges in autotyp and their long lat. We don't know the autotyp area of languages in Glottolog. We also can't be sure that the long lat of languoids with the same glottoids in autotyp and glottolog_df have the exact identical long lat. First let's make two datasets, one for autotyp languages (hence lgs where we know the area) and those that we wish to know about, the Glottolog ones.

lgs_with_known_area <- as.matrix(AUTOTYP[!is.na(AUTOTYP$Area),c("Longitude","Latitude")])
rownames(lgs_with_known_area) <- AUTOTYP[!is.na(AUTOTYP$Area),]$Language_ID

known_areas <- AUTOTYP %>% 
  dplyr::filter(!is.na(Area)) %>% 
  dplyr::select(Language_ID, Area) %>% 
  distinct() %>% 
  dplyr::select(AUTOTYP_Language_ID = Language_ID, everything())

rm(AUTOTYP)

lgs_with_unknown_area <- as.matrix(glottolog_df[,c("Longitude","Latitude")])
rownames(lgs_with_unknown_area) <- glottolog_df$Language_ID

# For missing, find area of closest langauge
atDist <- rdist.earth(lgs_with_known_area,lgs_with_unknown_area, miles = F)

rm(lgs_with_known_area, lgs_with_unknown_area)

df_matched_up <- as.data.frame(unlist(apply(atDist, 2, function(x){names(which.min(x))})), stringsAsFactors = F) %>% 
  rename(AUTOTYP_Language_ID = `unlist(apply(atDist, 2, function(x) {     names(which.min(x)) }))`)

glottolog_df_with_AUTOTYP <- df_matched_up %>% 
  tibble::rownames_to_column("Language_ID") %>%
  full_join(known_areas) %>% 
  right_join(glottolog_df) %>% 
  dplyr::select(-AUTOTYP_Language_ID) %>% 
  rename(AUTOTYP_area = Area) 

glottolog_df_with_AUTOTYP %>% 
  write_tsv("glottolog_AUTOTYPE_areas.tsv")