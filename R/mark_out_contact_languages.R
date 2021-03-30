#This script filters to "contact" languages. Glottolog contains the pseudo-families "pidgin" and "mixed language", but unlike Ethnologue creole are spliced into the language family of their lexifier. This script is an example for how you can get a list of glottocodes that pertain to contact languages.

#contact languages are teased out by 
# a) being in the known contact language pseudo families "Pidgin" and "Mixed Language"
# b) having "creol" or "kriol" in the name
# c) being in the language set for the Atlas of Pidgin and Creole Language Structures (APiCS) 

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  tidyr, #for unnesting
  readr #for reading in data files
)

options(tidyverse.quiet = TRUE)

#combining the tables languages and values from glottolog-cldf into one wide dataframe
source("make_lang_values_wide_fetch_online.R")
glottolog <- read_tsv("cldf_wide_df.tsv")

#APICS
apics_languages_url <- "https://raw.githubusercontent.com/cldf-datasets/apics/master/cldf/languages.csv"

apics_languages <- read_csv(apics_languages_url) %>% 
  distinct(Glottocode) %>% 
  filter(!is.na(Glottocode)) %>% 
  rename(Language_ID = Glottocode)


#filtering
contact_langs_df <- glottolog %>% 
    filter(Family_ID == "pidg1258"|
           Family_ID == "mixe1287"|
           str_detect(Name, "Creol")|
           str_detect(Name, "Kriol")) %>%  
             full_join(apics_languages) %>% 
             distinct(Language_ID)

contact_langs_df %>% 
write_tsv("output_tables/Glottolog_contact_languages.tsv")