#This script just specifies how one can exclude languages in glottolog which aren't real languages per se. Not that this is different from the level "language". The level "language" is defined by the structure of the data and in part by critiera of language-hood, but there are languoids included in the level "language" which haven't been used as a main communication by a human society and which aren't distinct from all other languages. This script just specifies which languoids need to be removed to slim it down to "only real languages".

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  readr #for reading in data files
)

options(tidyverse.quiet = TRUE)

#combining the tables languages and values from glottolog-cldf into one wide dataframe
source("make_lang_values_wide_fetch_online.R")
glottolog <- read_tsv("cldf_wide_df.tsv")

glottolog %>% 
  filter(level == "language") %>% 
  filter(Family_ID != 'book1242') %>% #removing bookkeeping languages
  filter(Family_ID != 'unat1236') %>% #removing unattested
  filter(Family_ID != 'pidg1258') %>% #removing pidgins
  filter(Family_ID != 'arti1236') %>% #removing artifical
  filter(Family_ID != 'spee1234')  %>% #removing speech register
  write_tsv("output_tables/Glottolog_real_languages.tsv")