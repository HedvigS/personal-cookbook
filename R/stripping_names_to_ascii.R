#This scripts removes non-ascii characters from language names so that they are more compatable with certain programs, like SplitsTree.

#This script defines a function and then applies it at the end either to something that's hardcoded in or to the first argument after the call of the script defined in CLI.

creating_stripped_name_cols <- function(df){

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


#making columns with the names of languages, but stripped so it won't cause trouble in applications like SplitsTree
df$Name_stripped <- df$Name %>% 
  stringi::stri_trans_general("latin-ascii") %>% 
  str_replace_all("\\(", "") %>%  
  str_replace_all("\\)", "") %>% 
  str_replace_all("\\-", "") %>% 
  str_replace_all("\\'", "?")

df$Name_stripped_no_spaces <- df$Name_stripped %>% 
  str_replace_all(" ", "_")  

fn_out_name <- paste0(df, "_names.stripped.tsv")

df %>% 
  dplyr::select(Language_ID, Name, Name_stripped, Name_stripped_no_spaces) %>% 
  write_tsv(fn_out_name)
}

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
#  source("make_lang_values_wide_fetch_online.R")
  df <- read_tsv("output_tables/cldf_wide_df.tsv")
  creating_stripped_name_cols(df)
} else if (length(args)==1) {
  df <- args[1] 
  creating_stripped_name_cols(df)
}