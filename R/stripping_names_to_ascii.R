#This scripts removes non-ascii characters from language names so that they are more compatable with certain programs, like SplitsTree.

#This script defines a function and then applies it at the end either to something that's hardcoded in or to the first argument after the call of the script defined in CLI.

#If you already have a specific file in mind, you can hardcoded it in here. If you don't but choose the CLI path, don't worry it won't be used if you've given an argument in CLI.
fn_hardcoded <- "output_tables/cldf_wide_df.tsv"

#If you don't have the specific file "output_tables/cldf_wide_df.tsv" and no other test file to run, run the line below.
#  source("make_lang_values_wide_fetch_online.R")


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

df %>% 
  dplyr::select(Language_ID, Name, Name_stripped, Name_stripped_no_spaces) %>% 
  write_tsv("table_names.stripped.tsv")
}

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  df <- read.delim(fn_hardcoded, sep = '\t')
  creating_stripped_name_cols(df)
} else if (length(args)==1) {
  #there is only one argument that matters, the file path to a df that has at least a Language_ID and Name col and is in tsv format
  df <-  read.delim(args[1], sep = '\t',)
  creating_stripped_name_cols(df)
}