#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

#you need to set cldf_folder folder to wherever the clone of the cldf dataset you want to widen is. 



#IMPORTANT NOTE
# There is some sort of kink in the way rcldf() is wrangling the values table such that aligmnet is going wrong and two sources are mucking up the columns and rows. For this reason, I'm inner-joining the two datasetes rather than full join. See log of error here: https://github.com/SimonGreenhill/rcldf/issues/11

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  jsonlite, #reading json files
  stringr, #for string evaluation
  cli,
  readr #for reading in data files
)

pacman::p_load_gh("SimonGreenhill/rcldf")

cat("This script requires that you have a clone of a cldf repos. In this case: https://github.com/glottolog/glottolog-cldf.\n")

#reading in cldf data via rcldf
cldf_folder <- "../../../glottolog/glottolog-cldf/cldf/"

cldf_json <- paste0(cldf_folder, "cldf-metadata.json")

cldf_object <- rcldf::cldf(cldf_json)

values <- cldf_object$tables$ValueTable %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") #making long data wide

languages <- cldf_object$tables$LanguageTable

#The languages-table from glottolog-cldf contains a paramter called "Language_ID" which is NOT the same as the parameter "Language_ID" in the values tables. This parameter is in fact the language leveled parent of a dialect. In order to avoid confusion, let's rename the parameter in the languages tables to the more transparent "Language_level_ID". This set-up first test if this is indeed a problem (i.e. if this is glottolog-cldf) and only does the renaming then.

if(str_detect(cldf_folder, "glottolog")) {
  languages <- languages %>% 
    rename(Language_level_ID = Language_ID) %>% 
    rename(Language_ID = ID)
} else{  languages <- languages %>% 
  rename(Language_ID = ID)}

cldf_wide_df <- dplyr::inner_join(values,languages) 

write_tsv(cldf_wide_df, "cldf_wide_df.tsv")