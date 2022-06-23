source("fun_def_h_load.R")

h_load(pkg = c("glottospace", "tidyverse", "reshape2"))

x_values <- read.csv("https://raw.githubusercontent.com/cldf-datasets/phoible/master/cldf/values.csv") 
x_langs <- read.csv("https://raw.githubusercontent.com/cldf-datasets/phoible/master/cldf/languages.csv") 
x_wide <- x_values %>% 
  distinct(Language_ID, Parameter_ID, .keep_all = T) %>% #removing duplicate data points for the same language and paramter
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
  rename(ID = Language_ID) %>% 
  full_join(dplyr::select(x_langs, ID, glottocode = Glottocode), by = "ID") %>% 
  dplyr::select(-ID) %>% 
  dplyr::select(glottocode, everything())

y_values <- read.csv("https://raw.githubusercontent.com/lexibank/abvdoceanic/main/cldf-structure/values.csv")
y_langs <- read.csv("https://raw.githubusercontent.com/lexibank/abvdoceanic/main/cldf-structure/languages.csv")
y_wide <- y_values %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
  rename(ID = Language_ID) %>% 
  full_join(dplyr::select(y_langs, ID, glottocode = Glottocode), by = "ID") %>% 
  dplyr::select(-ID) %>% 
  dplyr::select(glottocode, everything())


glottojoin_language_level <- function(x, y , duplicate_action = c("keep_least_missing", "keep_all")){
#  x <- x_wide
#  y <- y_wide
#  duplicate_action = "keep_least_missing"

glottolog_df_fn <- "output_tables/cldf_wide_df.tsv"
if(!file.exists("output_tables/cldf_wide_df.tsv")){
  source("make_lang_values_wide_fetch_online.R")
}

glottolog_df <- read_tsv(glottolog_df_fn, show_col_types = F) %>% 
  dplyr::select(glottocode = Glottocode, Language_level_ID)

#checking if the first col is something we recognise as a glottocode
  if(all(colnames(x[1]) %in% c("Language_ID", "glottocode", "Glottocode", "ID"))){
  #rename the first col to glottocode
      colnames(x) <- c("glottocode", colnames(x[,-1]))}else{
    cat(paste0("The first column of the x-dataframe wasn't Language_ID, glottocode, ID or Glottocode.\n"))
        stop()
  }
  
  if(all(colnames(y[1]) %in% c("Language_ID", "glottocode", "Glottocode", "ID"))){
    #rename the first col to glottocode
    colnames(y) <- c("glottocode", colnames(y[,-1]))}else{
      cat(paste0("The first column of the y-dataframe wasn't Language_ID, glottocode, ID or Glottocode.\n"))
      stop()
    }
  
overlap_x_glottolog <- full_join(glottolog_df, x, by = "glottocode") %>% nrow()
if(overlap_x_glottolog  == 0){
  cat(paste0("The x-data frame has no overlap with glottolog, something is wrong.\n"))
  stop()
}  
if(overlap_x_glottolog != nrow(x)){
  warning("The are rows in x-dataframe which don't overlap with glottolog, i.e. probably not valid glottocodes. Go check it out please. Ignoring those for now\n")
}

overlap_y_glottolog <- full_join(glottolog_df, y, by = "glottocode") %>% nrow()
if(overlap_y_glottolog  == 0){
  cat(paste0("The y-data frame has no overlap with glottolog, something is wrong.\n"))
  stop()
}  
if(overlap_y_glottolog != nrow(y)){
  warning("The are rows in y-dataframe which don't overlap with glottolog, i.e. probably not valid glottocodes. Go check it out please. Ignoring those for now\n")}

if(duplicate_action == "keep_least_missing"){
  x$na_prop <- apply(x[,-1], 1, function(x) mean(is.na(x)))
  y$na_prop <- apply(y[,-1], 1, function(x) mean(is.na(x)))

x <- x %>% 
    inner_join(glottolog_df, by = "glottocode") %>% 
    filter(!is.na(glottocode)) %>% #subsetting to only valid glottocodes
    group_by_("Language_level_ID") %>% 
    sample_n(size = 1, weight = 1 -na_prop) %>% 
  dplyr::select(-glottocode, -na_prop) %>% 
  dplyr::select(glottocode = Language_level_ID, everything())

y <- y %>% 
  inner_join(glottolog_df, by = "glottocode") %>% 
  filter(!is.na(glottocode)) %>% #subsetting to only valid glottocodes
  group_by_("Language_level_ID") %>% 
  sample_n(size = 1, weight = 1 -na_prop) %>% 
  dplyr::select(-glottocode, -na_prop) %>% 
  dplyr::select(glottocode = Language_level_ID, everything())

joined_df <- full_join(x, y, by = "glottocode")
}
if(duplicate_action == "keep_all"){

  x <- x %>% 
    inner_join(glottolog_df, by = "glottocode") %>% 
    dplyr::select(-glottocode) %>% 
    dplyr::select(glottocode = Language_level_ID, everything())

  y <- y %>% 
    inner_join(glottolog_df, by = "glottocode") %>% 
    dplyr::select(-glottocode) %>% 
    dplyr::select(glottocode = Language_level_ID, everything())
  
joined_df <- full_join(x, y, by = "glottocode")
  
  
}

joined_df
}