library(tidyverse)
library(reshape2)

#glottolog
glottolog <- read_tsv("output_tables/cldf_wide_df.tsv") %>% 
  dplyr::select(Glottocode, Name, Family_ID, level)

glottolog_fams <- glottolog %>% 
  filter(level == "family") %>% 
  dplyr::select(Family_ID = Glottocode, Name_family = Name)

glottolog  <- glottolog %>% 
  left_join(glottolog_fams)

#tng
abvd_lgs  <- read_csv("https://raw.githubusercontent.com/lexibank/abvd/master/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = ID, Glottocode)

abvd_forms <- read_csv("https://raw.githubusercontent.com/lexibank/abvd/master/cldf/forms.csv", show_col_types = F)


abvd_water_words <- abvd_forms %>% 
  filter(Parameter_ID == "122_water") %>% 
  dplyr::select(Parameter_ID, Value, Cognacy, Language_ID) %>% 
  left_join(abvd_lgs) %>% 
  left_join(glottolog) 

counts <- abvd_water_words %>% 
  group_by(Family_ID, Cognacy) %>% 
  summarise(n = n(),
            paste0(unique(Value),collapse = ", ")) 

water_words %>% 
  write_csv("output/water_words_abvd.csv", na = "")

#tng
tng_forms <- read_csv("https://raw.githubusercontent.com/lexibank/transnewguineaorg/master/cldf/forms.csv", show_col_types = F)

tng_lgs <- read_csv("https://raw.githubusercontent.com/lexibank/transnewguineaorg/master/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = ID, Glottocode)

tng_water_words <- tng_forms %>% 
  filter(Parameter_ID == "94_water") %>% 
  dplyr::select(Parameter_ID, Value, Cognacy, Language_ID) %>% 
  left_join(tng_lgs) %>% 
  left_join(glottolog) 

tng_water_words %>% 
  write_csv("output/water_words_tng.csv", na = "")

tng_water_parameters <- tng_forms %>% 
  filter(str_detect(Parameter_ID, "water")) %>% 
           distinct(Parameter_ID) 
  

