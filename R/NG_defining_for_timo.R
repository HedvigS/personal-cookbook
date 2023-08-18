library(tidyverse)
library(readODS)

subset <- read_tsv("output_tables/cldf_wide_df_glottolog_4.8.tsv") %>% 
  filter(level == "language") %>% 
  filter(Macroarea == "Papunesia"|
           Macroarea == "Australia")

subset %>% 
  dplyr::select(Glottocode, Longitude, Latitude) %>% 
write_csv("output_tables/glottolog_lgs_papunesia_australia.csv")

sheet1 <- read_ods("example_data/NG_for_timo.ods", sheet = 1, col_types = NA)
sheet2 <- read_ods("example_data/NG_for_timo.ods", sheet = 2, col_types = NA)
sheet3 <- read_ods("example_data/NG_for_timo.ods", sheet = 3, col_types = NA)
sheet4 <- read_ods("example_data/NG_for_timo.ods", sheet = 4, col_types = NA)
sheet5 <- read_ods("example_data/NG_for_timo.ods", sheet = 5, col_types = NA)
sheet6 <- read_ods("example_data/NG_for_timo.ods", sheet = 6, col_types = NA)
sheet7 <- read_ods("example_data/NG_for_timo.ods", sheet = 7, col_types = NA)
sheet8 <- read_ods("example_data/NG_for_timo.ods", sheet = 8, col_types = NA)
sheet9 <- read_ods("example_data/NG_for_timo.ods", sheet = 9, col_types = NA)

sheets <- sheet1 %>% 
  full_join(sheet2, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet3, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet4, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet5, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet6, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet7, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet8, by = c("Area",  "Longitude","Latitude")) %>% 
  full_join(sheet9, by = c("Area",  "Longitude","Latitude")) 
  
sheets %>% 
  filter(Area == "NG area") %>% 
  mutate(Latitude = as.numeric(Latitude)) %>% 
  mutate(Longitude = as.numeric(Longitude)) %>% 
  full_join(subset) %>% 
  dplyr::select(Name, Glottocode, Area, Longitude, Latitude, classification, Countries, Family_ID) %>%
  write_tsv("output_tables/NG_area_australia_papunesia_glottolog_4.8.tsv")


