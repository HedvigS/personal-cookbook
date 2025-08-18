library(tidyverse)
library(rgrambank)
library(randomcoloR)

glottolog <- read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/refs/heads/master/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(ID = ISO639P3code, Latitude, Longitude)

linguameta <- read_tsv("https://raw.githubusercontent.com/google-research/url-nlp/refs/heads/main/linguameta/linguameta.tsv", show_col_types = F) %>% 
  mutate(writing_systems = str_split(writing_systems, pattern = ",")) %>% 
  unnest(writing_systems) %>% 
  mutate(writing_systems = trimws(writing_systems)) %>% 
  dplyr::select(ID = iso_639_3_code, writing_systems) %>% 
  filter(ID %in% glottolog$ID)

basemap_list  <- rgrambank::basemap_pacific_center(LongLatTable = glottolog, DataTable = linguameta) 

set.seed(293)
color_vector <- randomcoloR::distinctColorPalette(k = length(unique(basemap_list$MapTable$writing_systems)))

map <- basemap_list$basemap +
  geom_jitter(mapping = aes(x = Longitude, y = Latitude, 
                            fill = basemap_list$MapTable$writing_systems, 
                            color =  basemap_list$MapTable$writing_systems), 
              
              size = 1.5, 
              alpha = 0.4, 
              shape = 21) +
  scale_fill_manual(values = color_vector) +
  scale_color_manual(values = color_vector) 

ggsave(plot = map, "linguameta_writing_systems_map.png")
