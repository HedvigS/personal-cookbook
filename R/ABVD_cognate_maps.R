library(dplyr)
library(readr)
library(stringr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(ggrepel)

LanguageTable <- read_csv("https://github.com/lexibank/abvd/raw/79a8979e6064e0a0dfce53e2ccb41b32742784c5/cldf/languages.csv", 
                  show_col_types = F) %>% 
  dplyr::select(ID, Name, Longitude, Latitude, Family)

concept <- "125_salt"

forms_raw <- read_csv("https://github.com/lexibank/abvd/raw/79a8979e6064e0a0dfce53e2ccb41b32742784c5/cldf/forms.csv", 
                  show_col_types = F) %>% 
  dplyr::filter(Loan == FALSE) %>% 
  dplyr::filter(!is.na(Cognacy)) %>% 
  dplyr::filter(!str_detect(Cognacy, "\\?")) %>% 
  dplyr::select(Language_ID, Parameter_ID, Form, Cognacy, Value) %>% 
  dplyr::filter(Parameter_ID == concept) 

forms <- forms_raw %>% 
  dplyr::mutate(Cognacy_sep = stringr::str_split(Cognacy, ",") %>% map(trimws)) %>% 
  tidyr::unnest(Cognacy_sep) %>% 
  dplyr::group_by(Parameter_ID, Cognacy_sep) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::group_by(Language_ID, Parameter_ID, Form) %>% 
  slice_max(order_by = n, n = 1) %>% 
  dplyr::select(Language_ID, Parameter_ID, Form, Cognacy = Cognacy_sep, Value) %>% 
  dplyr::mutate(Cognacy = as.numeric(Cognacy)) %>% 
  dplyr::mutate(Cognacy = as.character(Cognacy)) 
  
forms <- forms %>% 
  left_join(LanguageTable, by = c("Language_ID" = "ID")) %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  filter(Family == "Austronesian") %>% 
  dplyr::mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) 

set.seed(2016)
color_vector <- randomcoloR::distinctColorPalette(k = length(unique(forms$Cognacy)))

forms <- forms %>% 
  dplyr::mutate(
  Cognacy_factor = factor(Cognacy),
  Color = color_vector[as.integer(Cognacy_factor)]
)

forms_main_label_df <- forms %>% 
  dplyr::group_by(Parameter_ID, Cognacy, Form, Color) %>% 
  dplyr::mutate(n = n()) %>% 
  group_by(Parameter_ID, Cognacy, Color) %>% 
  dplyr::slice_max(order_by = n, n = 1) %>% 
  group_by(Parameter_ID, Cognacy, Color) %>% 
  dplyr::slice_sample(n = 1) %>% 
  dplyr::filter(n > 2) %>% 
  dplyr::rename(Form_main = Form) %>% 
  dplyr::distinct(Parameter_ID, Form_main, Cognacy, Color, n) %>% 
  dplyr::mutate(n = pmax(n, 3)) %>% 
  dplyr::mutate(n = log10(n)) %>% 
  dplyr::left_join(forms, by = c("Parameter_ID", "Cognacy", "Color")) %>% 
  group_by(Parameter_ID, Form_main, Cognacy, Color, n) %>% 
  summarise(long_mean = mean(Longitude, na.rm = T), 
            lat_mean = mean(Latitude, na.rm = T), .groups = "drop")

p <- SH.misc::basemap_EEZ(south = "down", xlim = c(35, 260), ylim = c(-60, 35))  +
  geom_jitter(data = forms, mapping = aes(x = Longitude, y = Latitude, color = Color, fill = Color), 
             alpha = 0.8, shape= 21, size = 3) +
  ggrepel::geom_label_repel(data = forms_main_label_df, alpha = 0.6,
                            max.overlaps = 50, 
                            mapping = aes(x = long_mean, y = lat_mean, label = Form_main, fill = Color, size = n)) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_continuous(range = c(3, 8)) +
  ggtitle(label = paste0(concept, " in the Austronesian Basic Vocabulary Database"), 
          subtitle = "(version = 79a8979e6064e0a0dfce53e2ccb41b32742784c5)")

p

ggsave(plot = p, filename = paste0("output/ABVD_", concept, "_map.png"), width = 14, height = 6)  

