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

concept <- "129_moon"
title_concept <- stringr::str_extract(concept, "(?<=_)\\w+") %>% 
  str_to_title()

forms_raw <- read_csv("https://github.com/lexibank/abvd/raw/79a8979e6064e0a0dfce53e2ccb41b32742784c5/cldf/forms.csv", 
                  show_col_types = F) %>% 
  dplyr::filter(Loan == FALSE) %>% 
  dplyr::filter(!is.na(Cognacy)) %>% 
  dplyr::filter(!str_detect(Cognacy, "\\?")) %>% 
  dplyr::select(Language_ID, Parameter_ID, Form, Cognacy, Value) 

#forms_raw$Parameter_ID %>% unique() %>% stringr::str_extract("(?<=_)\\w+") %>% 
#  str_to_title()


forms <- forms_raw %>% 
  dplyr::filter(Parameter_ID == concept) %>% 
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
  summarise(
    lat_middle = (min(Latitude, na.rm = TRUE) + max(Latitude, na.rm = TRUE)) / 2,
    long_middle = (min(Longitude, na.rm = TRUE) + max(Longitude, na.rm = TRUE)) / 2, .groups = "drop"
  )

p <- SH.misc::basemap_EEZ(south = "down", xlim = c(35, 260), ylim = c(-60, 35))  +
  geom_jitter(data = forms, mapping = aes(x = Longitude, y = Latitude, color = Color, fill = Color), 
             alpha = 0.6, shape= 21, size = 3, width = 1.5, height = 1.5) +
  ggrepel::geom_label_repel(data = forms_main_label_df, alpha = 0.6,
                            max.overlaps = 60, segment.color = NA,
                            mapping = aes(x = long_middle, y = lat_middle, label = Form_main, fill = Color, size = n)) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_continuous(range = c(3, 8)) +
  ggtitle(label = paste0(title_concept, " in the Austronesian Basic Vocabulary Database (ABVD)")) +
  annotate("text", 
           x = 36, 
           y = -56, 
           label = "Specifics and attribution:
           Original source: Greenhill, S.J., Blust. R, & Gray, R.D. (2008). The Austronesian Basic Vocabulary Database: 
           From Bioinformatics to Lexomics. Evolutionary Bioinformatics, 4:271-283. 
           This plot was made by Hedvig Skirgård using the data at https://github.com/lexibank/abvd/, 
           commit ref: 79a8979e6064e0a0dfce53e2ccb41b32742784c5.
           ABVD contains non-Austronesian languages (e.g. Tai-Kadai), however on this plot only Austronesian data is included.
           Each point is a wordlist of a given language, coloured by cognacy (word relatedness).
           If a word is assigned more than one cognacy class (e.g. '5, 12'), then the most common class in the overall dataset is chosen. 
           Non-numeric cognacy is ignored (e.g. '1?' or 'x'). Labels are plotted for cognacy groups with more than 3 members. 
           Labels represent the most common word for the group and they appear in the middle of their points.
           Areas with a lot of data (e.g. Vanuatu, Papua New Guinea), sadly cannot be fully displayed in this map. 
           For more information about the data, please visit https://abvd.eva.mpg.de/austronesian/ or https://github.com/lexibank/abvd/ ",
           
           hjust = 0, vjust = 0, 
           size = 2)
p

ggsave(plot = p, filename = paste0("output/ABVD_", title_concept, "_map.png"), width = 14, height = 6)  

