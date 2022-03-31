
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  jsonlite, #reading json files
  readxl,
  tidyverse,
  stringr, #for string evaluation
  readr #for reading in data files
)

soure("make_lang_values_wide_fetch_online.R")

lg_count_countries_df  <- read_tsv("output_tables/cldf_wide_df.tsv") %>% 
  filter(level == "language") %>% 
  mutate(Countries = str_split(Countries, ";")) %>% 
  unnest(Countries) %>% 
  group_by(Countries) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(`Countries`)) %>% 
  rename("ISO2 Alpha-code" = Countries) 
  
#The excel sheets used here is downlaoded from the link below on 2021-08-26
#https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx
#https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX

UN_pop_DF <- read_excel("example_data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", skip = 16) %>% 
  filter(Type == "Country/Area") %>% 
  dplyr::select("1950", "Country code") %>% 
  dplyr::mutate("1950" = str_extract(`1950`, "[:digit:]*[:punct:][:digit:]{1,3}")) %>% #something really weird is going on with the numbers after the third decimal, let's skip them
  mutate("1950" = as.numeric(`1950`)*1000) #it's in thousands

UN_pop_DF[UN_pop_DF == "..."] <- NA #replace all instances of "..." with NA

UN_country_codes <- read_excel("example_data/WPP2019_F01_LOCATIONS.XLSX", skip = 16) %>% 
  dplyr::select(Name = "Region, subregion, country or area*" , "ISO3 Alpha-code", Type = "Name...7", "Country code" = "Location code") %>% 
  filter(Type == "Country/Area") 

#alpha-3 to alpha-2 table from wikipedia
#https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
country_codes_mapping_df <- read_csv("example_data/wiki_table_country_codes") %>% 
  dplyr::select("ISO2 Alpha-code" = "ISO 3166-1[2]","ISO3 Alpha-code" = "...5")

combined <- UN_pop_DF  %>% 
  left_join(UN_country_codes) %>% 
  dplyr::select("ISO3 Alpha-code", "Population 1950" = "1950") %>% 
  left_join(country_codes_mapping_df) %>% 
  left_join(lg_count_countries_df) %>% 
  rename("Number of languages" = n)  %>% 
  mutate("Population 1950 log10" = log10(`Population 1950`)) %>% 
  mutate("Number of languages log10" = log10(`Number of languages`))

combined %>%
  ggplot(aes(x = `Population 1950` , y = `Number of languages`)) +
  geom_point( ) +
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x')

ggsave("number_of_languags_vs_pop.png")

combined %>%
  ggplot(aes(x = `Population 1950 log10` , y = `Number of languages log10`)) +
  geom_point( ) +
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x')
  
ggsave("number_of_languags_vs_poplog10.png")
