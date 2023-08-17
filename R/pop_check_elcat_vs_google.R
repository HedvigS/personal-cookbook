source("fun_def_h_load.R")

h_load(pkg = c("tidyverse", "viridis"))

google <- read_tsv("https://raw.githubusercontent.com/google-research/url-nlp/main/language_metadata/data.tsv", show_col_types = F) %>% 
  dplyr::select(Glottocode = `Glottocode (Glottolog)`, Speakers_google = `Number of speakers (rounded)`)

elcat_values <- read_csv("https://raw.githubusercontent.com/cldf-datasets/elcat/main/cldf/values.csv", show_col_types = F) %>% 
  filter(Parameter_ID == "speaker_number") %>% 
  dplyr::select(Language_ID, Parameter_ID, Value)

elcat_lgs <- read_csv("https://raw.githubusercontent.com/cldf-datasets/elcat/main/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = ID, Glottocode)

elcat_code <- read_csv("https://raw.githubusercontent.com/cldf-datasets/elcat/main/cldf/codes.csv", show_col_types = F) %>% 
  filter(Parameter_ID == "speaker_number") %>% 
  dplyr::select(ID, Description) %>% 
  separate(ID, into = c("Parameter_ID", "Value"), sep = "-") %>%  
  dplyr::select(Value, Description)
  
elcat <- full_join(elcat_values, elcat_lgs, relationship = "many-to-many", by = "Language_ID") %>% 
  full_join(elcat_code, relationship = "many-to-many", by = "Value") %>% 
  dplyr::select(Glottocode, Value, Speakers_elcat = Description) %>% 
  distinct()

combined <- full_join(google, elcat) %>% 
  distinct() %>% 
  mutate(Value = as.numeric(Value))

combined$Speakers_elcat <- fct_reorder(combined$Speakers_elcat, desc(combined$Value), .na_rm = F)

combined$Speakers_google <- factor(combined$Speakers_google, levels = 
c(NA,
"<10K",
"10K-100K", 
"10000",
"50000",
"80000",
"100000",
"200000",
"300000",
"400000",
"500000",
"600000",
"700000",
"800000",
"900000",
"1000000",
"2000000",
"3000000",
"6000000",
"35000000")
)

combined %>% 
  filter(!is.na(Value)) %>% 
  filter(!is.na(Speakers_google)) %>% 
  ggplot() +
  geom_point(mapping = aes(x = Speakers_google, y = Speakers_elcat, color = Speakers_google, fill = Speakers_elcat), stroke = 1, shape = 21) +
  theme_classic() +
  theme(legend.position = "None", 
    axis.text.x = element_text(hjust = 1, angle = 70), 
        text = element_text(size = 14)) +
  coord_fixed() 

ggsave("output/google_vs_elcat_pop.png")




