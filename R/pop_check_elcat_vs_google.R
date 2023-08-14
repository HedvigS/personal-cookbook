

google <- read_tsv("../../../google-research/url-nlp/language_metadata/data.tsv", show_col_types = F) %>% 
  dplyr::select(Glottocode = `Glottocode (Glottolog)`, Speakers_google = `Number of speakers (rounded)`)

elcat_values <- read_csv("../../../cldf-datasets/elcat/cldf/values.csv", show_col_types = F) %>% 
  filter(Parameter_ID == "speaker_number") %>% 
  dplyr::select(Language_ID, Parameter_ID, Value)

elcat_lgs <- read_csv("../../../cldf-datasets/elcat/cldf/languages.csv", show_col_types = F) %>% 
  dplyr::select(Language_ID = ID, Glottocode)

elcat_code <- read_csv("../../../cldf-datasets/elcat/cldf/codes.csv", show_col_types = F) %>% 
  filter(Parameter_ID == "speaker_number") %>% 
  dplyr::select(ID, Description) %>% 
  separate(ID, into = c("Parameter_ID", "Value"), sep = "-") %>%  
  dplyr::select(Value, Description)
  
elcat <- full_join(elcat_values, elcat_lgs, relationship = "many-to-many", by = "Language_ID") %>% 
  full_join(elcat_code, relationship = "many-to-many", by = "Value") %>% 
  dplyr::select(Glottocode, Value, Speakers_elcat = Description) %>% 
  distinct()

combined <- inner_join(google, elcat) %>% 
  distinct() %>% 
  filter(!is.na(Value)) %>% 
  filter(!is.na(Speakers_google)) %>% 
  mutate(Value = as.numeric(Value))

combined$Speakers_elcat <- fct_reorder(combined$Speakers_elcat, desc(combined$Value))

combined$Speakers_google <- factor(combined$Speakers_google, levels = 
c(
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
  ggplot() +
  geom_point(mapping = aes(x = Speakers_google, y = Speakers_elcat)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70))

ggsave("google_vs_elcat_pop.png")

