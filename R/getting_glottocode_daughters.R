library(tidyverse)

wish_list_nodes <-data.frame(parents = c("eski1264", "ural1272", "indo1319", "more1255", "sepi1257"))

Sahul_survey <- read_tsv("example_data/Sahul_structure_wide.tsv", show_col_types = F) %>% 
  dplyr::select(Language_ID = glottocode)

glottolog_df <- read_tsv("output_tables/cldf_wide_df.tsv", show_col_types = F) %>% 
  dplyr::select(Language_ID, subclassification, Language_level_ID, level, classification) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID))

Sahul_with_extra <- Sahul_survey %>% 
  left_join(glottolog_df, by = "Language_ID")

#family_matches

Sahul_with_extra_parents_unnested <- Sahul_with_extra %>% 
  mutate(parents = str_extract_all(classification, "[:alnum:]{4}[:digit:]{4}")) %>% 
  unnest(parents)

matches_df <- Sahul_with_extra_parents_unnested %>% 
  inner_join(wish_list_nodes, by = "parents") %>% 
  distinct(Language_ID, parents)


matches_df_summed <- matches_df  %>% 
  group_by(parents) %>% 
  summarise(n = n())
  
matches_df  %>% 
  arrange(desc(n)) %>% 
  write_tsv("output/matches.tsv", na= "")