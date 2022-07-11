source("requirements.R")

phist_macroarea_list <- read_tsv("output/dist_fixation_scores/PHiST_macroarea.tsv") %>% 
  rename(PHiST_score = value)

phist_autptyp_area_list <- read_tsv("output/dist_fixation_scores/PHiST_autotyp_area.tsv") %>% 
  rename(PHiST_score = value)

phist_family_list <- read_tsv("output/dist_fixation_scores/PHiST_Family_ID.tsv") %>% 
  rename(PHiST_score = value)

#metadata
if (!file.exists("output/non_GB_datasets/glottolog_AUTOTYP_areas.tsv")) { source("unusualness/processing/assigning_AUTOTYP_areas.R") }		
autotyp_area <- read_tsv("output/non_GB_datasets/glottolog_AUTOTYP_areas.tsv", col_types = cols()) %>%
  dplyr::select(Language_ID, AUTOTYP_area)

#glottolog-cldf
glottolog_df <-  read_tsv("output/non_GB_datasets/glottolog-cldf_wide_df.tsv", col_types = cols()) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID)) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::select(Language_ID  = Language_level_ID, Family_ID, Name, Macroarea, Longitude, Latitude) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_ID = ifelse(is.na(Family_ID), "Isolate", Family_ID))

#reading in GB
Language_meta_data <- read.delim(file.path("output", "GB_wide", "GB_cropped_for_missing.tsv"), sep ="\t")  %>% 
  dplyr::select(Language_ID) %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  left_join(autotyp_area, by = "Language_ID")

#calculate geo_dists
lat_long_matrix <- Language_meta_data %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

geo_dist <- fields::rdist.earth(lat_long_matrix) 

geo_dist_list <- geo_dist %>% 
  reshape2::melt() 

#summarise for autotyp-area
left <- Language_meta_data %>% 
  dplyr::select(Var1 = Language_ID , Var1_AUTOTYP_area = AUTOTYP_area)

right <- Language_meta_data %>% 
  dplyr::select(Var2 = Language_ID , Var2_AUTOTYP_area = AUTOTYP_area)

autotyp_area_comparison_list <- geo_dist_list %>% 
  left_join(left) %>% 
  left_join(right) %>% 
  group_by(Var1_AUTOTYP_area, Var2_AUTOTYP_area) %>% 
  summarise(mean_dist = mean(value, na.rm = T)) %>%
  unite(Var1_AUTOTYP_area, Var2_AUTOTYP_area, col = Vars, sep = " - ") %>% 
  right_join(phist_autptyp_area_list)

autotyp_area_comparison_list %>% 
  ggplot(aes(x = PHiST_score, y = mean_dist)) +
  geom_point(color = "turquoise3") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8, size = 8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme_classic()+
  theme(text = element_text(size = 20))


ggsave("output/dist_fixation_scores/PHiST_vs_geo_autotyp_area.png")

#summarise for macroarea
left <- Language_meta_data %>% 
  dplyr::select(Var1 = Language_ID , Var1_macroarea = Macroarea)

right <- Language_meta_data %>% 
  dplyr::select(Var2 = Language_ID , Var2_macroarea = Macroarea)

Macroarea_comparison_list <- geo_dist_list %>% 
  left_join(left) %>% 
  left_join(right) %>% 
  group_by(Var1_macroarea, Var2_macroarea) %>% 
  summarise(mean_dist = mean(value, na.rm = T)) %>%
  unite(Var1_macroarea, Var2_macroarea, col = Vars, sep = " - ") %>% 
  right_join(phist_macroarea_list)

Macroarea_comparison_list %>% 
  ggplot(aes(x = PHiST_score, y = mean_dist)) +
  geom_point(color = "green") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8, size =8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave("output/dist_fixation_scores/PHiST_vs_geo_Macroarea.png")


#compare to phylo


#summarise for Family_ID
left <- Language_meta_data %>% 
  dplyr::select(Var1 = Language_ID , Var1_Family_ID = Family_ID)

right <- Language_meta_data %>% 
  dplyr::select(Var2 = Language_ID , Var2_Family_ID = Family_ID)


tree <- read.tree("output/spatiophylogenetic_modelling/processed_data/jaeger_pruned.tree")

tree_dist <- adephylo::distTips(tree) %>% 
  as.matrix() 

family_ID_phist_comparison <- tree_dist %>% 
  reshape2::melt() %>% 
  left_join(left) %>% 
  left_join(right) %>% 
  group_by(Var1_Family_ID , Var2_Family_ID ) %>% 
  summarise(mean_tree_dist = mean(value)) %>% 
  unite(Var1_Family_ID , Var2_Family_ID, sep = " - " , col = "Vars") %>% 
  right_join(phist_family_list)

family_ID_phist_comparison %>% 
  ggplot(aes(x = PHiST_score, y = mean_tree_dist)) +
  geom_point(color = "orange") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8, size =8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave("output/dist_fixation_scores/PHiST_vs_tree_dist_family.png")

