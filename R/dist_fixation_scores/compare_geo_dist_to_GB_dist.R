source("requirements.R")
pacman::p_load("fields")
p_load("wesanderson")
p_load("adephylo")

#reading in lg meta data
Language_meta_data <-  read_csv(GRAMBANK_LANGUAGES, col_types=LANGUAGES_COLSPEC) %>%		
  dplyr::select(Language_ID = Language_level_ID, Latitude, Longitude, Family_name) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name))

left <- Language_meta_data %>% 
  dplyr::select(Var1 = Language_ID , Family_name_var1 = Family_name) 

right <- Language_meta_data %>% 
  dplyr::select(Var2 = Language_ID , Family_name_var2 = Family_name) 

#reading in GB
GB <- read.delim(file.path("GB_wide", "GB_cropped_for_missing.tsv"), sep ="\t") %>%
  column_to_rownames("Language_ID") %>%
  as.matrix()

#calculating gower distances %>% 
GB_dist <- GB %>% 
  cluster::daisy(metric = "gower", warnBin = F) %>% 
  as.matrix()

rownames(GB_dist) <- rownames(GB)
colnames(GB_dist) <- rownames(GB)

GB_dist[upper.tri(x = GB_dist, diag = T)] <- NA

GB_dist_list <- GB_dist %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  rename(GB_dist = value) 

##geo dist

lat_long_matrix <- Language_meta_data %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

geo_dist <- fields::rdist.earth(lat_long_matrix) 

geo_dist[upper.tri(geo_dist, diag = T)] <- NA

geo_dist_list <- geo_dist %>%   
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  rename(geo_dist = value) %>% 
  mutate(geo_dist = log10(geo_dist))

#tree dist 
if (!dir.exists("spatiophylogenetic_modelling/processed_data")) { 
  dir.create("spatiophylogenetic_modelling/processed_data")
  source("spatiophylogenetic_modelling/processing/pruning_jagertree.R")}		

tree <- read.tree("spatiophylogenetic_modelling/processed_data/jaeger_pruned.tree")


tree_dist <- adephylo::distTips(tree) %>% 
  as.matrix() 

tree_dist[upper.tri(tree_dist, diag = T)] <- NA

tree_dist_list <- tree_dist %>% 
  as.data.frame() %>% 
  rownames_to_column("Language_ID") %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  filter(!is.na(value)) %>% 
  rename(tree_dist = value, Var1 = Language_ID, Var2 = variable) %>% 
  mutate(tree_dist = log10(tree_dist))

#joined 

joined <- full_join(GB_dist_list, geo_dist_list) %>% 
  full_join(tree_dist_list) %>% 
  left_join(left) %>% 
  left_join(right) %>% 
  mutate(same_fam = ifelse(Family_name_var1 == Family_name_var2, "same", "diff"))

png("temp_scripts_for_meetings/geo_GB_dist.png", height = 10.6, width = 11.3)
joined %>% 
  ggplot(aes(x = geo_dist, y = GB_dist)) +
  geom_point(aes(color = same_fam), alpha = 0.6) +
  theme_classic() +
  geom_smooth(aes(group=same_fam), color = "black") +
  scale_color_manual(values= wes_palette("GrandBudapest2", n = 2))

x <- dev.off()


png("temp_scripts_for_meetings/tree_GB_dist.png", height = 10.6, width = 11.3)

joined %>% 
  ggplot(aes(x = tree_dist, y = GB_dist)) +
  geom_point(aes(color = same_fam), alpha = 0.6) +
  theme_classic() +
  geom_smooth(aes(group=same_fam), color = "black") +
  scale_color_manual(values= wes_palette("GrandBudapest2", n = 2))

x <- dev.off()