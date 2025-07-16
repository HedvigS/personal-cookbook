library(devtools)

#devtools::install_github("HedvigS/SH.misc")
library(SH.misc)

#devtools::install_github("HedvigS/rgrambank")
library(rgrambank)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE)   
library(rcldf)

#install.packages("cluster")
library(cluster)

#install.packages("sf")       # for reading spatial data
library(sf)
library(fields)
library(ape)
library(cluster)

library(randomcoloR)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

#polygons
if(!file.exists("output/asher2007/cldf/traditional/languages.geojson")){
SH.misc::get_zip_from_url("https://zenodo.org/records/15287258/files/Glottography/asher2007world-v1.0.0.zip", exdir = "output/asher2007")
}


polygons <- sf::st_read("output/asher2007/cldf/traditional/languages.geojson")

pal <- c(randomcoloR::distinctColorPalette(1000),
         randomcoloR::distinctColorPalette(1000),
         randomcoloR::distinctColorPalette(1000), 
         randomcoloR::distinctColorPalette(1000),
         randomcoloR::distinctColorPalette(382))

p_world_asher2007 <- polygons %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = cldf.languageReference), alpha = 0.5, color = NA) +
  scale_fill_manual(values = pal) +
  theme_light() +
  theme(legend.position = "None")

ggsave(filename = "output/asher2007_worldmap.png", plot = p_world_asher2007,  width = 30, height = 20, units = "cm", dpi = 300)

polygons_proj <- st_transform(polygons, crs = "+proj=robin") 

dist_matrix <- st_distance(polygons_proj)

d_numeric <- units::drop_units(dist_matrix)
d_matrix <- as.matrix(d_numeric)

rownames(d_matrix) <- polygons_proj$cldf.languageReference
colnames(d_matrix) <- polygons_proj$cldf.languageReference

asher2007_polygons_dists_long <- d_matrix %>% 
  reshape2::melt() %>% 
  dplyr::rename(asher2007_polygons_dists = value) %>% 
  filter(Var1 != Var2)

#polygons
#if(!file.exists("output/asher2007/cldf/traditional/languages.geojson")){
#  options(timeout = 600)
#  SH.misc::get_zip_from_url("https://zenodo.org/records/7973820/files/all-data.zip", exdir = "output/naranjo_jaeger_Euclide")
#}


glottolog_ValueTable <- readr::read_csv("https://github.com/glottolog/glottolog-cldf/raw/refs/tags/v5.0/cldf/values.csv", 
                  show_col_types = F) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value")

glottolog_LanguageTable <-  readr::read_csv("https://github.com/glottolog/glottolog-cldf/raw/refs/tags/v5.0/cldf/languages.csv",
                  show_col_types = F) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::rename(Language_ID = ID)

glottolog_LanguageValueTable <- glottolog_ValueTable %>% 
  dplyr::full_join(glottolog_LanguageTable, by = "Language_ID") %>% 
  dplyr::filter(!is.na(Longitude)) %>%
  dplyr::filter(level == "language") %>% 
  dplyr::filter(category == "Spoken_L1_Language") %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude)

glottolog_dists_haversine_wide <- fields::rdist.earth(x1 = glottolog_LanguageValueTable, x2 = glottolog_LanguageValueTable, miles = F)

rownames(glottolog_dists_haversine_wide) <- rownames(glottolog_LanguageValueTable)
colnames(glottolog_dists_haversine_wide) <- rownames(glottolog_LanguageValueTable)

glottolog_dists_haversine_long <- glottolog_dists_haversine_wide %>% 
  reshape2::melt() %>% 
  rename(glottolog_points_dist_haversine = value) %>% 
  filter(Var1 != Var2)


glottolog_dists_euclide <- dist(x = glottolog_LanguageValueTable) %>% as.matrix()

rownames(glottolog_dists_euclide) <- rownames(glottolog_LanguageValueTable)
colnames(glottolog_dists_euclide) <- rownames(glottolog_LanguageValueTable)

glottolog_dists_euclide_long <- glottolog_dists_euclide %>% 
  reshape2::melt() %>% 
  rename(glottolog_points_dist_euclide = value) %>% 
  filter(Var1 != Var2)

#Trees
tree <- ape::read.nexus(file = "example_data/global-language-tree-MCC-labelled.tree")

tree$tip.label <-  tree$tip.label %>% substr(1, 8) 

dist_matrix <- ape::cophenetic.phylo(tree)

tree_dists <- dist_matrix %>% 
  reshape2::melt() %>% 
  rename(global_tree_dist = value) %>% 
  filter(Var1 != Var2)



#grambank distances

GB <- GB_rcldf_obj$tables$ValueTable %>% 
  filter(Value != "?") %>% 
  mutate(Value = as.numeric(Value)) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix() 

GB_dists <- GB %>% 
      cluster::daisy(metric = "gower", warnBin = F)  %>% 
  as.matrix()
  
rownames(GB_dists) <- rownames(GB)
colnames(GB_dists) <- rownames(GB)
  
GB_dists_long <- GB_dists %>% 
  reshape2::melt() %>% 
  rename(GB_dist = value) %>% 
  filter(Var1 != Var2)


#PCA
#MDS
#Gower full
#Gower release paper prine
#Gower post densify

dists_joined <- asher2007_polygons_dists_long %>% 
  full_join(glottolog_dists_euclide_long, by = c("Var1", "Var2")) %>% 
  full_join(glottolog_dists_haversine_long, by = c("Var1", "Var2")) %>% 
  full_join(tree_dists, by = c("Var1", "Var2")) %>% 
  full_join(GB_dists_long, by = c("Var1", "Var2")) 

dists_joined %>% 
  dplyr::filter(!is.na(GB_dist)) %>% 
  sample_n(3400) %>% 
  dplyr::select(-Var1, -Var2) %>% 
  SH.misc::coloured_SPLOM(herringbone = T)

