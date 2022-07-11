source("requirements.R")

OUTPUTDIR <- "output/dist_fixation_scores/"
if (!dir.exists(OUTPUTDIR)) {dir.create(OUTPUTDIR)}

#reading in GB
GB <- read.delim(file.path("output", "GB_wide", "GB_cropped_for_missing.tsv"), sep ="\t") 

GB_matrix <- GB %>%
  column_to_rownames("Language_ID") %>%
  as.matrix()

#reading in lg meta data
#areas
if (!file.exists("output/non_GB_datasets/glottolog_AUTOTYP_areas.tsv")) { source("unusualness/processing/assigning_AUTOTYP_areas.R") }		
autotyp_area <- read_tsv("output/non_GB_datasets/glottolog_AUTOTYP_areas.tsv", col_types = cols()) %>%
  dplyr::select(Language_ID, AUTOTYP_area)

#other meta data
Language_meta_data <-  read_tsv("output/non_GB_datasets/glottolog-cldf_wide_df.tsv", col_types = cols()) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID)) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::select(Language_ID  = Language_level_ID, Family_ID, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_ID = ifelse(is.na(Family_ID), "Isolate", Family_ID)) %>% 
  left_join(autotyp_area, by = "Language_ID")

#calculating gower distances %>% 
GB_dist <- GB_matrix %>% 
  cluster::daisy(metric = "gower", warnBin = F) %>% 
  as.matrix()

#insert back the names
rownames(GB_dist) <- rownames(GB_matrix)
colnames(GB_dist) <- rownames(GB_matrix)

source("dist_fixation_scores/dist_viz_funs.R")

#AUTOTYP-area based plots

dists_ridgeplot(dist_matrix = GB_dist, group_df = Language_meta_data, group = "AUTOTYP_area", title = "AUTOTYP_area", fn = file.path(OUTPUTDIR, "ridgeplot_AUTOTYP_area.png")) 

dists_heatmap(dist_matrix = GB_dist, group_df = Language_meta_data, group = "AUTOTYP_area", title = "AUTOTYP_area", fn = file.path(OUTPUTDIR, "heatmap_AUTOTYP_area.png")) 

dists_ridgeplot(dist_matrix = GB_dist, group_df = Language_meta_data, group = "Macroarea", title = "Macroarea", fn = file.path(OUTPUTDIR, "ridgeplot_Macroarea.png")) 

dists_heatmap(dist_matrix = GB_dist, group_df = Language_meta_data, group = "Macroarea", title = "Macroarea", fn = file.path(OUTPUTDIR, "heatmap_Macroarea.png")) 

dists_ridgeplot(dist_matrix = GB_dist, group_df = Language_meta_data, group = "Macroarea", title = "Macroarea", fn = file.path(OUTPUTDIR, "ridgeplot_Macroarea.png")) 

dists_heatmap(dist_matrix = GB_dist, group_df = Language_meta_data, group = "Macroarea", title = "Macroarea", fn = file.path(OUTPUTDIR, "heatmap_Macroarea.png")) 


