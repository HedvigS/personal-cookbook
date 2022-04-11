#This is a script showcasing computing gower/relative hamming-distances with linguistic data and visualising as ridgeplots. Thanks to Simon Greenhill for the idea.

#written by Hedvig Skirgård

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  tidyverse,#for data wrangling
  ggridges
)

options(tidyverse.quiet = TRUE)

#reading in data
Sahul_df <- read_tsv("example_data/Sahul_structure_wide.tsv")

data <- Sahul_df

GB_matrix <- GB %>%
  column_to_rownames("Language_ID") %>%
  as.matrix()

#reading in lg meta data
#areas
autotyp_area_fn <- "output_tables/glottolog_AUTOTYP_areas.tsv"
if (!file.exists(autotyp_area_fn)) { 
  source("assigning_AUTOTYP_areas.R") }		
autotyp_area <- read_tsv(autotyp_area_fn, col_types = cols()) %>%
  dplyr::select(Language_ID, AUTOTYP_area)

#meta data
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

rownames(GB_dist) <- rownames(GB_matrix)
colnames(GB_dist) <- rownames(GB_matrix)

GB_dist[upper.tri(GB_dist, diag = T)] <- NA

GB_dist_list <- GB_dist %>% 
  reshape2::melt()  %>% 
  filter(!is.na(value)) 

#make dataframe for the globa distribution
GB_dist_gobal <- GB_dist_list %>% 
  dplyr::select(value) %>% 
  mutate(group_var1 = "global", 
         group_var2 = "global")

#grouped by autotyp area
left <- Language_meta_data %>% 
  dplyr::select(Var1 = Language_ID, group_var1 = AUTOTYP_area)

right <- Language_meta_data %>% 
  dplyr::select(Var2 = Language_ID, group_var2 = AUTOTYP_area)

GB_dist_list_sided <- GB_dist_list %>% 
  left_join(left, by = "Var1") %>% 
  left_join(right, by = "Var2") %>% 
  mutate(same = ifelse(group_var1 == group_var2,"same", "diff")) %>% 
  filter(same == "same") %>% 
  group_by(group_var1) %>% 
  mutate(mean_value = mean(value)) %>% 
  full_join(GB_dist_gobal, by = c("value", "group_var1", "group_var2")) 

GB_dist_list_sided$group_var1 <- fct_reorder(GB_dist_list_sided$group_var1, GB_dist_list_sided$mean_value)

mean_labels <- GB_dist_list_sided %>% 
  distinct(group_var1, mean_value)

GB_dist_list_sided %>% 
  ggplot() +
  geom_density_ridges(aes(x = value, y =group_var1, fill = group_var1), quantile_lines = T, quantile_fun = mean, jittered_points = TRUE, point_size = 2, point_shape = 21  ,  position = position_points_jitter(height = 0))  +
  geom_label(data = mean_labels, aes(x = mean_value, y = group_var1,
                                     label = round(mean_value, 2)), size = 2, nudge_x = 0.01, nudge_y = 0.2, alpha = 0.7, label.padding = unit(0.1, "lines")) +
  theme_classic() +
  theme(axis.title = element_blank(), 
        legend.position = "None") 

ggsave("output/ridgeplot_AUTOTYP_area.png")