#This is a script showcasing computing gower/relative hamming-distances with linguistic data and visualising as ridgeplots. Thanks to Simon Greenhill for the idea.

#written by Hedvig Skirgård

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  tidyverse,#for data wrangling
  ggridges, 
  gplots, 
  viridis
)

options(tidyverse.quiet = TRUE)

#reading in data
Sahul_df <- read_tsv("example_data/Sahul_structure_wide.tsv")

data <- Sahul_df

#reading in lg meta data
#areas
autotyp_area_fn <- "output_tables/glottolog_AUTOTYP_areas.tsv"
if (!file.exists(autotyp_area_fn)) { 
  source("assigning_AUTOTYP_areas.R") }		
autotyp_area <- read_tsv(autotyp_area_fn, col_types = cols()) %>%
  dplyr::select(Language_ID, AUTOTYP_area)

#meta data
Language_meta_data <-  read_tsv("output_tables/cldf_wide_df.tsv", col_types = cols()) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID)) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::select(Language_ID  = Language_level_ID, Family_ID, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_ID = ifelse(is.na(Family_ID), "Isolate", Family_ID)) %>% 
  full_join(autotyp_area, by = "Language_ID") %>% 
  rename(glottocode = Language_ID)

#cropping data for missing variables
missing_prop <- data %>% 
  .[,-1] %>% 
    is.na() %>%
  mean()

cat(sprintf("%0.2f%% of the data read was missing before cropping.\n",
            missing_prop * 100
))

# remove languages with less than LG_NA_PROP_CUTOFF data
# 0 = good (no missing data), 1 = bad (entirely missing)
LG_NA_PROP_CUTOFF = 0.5

# remove features present in less than FEAT_NA_PROP_CUTOFF languages
FEAT_NA_PROP_CUTOFF = 0.5

#counting the missing data per col, i.e. per feature
col_na_means <- data %>% 
  as.matrix() %>% 
  is.na() %>% 
  colMeans() 

cols_to_keep <- tibble(Feature_ID = colnames(data),
                       feature_na_prop = col_na_means) %>% 
  filter(Feature_ID != "ID") %>% 
  arrange(-feature_na_prop) %>% 
  distinct(Feature_ID, .keep_all = T) %>% 
  filter(feature_na_prop < FEAT_NA_PROP_CUTOFF) %>% 
  dplyr::select(Feature_ID) %>% 
  as.matrix() %>% 
  as.vector()

#Some languages have a large amount of missing values. Let's exclude those with a large amount of missing data

data$na_prop <- data %>% 
  column_to_rownames("glottocode") %>% 
  apply(1, function(x) mean(is.na(x)))

#df with the features and observations with the fewest missing as per cut offs.
data_few_missing_values <- data %>% 
  filter(na_prop < LG_NA_PROP_CUTOFF ) %>% 
  dplyr::select(-na_prop) %>% 
  dplyr::select(glottocode, all_of(cols_to_keep))


#calculating gower distances %>% 
dist <- data_few_missing_values %>% 
  column_to_rownames("glottocode") %>% 
  as.matrix() %>% 
  cluster::daisy(metric = "gower", warnBin = F) %>% 
  as.matrix()

rownames(dist) <- data_few_missing_values$glottocode
colnames(dist) <- data_few_missing_values$glottocode

dist_sym <- dist 

dist[upper.tri(dist, diag = T)] <- NA #turning the diagonal (i.e. distances to itself) and the upper triangle to NA. This is because the matrix is symmertic, and we only want to know about each pair once.

dist_list <- dist %>% 
  reshape2::melt()  %>% 
  filter(!is.na(value)) 

#make dataframe for the globa distribution
dist_gobal <- dist_list %>% 
  dplyr::select(value) %>% 
  mutate(group_var1 = "global", 
         group_var2 = "global")

#grouped by autotyp area
left <- Language_meta_data %>% 
  dplyr::select(Var1 = glottocode, group_var1 = AUTOTYP_area)

right <- Language_meta_data %>% 
  dplyr::select(Var2 = glottocode, group_var2 = AUTOTYP_area)

dist_list_sided <- dist_list %>% 
  left_join(left, by = "Var1") %>% 
  left_join(right, by = "Var2") %>% 
  mutate(same = ifelse(group_var1 == group_var2,"same", "diff")) %>% 
  filter(same == "same") %>% 
  group_by(group_var1) %>% 
  mutate(mean_value = mean(value)) %>% 
  full_join(dist_gobal, by = c("value", "group_var1", "group_var2")) 

dist_list_sided$group_var1 <- fct_reorder(dist_list_sided$group_var1, dist_list_sided$mean_value)

#making a small df for the mean labels
mean_labels <- dist_list_sided %>% 
  distinct(group_var1, mean_value)

dist_list_sided %>% 
  ggplot() +
  geom_density_ridges(aes(x = value, 
                          y =group_var1, 
                          fill = group_var1), 
                      alpha = 0.8,
                      quantile_lines = T, 
                      quantile_fun = mean, 
                      jittered_points = TRUE, 
                      point_size = 2, 
                      point_shape = 21  ,  
                      position = position_points_jitter(height = 0))  +
  geom_label(data = mean_labels, aes(x = mean_value, y = group_var1,
                                     label = round(mean_value, 2)), size = 6, nudge_x = 0.01, nudge_y = 0.2, alpha = 0.7, label.padding = unit(0.1, "lines")) +
  theme_classic() +
  theme(axis.title = element_blank(), 
        legend.position = "None", 
        text = element_text(size = 20)) 

ggsave("output/ridgeplot_AUTOTYP_area.png", width = 9, height = 11, units = "in")

#heatmap

dist_sym_grouped <- dist_sym %>% 
  reshape2::melt()  %>% 
  left_join(left, by = "Var1") %>% 
  left_join(right, by = "Var2")  %>%
  group_by(group_var1, group_var2) %>% 
  summarise(mean = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  reshape2::dcast(group_var1 ~ group_var2, value.var = "mean") %>% 
  column_to_rownames("group_var1") %>% 
  as.matrix() 
  
png("output/heatmap_autotyp_area_group.png", width = 2000, height = 2000, units = "px", pointsize = 34)

dist_sym_grouped %>%
  gplots::heatmap.2(  key = F, symm = T,
                          dendrogram = "none",
                          revC = T,
                          trace = "none", 
                      margin=c(15,15),
                      cellnote = round(dist_sym_grouped, 2),
                      col=viridis(15, direction = -1))

x <- dev.off()

png("output/ridgeplot_matrix", width = 10, height = 10, units = "in",res = 200)

dist_sym %>% 
  reshape2::melt()  %>% 
  left_join(left, by = "Var1") %>% 
  left_join(right, by = "Var2") %>%
  group_by(group_var1, group_var2) %>% 
  mutate(mean = mean(value, na.rm = T), 
         n = n()) %>%
  filter(n > 1) %>% 
  ggplot(aes(x=value, fill=mean, color = mean))+
  geom_density(alpha=0.2, position="identity") +
  facet_grid(group_var1 ~ group_var2) +
  theme_classic() +
  theme(legend.position = "none")

x <- dev.off()
