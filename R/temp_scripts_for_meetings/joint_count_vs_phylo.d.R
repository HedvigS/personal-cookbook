source("requirements.R")
p_load("spdep")

#get the phylo.d table   
if (!file.exists("output/phylosig/pylosig_table.tsv")) { source("phylo_signal_features_d_stat.R") }		
phylo_d_table <- read.delim("output/phylosig/pylosig_table.tsv", sep = "\t") %>% 
  dplyr::rename(Feature_ID = Feature)

GB <- read.delim("output/GB_wide/GB_wide_imputed_binarized.tsv", sep = "\t")

parameters <- read.delim("feature_grouping_for_analysis.csv", sep = ",")

Language_meta_data <-  read.delim(GRAMBANK_LANGUAGES, sep = ",") %>%		
  dplyr::select(Language_ID = Language_level_ID, Latitude, Longitude, Family_name) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name))

GB_locations <- GB %>% 
  left_join(Language_meta_data, by = "Language_ID") %>% 
  dplyr::select(Language_ID,Longitude, Latitude) %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix()

k_to_test <- c(5, 10, 20, 100)

#empty df to bind to
df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) <- c("Feature_ID","jtot_z_value", "k") 
df$Feature_ID <- as.character(df$Feature_ID)
df$jtot_z_value <- as.numeric(df$jtot_z_value)
df$k <- as.numeric(df$k)

for(k in k_to_test){
  
  GB_knn <- knearneigh(GB_locations, k = k, longlat = T)
  
  GB_nb <- knn2nb(GB_knn)
  
  features <- GB %>% 
    dplyr::select(-Language_ID) %>% 
    colnames()
  
  
  index <- 0
  for(feature in features){
    
    #feature <- features[1]
    
    cat(paste0("# Caclualting the join count statistic for ", feature, " with k set to ", k, " . That means I'm ", round(index/length(features) * 100, 2), "% done, for this k.\n"))
    index <- index + 1 
    
    gb_spec <- GB[[feature]] %>% as.factor()
    names(gb_spec) <- GB$Language_ID
    join_count_object <- spdep::joincount.multi(fx = gb_spec, listw = nb2listw(GB_nb, style = "B"))
    
    jtot_z_value<- join_count_object["Jtot", "z-value"] 
    
    df_spec_feature <-  data.frame(
      Feature_ID = feature,
      jtot_z_value = jtot_z_value, 
      k = k) 
    
    df <- df %>% 
      full_join(df_spec_feature, by = c("Feature_ID", "jtot_z_value", "k")) 
  }
  
  cat("I'm a 100% done for this k! Go me!\n")
}

cat("I'm a 100% done for all k! Go me!\n")

joined_df <- df %>% 
  full_join(phylo_d_table, by = "Feature_ID") %>% 
  group_by(k) %>% 
  mutate(jtot_z_value = scale(jtot_z_value))

joined_df$k <- fct_reorder(as.character(joined_df$k), joined_df$k) 

png("output/phylosig/phylo_d_vs_join_count.png")
joined_df %>% 
  left_join(parameters, by = "Feature_ID") %>% 
  ggplot(aes(x = jtot_z_value, y = `D-estimate`, col = k)) +
  geom_point() +
  theme_classic() +
  geom_smooth() +
  scale_y_reverse() +
  annotate("segment",col="black", alpha = 0.6, x = 0, xend = 0, y = 1, yend = -1, size = 0.5, linetype = "dashed")  +
  annotate("segment",col="black", alpha = 0.6, y = 0, yend = 0, x = -3, xend = 2, size = 0.5, linetype = "dashed") 

x <- dev.off()