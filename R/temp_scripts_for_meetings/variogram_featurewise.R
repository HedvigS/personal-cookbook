source('requirements.R')

#### Read Data ####

#reading in GB
GB_imputed_filename <- file.path("output", "GB_wide", "GB_wide_imputed_binarized.tsv")
GB_imputed <- read.delim(GB_imputed_filename, sep = "\t")

languages <- read.delim("output/non_GB_datasets/glottolog-cldf_wide_df.tsv") %>%		
  dplyr::select(Language_ID, Family_ID, Name, Longitude, Latitude, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  inner_join(dplyr::select(GB_imputed, "Language_ID"), by = "Language_ID") %>% 
  mutate(Longitude = round(Longitude, 3)) %>% # let's cut down the precision of the lat/long to make the process go quicker. See stack exchange thread where they say "The third decimal place is worth up to 110 m: it can identify a large agricultural field or institutional campus." https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude
  mutate(Latitude = round(Latitude, 3)) 

data_df <- languages %>% 
  inner_join(GB_imputed, by = "Language_ID")

#features to loop over
features <- GB_imputed %>% 
  dplyr::select(-Language_ID) %>% 
  colnames() 

#spatial variogram
df_vario <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df_vario) <- c("bin", "mean.bin.dist", "vario", "Feature_ID")
df_vario$bin <- as.character(df_vario$bin)
df_vario$mean.bin.dist <- as.numeric(df_vario$mean.bin.dist)
df_vario$vario <- as.numeric(df_vario$vario)
df_vario$Feature_ID <- as.character(df_vario$Feature_ID)

#### Spatial variogram ####

pb = txtProgressBar(min = 0, max = length(seq_along(features)), initial = 0, style = 3, width = 50) 

for (idx in seq_along(features)) {
  feature <- features[idx]
  
cat("\n Running vario on feature", feature, ".\n")
  
vario.spatial = vario(n.bins = 15,
                  data = data_df[,c("Latitude", "Longitude", feature)],
                  type = "moran")

df_feature <- as.data.frame(vario.spatial$mean.bin.dist) %>%
  rownames_to_column("bin") %>% 
  cbind(as.data.frame(vario.spatial$vario)) %>% 
  mutate(Feature_ID = feature)

colnames(df_feature) <- c("bin", "mean.bin.dist", "vario", "Feature_ID")

df_vario <- df_vario %>% 
  full_join(df_feature, by = c("bin", "mean.bin.dist", "vario", "Feature_ID"))
setTxtProgressBar(pb, idx)
}

close(pb)

df_vario %>% 
  write_tsv("output/spatiophylogenetic_modelling/results/df_spatial_vario_featurewise.tsv")

df_vario <-   read.delim("df_vario.tsv", sep ="\t")

parameters_groups <- read.delim("feature_grouping_for_analysis.csv", sep = ",")

p_load("lemon")
df_vario_plot <- df_vario %>% 
  left_join(parameters_groups, by = "Feature_ID") %>%
  ggplot() +
  geom_line(aes(x = mean.bin.dist, y = vario, col = df_vario$Feature_ID)) +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        plot.margin = unit(c(1,3,1,1), "lines"),
        panel.spacing = unit(2, "lines"))+
  facet_rep_wrap(vars(Main_domain), 
             repeat.tick.labels = 'bottom') +
  xlab("Distance") +
  ylab("Moran's I") 
  
png("output/spatiophylogenetic_modelling/figures/variogram_featurewise.png", width = 10, height = 10, units = "in", res = 300)
plot(df_vario_plot)
x <- dev.off()