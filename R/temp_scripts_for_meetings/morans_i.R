source("requirements.R")

#outputdir
OUTPUTDIR <- file.path("dists")
if (!dir.exists(OUTPUTDIR)) { dir.create(OUTPUTDIR) }		

#reading in GB
GB <- read.delim(file.path("PCA", "PCA_language_values.tsv"))

#reading in lg meta data
Language_meta_data <-  read_csv(GRAMBANK_LANGUAGES, col_types=LANGUAGES_COLSPEC) %>%		
  dplyr::select(Language_ID = Language_level_ID, Family_name, Name, Macroarea, Latitude, Longitude) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name)) %>% 
  inner_join(GB)

#geo dists plain
lat_long_matrix <- Language_meta_data %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

geo_dist <- fields::rdist.earth(lat_long_matrix) 

#invert
geo_dist.inv <- 1/geo_dist
diag(geo_dist.inv) <- 0

moran_pc1 <- ape::Moran.I(Language_meta_data$PC1, geo_dist.inv)
moran_pc2 <- ape::Moran.I(Language_meta_data$PC2, geo_dist.inv)

