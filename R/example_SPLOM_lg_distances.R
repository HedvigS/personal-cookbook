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

#remotes::install_github("annagraff/densify")
library(densify)

#install.packages("sf")       # for reading spatial data
library(sf)
library(fields)
library(ape)
library(cluster)
library(GGally)
library(randomcoloR)
library(missForest)
#install.packages("Amelia")
library(Amelia)
#install.packages("nFactors")
library(nFactors)


# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

lgs <- GB_rcldf_obj$tables$LanguageTable %>% 
  distinct(Language_level_ID) %>% 
  pull()

##############################################
# A S H E R & M O S E L E Y   P O LY G O N S #
##############################################

if(!file.exists("output/asher2007/cldf/traditional/languages.geojson")){
SH.misc::get_zip_from_url("https://zenodo.org/records/15287258/files/Glottography/asher2007world-v1.0.0.zip", exdir = "output/asher2007")
}


polygons <- sf::st_read("output/asher2007/cldf/traditional/languages.geojson") %>% 
  dplyr::filter(cldf.languageReference %in% lgs)

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

d_matrix[upper.tri(d_matrix, diag = F)] <- NA

asher2007_polygons_dists_long <- d_matrix %>% 
  reshape2::melt() %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::rename(asher2007_polygons_dists = value) %>% 
  filter(Var1 != Var2)

#polygons
#if(!file.exists("output/asher2007/cldf/traditional/languages.geojson")){
#  options(timeout = 600)
#  SH.misc::get_zip_from_url("https://zenodo.org/records/7973820/files/all-data.zip", exdir = "output/naranjo_jaeger_Euclide")
#}


############################################
### G L O T T O L O G   P O I N T S ########
############################################
glottolog_ValueTable_long <- readr::read_csv("https://github.com/glottolog/glottolog-cldf/raw/refs/tags/v5.0/cldf/values.csv", 
                  show_col_types = F)  
  
  glottolog_ValueTable <- glottolog_ValueTable_long %>% 
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

glottolog_dists_haversine_wide[upper.tri(glottolog_dists_haversine_wide, diag = F)] <- NA

glottolog_dists_haversine_long <- glottolog_dists_haversine_wide %>% 
  reshape2::melt() %>% 
  dplyr::filter(!is.na(value)) %>% 
  rename(glottolog_points_dist_haversine = value) %>% 
  filter(Var1 != Var2)


glottolog_dists_euclide <- dist(x = glottolog_LanguageValueTable) %>% as.matrix()

rownames(glottolog_dists_euclide) <- rownames(glottolog_LanguageValueTable)
colnames(glottolog_dists_euclide) <- rownames(glottolog_LanguageValueTable)

glottolog_dists_euclide[upper.tri(glottolog_dists_euclide, diag = F)] <- NA

glottolog_dists_euclide_long <- glottolog_dists_euclide %>% 
  reshape2::melt() %>% 
  dplyr::filter(!is.na(value)) %>% 
  rename(glottolog_points_dist_euclide = value) %>% 
  filter(Var1 != Var2)

############################################
####### G L O B A L   T R E E ##############
############################################

tree <- ape::read.nexus(file = "example_data/global-language-tree-MCC-labelled.tree")

tree$tip.label <-  tree$tip.label %>% substr(1, 8) 

dist_matrix <- ape::cophenetic.phylo(tree)

dist_matrix[upper.tri(dist_matrix, diag = F)] <- NA

tree_dists <- dist_matrix %>% 
  reshape2::melt() %>% 
  dplyr::filter(!is.na(value)) %>% 
  rename(global_tree_dist = value) %>% 
  filter(Var1 != Var2)

############################################
########## G R A M B A N K #################
############################################


#grambank distances
GB_all_long <- GB_rcldf_obj$tables$ValueTable %>% 
  dplyr::mutate(Value = ifelse(is.na(Value), "?", Value)) 

GB_all_long_reduced <- GB_all_long %>% 
  rgrambank::make_binary_ValueTable() %>% 
  rgrambank::reduce_ValueTable_to_unique_glottocodes(LanguageTable = GB_rcldf_obj$tables$LanguageTable, merge_dialects = T, method = "singular_least_missing_data", replace_missing_language_level_ID = T, treat_question_mark_as_missing = T)

GB_wide_all <- GB_all_long_reduced %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix() 


# GBI
GBI <- rgrambank::make_GBI(ValueTable = GB_all_long, 
                    recode_patterns_full = read.delim("example_data/feature-recode-patterns.csv", sep = ","),
                    all_decisions = read.delim( "example_data/decisions-log.csv", sep = ","))

#setting up GBI logical
GBI_log <- GBI$logicalGBI 

GBI_log_reduced_wide <- GBI_log %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::rename(Parameter_ID = variable, Value = value) %>%
  dplyr::filter(!is.na(Value)) %>% 
  dplyr::filter(Value != "?") %>% 
  dplyr::filter(Value != "NA") %>%
  rgrambank::reduce_ValueTable_to_unique_glottocodes(LanguageTable = GB_rcldf_obj$tables$LanguageTable,
    GlottologLanguageTable = glottolog_LanguageTable,
                                                     merge_dialects = T, 
                                                     method = "singular_least_missing_data", 
                                                     replace_missing_language_level_ID = T, 
                                                     treat_question_mark_as_missing = T) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix() 

#setting up GBI stat
GBI_stat <- GBI$statisticalGBI     

GBI_stat_reduced_wide <- GBI_stat %>% 
reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::rename(Parameter_ID = variable, Value = value) %>%
  dplyr::filter(!(Parameter_ID %in% c("GB995F", "GB332EON", "GB900EO"))) %>% #some variables generated by GBI stat are not easily mapped to a numeric vector, we are excluding them here
  dplyr::filter(!is.na(Value)) %>% 
  dplyr::filter(Value != "?") %>% 
  dplyr::filter(Value != "NA") %>%
  mutate(Value = ifelse(Parameter_ID == "GB800EO" & Value == "bound", "1", Value)) %>% 
  mutate(Value = ifelse(Parameter_ID == "GB800EO" & Value == "non-bound", "0", Value)) %>% 
  rgrambank::reduce_ValueTable_to_unique_glottocodes(LanguageTable = GB_rcldf_obj$tables$LanguageTable,
                                                     GlottologLanguageTable = glottolog_LanguageTable,
                                                     merge_dialects = T, 
                                                     method = "singular_least_missing_data", 
                                                     replace_missing_language_level_ID = T, 
                                                     treat_question_mark_as_missing = T) %>% 
  dplyr::mutate(Value = as.numeric(Value)) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix() 


#functions


missing_prop <- function(df){
  prop <- sum(is.na(df)) / (sum(!is.na(df)) + sum(is.na(df))) 
  paste0(round(x = prop, digits = 2) * 100, "%")
  
}

fun_cropping <- function(df_wide){
  #  df_wide <- GB_wide_all
  df_long <- df_wide %>% 
    reshape2::melt() %>% 
    dplyr::filter(!is.na(value)) %>% 
    dplyr::filter(value != "?") %>% 
    dplyr::filter(value != "NA") %>%
    dplyr::select(Language_ID = Var1, Parameter_ID = Var2, Value = value) 
  
  df_widecropped_75 <- df_long %>% 
    rgrambank::crop_missing_data(verbose = F) %>% 
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>% 
    column_to_rownames("Language_ID") %>% 
    as.matrix() 
  
  wide_densified <- rgrambank::densify_GB(Grambank_ValueTable = df_long,
                                          Glottolog_ValueTable = glottolog_ValueTable_long, verbose = F) 
  
  wide_densified <- wide_densified$Grambank_ValueTable_densified %>% 
    tibble::column_to_rownames("Language_ID")  
  
  output <- list(df_cropped_75 =  df_widecropped_75, df_wide_densified = wide_densified)
}

#imputation function
fun_imputation <- function(df_wide){
  
  df_imputed <-   df_wide %>% 
    as.data.frame() %>%
    mutate_all(as.factor) %>% 
    missForest::missForest() 
  
  df_imputed <-   df_imputed$ximp 
  
  df_imputed
}


## dists
fun_gower_dists <- function(df_wide, value_name){
  
#  df_wide <- GB_cropped_densified 
#  value_name <- "GBI_stat_reduced_value"
  
if(any(class(df_wide) == "data.frame")){
  df_wide <- df_wide %>% 
    dplyr::mutate(across(everything(), ~ as.numeric(.))) %>% 
    as.matrix()
}
  
  # Convert to numeric matrix
  df_wide_num <- matrix(as.numeric(df_wide), nrow = nrow(df_wide), ncol = ncol(df_wide))
  
  dists <- df_wide_num %>% 
    cluster::daisy(metric = "gower", warnBin = F)  %>% 
    as.matrix()
  
  rownames(dists) <- rownames(df_wide)
  colnames(dists) <- rownames(df_wide)
  
  #the distances are symmetrical, so we don't need the full distance matrix of upper and lower triangles, but just one of them. setting the upper one to NA, which we will later filter out.
  dists[upper.tri(dists, diag = F)] <- NA
  
  dists_long <- dists %>% 
    reshape2::melt() %>% 
    filter(Var1 != Var2) %>% 
    dplyr::filter(!is.na(value))
  
  colnames(dists_long) <- c("Var1", "Var2", value_name)
  dists_long
}

GB_wide_all <- GB_wide_all
GB_cropped <- GB_wide_all %>%
  fun_cropping()

GB_cropped_75 <- GB_cropped$df_cropped_75
GB_cropped_densified <- GB_cropped$df_wide_densified

GBI_log_reduced_wide <- GBI_log_reduced_wide
GBI_log_reduced_wide_cropped <-   fun_cropping(GBI_log_reduced_wide)
GBI_log_wide_cropped_75 <- GBI_log_reduced_wide_cropped$df_cropped_75
GBI_log_wide_cropped_densified <- GBI_log_reduced_wide_cropped$df_wide_densified

GBI_stat_reduced_wide <- GBI_stat_reduced_wide
GBI_stat_reduced_wide_cropped <-   fun_cropping(GBI_stat_reduced_wide)
GBI_stat_wide_cropped_75 <- GBI_stat_reduced_wide_cropped$df_cropped_75
GBI_stat_wide_cropped_densified <- GBI_stat_reduced_wide_cropped$df_wide_densified

  
#imputed
GB_wide_all_imputed <- fun_imputation(GB_wide_all)
GB_cropped_75_imputed <- fun_imputation(GB_cropped_75)
GB_cropped_densified_imputed <- fun_imputation(GB_cropped_densified)

GBI_log_reduced_wide_imputed <- fun_imputation(GBI_log_reduced_wide)
GBI_log_wide_cropped_75_imputed <- fun_imputation(GBI_log_wide_cropped_75)
GBI_log_wide_cropped_densified_imputed <- fun_imputation(GBI_log_wide_cropped_densified)

GBI_stat_reduced_wide_imputed <- fun_imputation(GBI_stat_reduced_wide)
GBI_stat_wide_cropped_75_imputed <- fun_imputation(GBI_stat_wide_cropped_75)
GBI_stat_wide_cropped_densified_imputed <- fun_imputation(GBI_stat_wide_cropped_densified)

#dists
GB_dists_all_gower <- GB_wide_all %>% fun_gower_dists(value_name = "GB_\n_all_gower")
GB_cropped_75_gower <- GB_cropped_75  %>% fun_gower_dists(value_name = "GB_cropped\n_75_gower")
GB_cropped_densified_gower <- GB_cropped_densified  %>%   fun_gower_dists(value_name = "GB_cropped\n_densified\n_gower")
GB_wide_all_imputed_gower <- GB_wide_all_imputed  %>% fun_gower_dists(value_name = "GB_all\n_imputed\n_gower")
GB_cropped_75_imputed_gower <- GB_cropped_75_imputed  %>% fun_gower_dists(value_name = "GB_cropped_75\n_imputed\n_gower")
GB_cropped_densified_imputed_gower <- GB_cropped_densified  %>% fun_gower_dists(value_name = "GB_cropped\n_densified\n_imputed\n_gower")

GBI_log_reduced_wide_gower <- GBI_log_reduced_wide  %>% fun_gower_dists(value_name = "GBI_log_\nall\n_gower")
GBI_log_wide_cropped_75_gower <- GBI_log_wide_cropped_75  %>% fun_gower_dists(value_name = "GBI_log\n_cropped_75\n_gower")
GBI_log_wide_cropped_densified_gower <- GBI_log_wide_cropped_densified  %>% fun_gower_dists(value_name = "GBI_log\n_cropped_densified\n_gower")
GBI_log_reduced_wide_imputed_gower <- GBI_log_reduced_wide_imputed  %>% fun_gower_dists(value_name = "GBI_log\n_imputed\n_gower")
GBI_log_wide_cropped_75_imputed_gower <- GBI_log_wide_cropped_75_imputed  %>% fun_gower_dists(value_name = " GBI_log\n_cropped_75\n_imputed\n_gower")
GBI_log_wide_cropped_densified_imputed_gower <- GBI_log_wide_cropped_densified_imputed  %>% fun_gower_dists(value_name = "GBI_log\n_cropped_densified\n_imputed\n_gower")

GBI_stat_reduced_wide_gower <- GBI_stat_reduced_wide  %>% fun_gower_dists(value_name = "GBI_stat_\nall\n_gower")
GBI_stat_wide_cropped_75_gower <- GBI_stat_wide_cropped_75  %>% fun_gower_dists(value_name = "GBI_stat\n_cropped_75\n_gower")
GBI_stat_wide_cropped_densified_gower <- GBI_stat_wide_cropped_densified  %>% fun_gower_dists(value_name = "GBI_stat\n_cropped_densified\n_gower")
GBI_stat_reduced_wide_imputed_gower <- GBI_stat_reduced_wide_imputed  %>% fun_gower_dists(value_name = "GBI_stat\n_imputed\n_gower")
GBI_stat_wide_cropped_75_imputed_gower <- GBI_stat_wide_cropped_75_imputed  %>% fun_gower_dists(value_name = " GBI_stat\n_cropped_75\n_imputed\n_gower")
GBI_stat_wide_cropped_densified_imputed_gower <- GBI_stat_wide_cropped_densified_imputed  %>% fun_gower_dists(value_name = "GBI_stat\n_cropped_densified\n_imputed\n_gower")



#PCA
fun_PCA_dists <- function(df_wide, value_name){
  
  #df_wide <-  GB_cropped_densified
  
  if(any(class(df_wide) == "data.frame")){
    df_wide <- df_wide %>% 
      dplyr::mutate(across(everything(), ~ as.numeric(.))) %>% 
      as.matrix()
  }
  
  # Convert to numeric matrix
  df_wide_num <- matrix(as.numeric(df_wide), nrow = nrow(df_wide), ncol = ncol(df_wide))
  
  pca_obj <-  prcomp(x = df_wide_num)
  
  pca_obj
  
  
  #testing to evaluate the optimal number of components
  ev <- eigen(cor(df_wide_num)) # get eigenEstimates
  ap <- nFactors::parallel(
    subject=nrow(df_wide_num),
    var=ncol(df_wide_num),
    rep=100,
    cent=0.05)
  nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  
  optimal_components <- nS$Components$nparallel
  
  pca_dist <- pca_obj$x[,1:optimal_components] %>% 
  cluster::daisy(metric = "gower", warnBin = F)  %>% 
    as.matrix()
  
  rownames(pca_dist ) <- rownames(df_wide)
  colnames(pca_dist ) <- rownames(df_wide)
  
  #the distances are symmetrical, so we don't need the full distance matrix of upper and lower triangles, but just one of them. setting the upper one to NA, which we will later filter out.
  pca_dist [upper.tri(pca_dist , diag = F)] <- NA
  
  dists_long <- pca_dist  %>% 
    reshape2::melt() %>% 
    filter(Var1 != Var2) %>% 
    dplyr::filter(!is.na(value))
  
  colnames(dists_long) <- c("Var1", "Var2", value_name)
  dists_long
  
  
}
  

GB_wide_all_imputed_PCA <- GB_wide_all_imputed  %>% fun_PCA_dists(value_name = "GB_all\n_imputed\n_PCA")
GB_cropped_75_imputed_PCA <- GB_cropped_75_imputed  %>% fun_PCA_dists(value_name = "GB_cropped_75\n_imputed\n_PCA")
GB_cropped_densified_imputed_PCA <- GB_cropped_densified_imputed  %>% fun_PCA_dists(value_name = "GB_cropped\n_densified\n_imputed\n_PCA")

GBI_log_reduced_wide_imputed_PCA <- GBI_log_reduced_wide_imputed  %>% fun_PCA_dists(value_name = "GBI_log\n_imputed\n_PCA")
GBI_log_wide_cropped_75_imputed_PCA <- GBI_log_wide_cropped_75_imputed  %>% fun_PCA_dists(value_name = " GBI_log\n_cropped_75\n_imputed\n_PCA")
GBI_log_wide_cropped_densified_imputed_PCA <- GBI_log_wide_cropped_densified_imputed  %>% fun_PCA_dists(value_name = "GBI_log\n_cropped_densified\n_imputed\n_PCA")

GBI_stat_reduced_wide_imputed_PCA <- GBI_stat_reduced_wide_imputed  %>% fun_PCA_dists(value_name = "GBI_stat\n_imputed\n_PCA")
GBI_stat_wide_cropped_75_imputed_PCA <- GBI_stat_wide_cropped_75_imputed  %>% fun_PCA_dists(value_name = " GBI_stat\n_cropped_75\n_imputed\n_PCA")
GBI_stat_wide_cropped_densified_imputed_PCA <- GBI_stat_wide_cropped_densified_imputed  %>% fun_PCA_dists(value_name = "GBI_stat\n_cropped_densified\n_imputed\n_PCA")





dists_joined <- asher2007_polygons_dists_long %>% 
  full_join(glottolog_dists_euclide_long, by = c("Var1", "Var2")) %>% 
  full_join(glottolog_dists_haversine_long, by = c("Var1", "Var2")) %>% 
 full_join(tree_dists, by = c("Var1", "Var2")) %>%
  full_join(GB_dists_all_gower, by = c("Var1", "Var2")) %>% 
  full_join(GB_cropped_75_gower, by = c("Var1", "Var2"))  %>% 
  full_join(GB_cropped_densified_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GB_wide_all_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GB_cropped_75_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GB_cropped_densified_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_reduced_wide_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_wide_cropped_75_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_wide_cropped_densified_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_reduced_wide_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_wide_cropped_75_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_wide_cropped_densified_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_reduced_wide_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_wide_cropped_75_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_wide_cropped_densified_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_reduced_wide_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_wide_cropped_75_imputed_gower , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_wide_cropped_densified_imputed_gower , by = c("Var1", "Var2")) %>% 
  full_join(GB_wide_all_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GB_cropped_75_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GB_cropped_densified_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_reduced_wide_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_wide_cropped_75_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_log_wide_cropped_densified_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_reduced_wide_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_wide_cropped_75_imputed_PCA , by = c("Var1", "Var2"))  %>% 
  full_join(GBI_stat_wide_cropped_densified_imputed_PCA , by = c("Var1", "Var2")) 


p <- dists_joined %>%
  dplyr::filter(!is.na(`GB_\n_all_gower`)) %>% 
  sample_n(6400) %>% 
  dplyr::select(-Var1, -Var2) %>% 
  SH.misc::coloured_SPLOM(herringbone = T)


ggsave(plot = p, filename= "test.png", height = 30, width = 30)
