#This is a script showcasing imputing data with random forests, doing a PCA and visualising the results by mapping the first three components to RGB values and display them in a 3D plot.

#written by Hedvig Skirgård

#installing and loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  tidyverse,#for data wrangling
  missForest, #for imputation 
  Amelia, #for visualising missingness
  readr #for reading in data files
)

p_load_gh("AckerDWM/gg3D") #for 3d plotting

options(tidyverse.quiet = TRUE)

#reading in data
Sahul_df <- read_tsv("example_data/Sahul_structure_wide.tsv")

data <- Sahul_df %>% 
  rename(ID = glottocode)

missing_prop <- data %>% 
  dplyr::select(-ID) %>%
  is.na() %>%
  mean()

cat(sprintf("%0.2f%% of the data read was missing before imputation.\n",
            missing_prop * 100
))

Amelia::missmap(obj = data)

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
  column_to_rownames("ID") %>% 
  apply(1, function(x) mean(is.na(x)))

#df with the features and observations with the fewest missing as per cut offs.
data_few_missing_values <- data %>% 
  filter(na_prop < LG_NA_PROP_CUTOFF ) %>% 
  dplyr::select(-na_prop) %>% 
  dplyr::select(ID, all_of(cols_to_keep))


#In order to make sure the imputation is doing categorically and not for continuous integers the values are replaces by strings. yes, I know this is silly but I've found it simplfies things, unfortunately.
data_prepped_for_imputation <- data_few_missing_values %>% 
  reshape2::melt(id.vars = "ID") %>% 
  mutate(value = str_replace_all(value, "0", "0 - absent")) %>%
  mutate(value = str_replace_all(value, "1", "1 - present")) %>%
  mutate(value = str_replace_all(value, "2", "2 - multistate")) %>%
  mutate(value = str_replace_all(value, "3", "3 - multistate")) %>%
  mutate(value = str_replace_all(value, "4", "4 - multistate")) %>%
  mutate(value = as.factor(value)) %>%
  dplyr::select(ID, variable, value) %>%
  spread(key = variable, value, drop = FALSE) 

missing_value_percentage_after_cropping <- mean(is.na(data_prepped_for_imputation))
cat(sprintf(
  "The data has now been cropped, and there remains %0.2f%% missing values.\n",
  missing_value_percentage_after_cropping * 100
))

Amelia::missmap(obj = data_few_missing_values)

cat("These are the values that will be imputed by random forests.\n")

#Saving the language IDs separately, it's better to leave them out of the imputation workflow
IDs <- data_prepped_for_imputation$ID

#actual imputation
imputed_data <- data_prepped_for_imputation %>%
  dplyr::select(-ID) %>%
  as.matrix() %>%
  data.frame() %>%
  mutate_all(as.factor) %>% 
  missForest() 

imputed_data$OOBerror

df_imputed <- imputed_data$ximp
df_imputed$ID <- IDs

df_imputed_num <- df_imputed %>%
  mutate_all(as.character) %>% 
  reshape2::melt(id = "ID")  %>%
  mutate(value =  str_replace_all(value, "0 - absent", "0")) %>%
  mutate(value =  str_replace_all(value,  "1 - present", "1")) %>%
  mutate(value =  str_replace_all(value,  "2 - multistate", "2")) %>%
  mutate(value =  str_replace_all(value,  "3 - multistate", "3")) %>%
  mutate(value =  str_replace_all(value,  "4 - multistate", "4")) %>%
  mutate(value = as.numeric(value)) %>% 
    dplyr::select(ID, Parameter_ID = variable, value) %>%
  spread(key = Parameter_ID, value = value, drop = FALSE) 

cat("The imputation of missing values is done. ", round(mean(is.na(data_prepped_for_imputation))*100,2), "% of the data was imputed. 
    Note: we did not impute all missing values in the entire set. Before imputation the dataset was cropped to a subset where languages with more than ",   LG_NA_PROP_CUTOFF *100 ,"% missing values were removed and features with more than",  FEAT_NA_PROP_CUTOFF*100,"% missing languages were removed. This left us with", df_imputed_num %>% nrow(), "languages and", df_imputed_num[,-1] %>% ncol(),"features

In the full dataset, there was",round(missing_prop, 2)*100, "% of the data missing.

After this cropping the dataset contained ", round(mean(is.na(data_prepped_for_imputation))*100,2), "% missing data, this is what was imputed. The Out of Bag error or for the imputation is", round(imputed_data$OOBerror, 2), "

After imputation, the dataset contains",round(mean(is.na(df_imputed_num)*100,2)), "% missing values and is ready for analysis that requires a complete dataset (PCA or otherwise).")

PCA <- df_imputed_num %>% 
  column_to_rownames("ID") %>% 
  prcomp(scale. = TRUE) 

PCA_df <- PCA$x %>% as.data.frame() %>% 
  rownames_to_column("ID") %>% 
  dplyr::select(ID, PC1, PC2, PC3)

###Map first 3 PCA components to RGB
PCA_df$RGB <- PCA_df %>%
  dplyr::select(PC1, PC2, PC3) %>%
  sweep(2,apply(.,2,function(x){2*max(abs(x))}),"/") %>%
  sweep(2,0.5,"+") %>%
  rgb(alpha = 1)

##3D plots for the first 3 components, color coded

png("output/3D_scatterplot_RGB.png", height = 700, width = 700, units = "px")

#rotation variables for the 3d plot
theta = 20
phi = 0

PCA_df %>% 
ggplot( aes(x = PC1, y = PC2, z =PC3), color = PCA_df$RGB) +
  theme_void() +
  gg3D::axes_3D(theta=theta, phi=phi) +
  gg3D::stat_3D(theta=theta, phi=phi, color = PCA_df$RGB) +
  gg3D::labs_3D(theta=theta, phi=phi,
    labs=c("PC1", "PC2", "PC3"), 
    hjust=c(1,0,0), vjust=c(1.5,1,-.2), color = c("darkred", "darkblue", "darkgreen")) 

x <- dev.off()