#source("requirements.R")

args <- commandArgs(trailingOnly=TRUE)

if(length(args) != 0){
group = args[[1]]
#group = "Family_ID"
cut_off = args[[2]]
#cut_off = 50
}

source("fun_def_h_load.R")

h_load(pkg = c("tidyverse", "reshape2", "txtplot"))

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

#glottolog-cldf
Language_meta_data <-  read_tsv("output/non_GB_datasets/glottolog-cldf_wide_df.tsv", col_types = cols()) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID)) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::select(Language_ID  = Language_level_ID, Family_ID, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_ID = ifelse(is.na(Family_ID), Language_ID, Family_ID))

#join to ensure exact same order
Language_meta_data <- GB %>% 
  left_join(Language_meta_data, by = "Language_ID") %>% 
  left_join(autotyp_area, by = "Language_ID")

#using Muthukrishna's et al's approach

source("dist_fixation_scores/fun_def_Muthukrishna_2020_CultureFst.r")

fun_cfx <- function(df = Language_meta_data, group, cut_off = 0){

#  group = "Family_ID"
#  df <- Language_meta_data
# cut_off <- 10  
  cat(paste0("Running the cultural fixation scores on groupings by ", group ,".\n"))
  
  group_df <- df %>%
    dplyr::select(Language_ID, group = all_of(group)) %>% 
    group_by(group) %>% 
    mutate(n = n()) %>% 
    filter(n > cut_off) %>% 
    dplyr::select(-n)
  
    GB_cropped <- GB %>% 
    inner_join(group_df, by = "Language_ID") %>% 
    dplyr::select(-Language_ID) %>% 
    dplyr::select(group, everything())

    cat(paste0("Below are the number of languages per ", group, ", smallest 6 groups and then a boxplot of all.\n"))
    
    plot_df <- GB_cropped %>% 
      group_by(group) %>% 
      summarise(n = n())  %>% 
      arrange(n)
    
    print(plot_df[1:6,])
  
    txtplot::txtboxplot(plot_df$n, width = 50)
    
    features <- GB_cropped[,-1] %>% colnames()
    types <- rep(0, length(features))
    names(types) <- features
    
    cfx_object <- CultureFst(d = GB_cropped, loci = features, type = types, bootstrap = T, no.samples = 100, message = paste0(group, ", cut off = ", cut_off, " run.")) 
    cfx_matrix <- cfx_object$mean.fst %>% as.matrix()
    cfx_matrix[upper.tri(x = cfx_matrix, diag = T)] <- NA
    
    cfx_list <-cfx_matrix %>% 
      reshape2::melt() %>% 
      filter(!is.na(value)) %>% 
      unite(Var1, Var2, col = "Vars", sep = " - ") %>% 
      rename(Value_cfx = value)
    
    table_fn <- paste0("cfx_",group, "_cut_off_",cut_off,  "_list.tsv")
    
    cfx_list %>% 
      write_tsv(file = file.path(OUTPUTDIR,  table_fn))
}

fun_cfx(df = Language_meta_data, group = "Family_ID", cut_off = 50)

fun_cfx(df = Language_meta_data, group = "Family_ID", cut_off = 20)

fun_cfx(df = Language_meta_data, group = "Family_ID", cut_off = 10)

fun_cfx(df = Language_meta_data, group = "Family_ID", cut_off = 5)

fun_cfx(df = Language_meta_data, group = "Family_ID", cut_off = 1)




