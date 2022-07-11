#correlation matrix

source("requirements.R")

source("make_wide.R")

wide <- read_tsv("GB_wide/GB_wide_strict.tsv") %>% 
  dplyr::select(-na_prop) %>% 
  column_to_rownames("Language_ID")

cor_matrix <- cor(wide, method = "pearson", use = "complete.obs" )

diag(cor_matrix) <- NA
cor_matrix[upper.tri(cor_matrix)] <- NA

melted <- cor_matrix %>% 
  reshape2::melt() %>% 
  filter(Var1 != Var2) %>% 
  filter(!is.na(value)) 

melted_nrow <- melted %>% nrow()

more_than_point_5 <-  melted %>% 
  mutate(Value = abs(value)) %>% 
  filter(Value > 0.5)
  
more_than_point_5_nrow <- more_than_point_5 %>% nrow()
  
percent_of_concern <- (more_than_point_5_nrow / melted_nrow *100) %>% round(digits = 2) 

cat(percent_of_concern, "% of pairwise combinations of features have a correlation (Pearson) above 0.5.")