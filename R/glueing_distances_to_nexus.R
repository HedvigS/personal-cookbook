#everyone it seems has a semi-hacky script for cobbling together a nexus file. Here's mine in R, specifically for distances matrices but can be modified to feed in data and let SplitsTree or other software calcualte distances themselves.

#This script was written with significant help from Nay San https://www.resunay.com

pacman::p_load(
  dplyr,#for data wrangling
  knitr, 
  glue, 
  tibble,
  readr #for reading in data files
)

dist_matrix <- read_tsv("example_data/example_matrix.tsv") %>% 
  column_to_rownames("Language_level_ID_Var1") 

diag(dist_matrix) <- 0 #double confirm that diagonal is 0

#NN

langs_string <- dist_matrix  %>% 
  colnames() %>% 
  as.data.frame() %>% 
  rename(var = ".") %>% 
  mutate(index = 1:n() - 1) %>% 
  glue_data("[{index}] '{var}'")

nrow(dist_matrix) -> taxa_number

rownames(dist_matrix) <- NULL
colnames(dist_matrix) <- NULL

knitr::kable(dist_matrix) %>% 
  str_remove_all("\\||-|:") %>% 
  .[-c(1,2)] %>% 
  paste0(collapse = "\n") -> matrix_string

glue('
     #nexus
     BEGIN TAXA;
     DIMENSIONS NTAX = {taxa_number};
     TAXLABELS 
     {langs_string %>% paste0(collapse = "\n")};
     END; 
     
     [taxa]
     
     BEGIN DISTANCES;
     dimensions ntax = {taxa_number};
     format labels= no diagonal triangle=both;
     MATRIX 
     {matrix_string};
     END; [distances]
     ') -> dist_nexus

write_lines(dist_nexus, "otuput/ABVD_distances.nex", na = "")