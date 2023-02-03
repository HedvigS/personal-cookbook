source("fun_def_h_load.R")

h_load("NCmisc")
h_load("tidyverse")

r_fns <- list.files(path = ".", pattern = "*.R$", full.names = T, recursive = T)

df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "scripts" = as.character())


for(fn in r_fns){

#  fn <- r_fns[2]
  
  cat(paste0("I'm on ", fn, ".\n"))
  
x <- NCmisc::list.functions.in.file(filename = fn) %>% 
  as.data.frame() %>% 
  rownames_to_column("packages") %>% 
  rename("functions" = 2) %>% 
  mutate(packages = str_replace_all(packages, "package:", "")) %>% 
  mutate(packages = str_replace_all(packages, "c\\(", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\)", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\\"", "")) %>% 
  mutate(packages = str_replace_all(packages, "character\\(0", "")) %>% 
  mutate(packages = str_split(packages, ",")) %>% 
  unnest(cols = "packages") %>% 
  unnest(cols = "functions") %>% 
  mutate(scripts = fn)

df <- full_join(x, df, by = c("packages", "functions", "scripts"))
}

used_packages <- df %>% 
  mutate(used = "TRUE")

loaded_packages <- data.frame(packages = (.packages())) %>% 
  mutate(loaded = "TRUE")

joined_df <- full_join(used_packages, loaded_packages)

unused_but_loaded <- joined_df %>% 
  filter(is.na(used)) %>% 
  filter(!is.na(loaded)) %>% 
  dplyr::select(packages)

cat("There are ", nrow(unused_but_loaded), "packages that it seems like you're not using, but that are loaded.\n They are:\n" )

unused_but_loaded

warning("Note there may be errors, for example the script doesn't tend to understand tidyverse properly but checks each package inside." )

cat("The top 5 packages from which you use the most different functions are: ")
used_packages %>% 
  distinct(packages, functions) %>% 
  group_by(packages) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  .[1:5,]

cat("Keep in mind, this is not top-5 per times you use the package but the top-5 of pacakges from which you use the most functions.")
