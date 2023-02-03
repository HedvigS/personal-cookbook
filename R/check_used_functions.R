#loading necessary packages
source("fun_def_h_load.R")
h_load("NCmisc")
h_load("tidyverse")

#making a list of all r-scripts in this directory, including down in sub-directories
r_fns <- list.files(path = ".", pattern = "*.R$", full.names = T, recursive = T)

#making an empty df to bind to in the for loop
df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "scripts" = as.character())


#looping over each script
for(fn in r_fns){

#  fn <- r_fns[2]
  
  cat(paste0("I'm on ", fn, ".\n"))
  
x <- NCmisc::list.functions.in.file(filename = fn) %>% #listing the functions
  as.data.frame() %>% #making into df
  rownames_to_column("packages") %>% #set columns
  rename("functions" = 2) %>% 
  mutate(scripts = fn) %>% 
  mutate(packages = str_replace_all(packages, "package:", "")) %>% #clean up the content in the packages col
  mutate(packages = str_replace_all(packages, "c\\(", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\)", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\\"", "")) %>% 
  mutate(packages = str_replace_all(packages, "character\\(0", "")) %>% 
  mutate(packages = str_split(packages, ",")) %>% 
  unnest(cols = "packages") %>% #split into many rows when there are more than one function or package per row
  unnest(cols = "functions")  

#joing to outside for loop df
df <- full_join(x, df, by = c("packages", "functions", "scripts"))
}

#add col for being used
used_packages <- df %>% 
  mutate(used = "TRUE")

#make df with all loaded packages (loaded in this script!!)
loaded_packages <- data.frame(packages = (.packages())) %>% 
  mutate(loaded = "TRUE")

warning("This script will compare the list of used packages to those loaded in the environment and session of this script. If need be, run scripts that load all packages you think you want first to make it a fair comparison.")

joined_df <- full_join(used_packages, loaded_packages)

#report unused but loaded pkgs
unused_but_loaded <- joined_df %>% 
  filter(is.na(used)) %>% 
  filter(!is.na(loaded)) %>% 
  dplyr::select(packages)

cat("There are ", nrow(unused_but_loaded), "packages that it seems like you're not using, but that are loaded.\n They are:\n" )

unused_but_loaded

warning("Note there may be errors, for example the script doesn't tend to understand tidyverse properly but checks each package inside." )

#check which are used the most (as in most unique functions called per package)
most_used <- used_packages %>% 
  distinct(packages, functions) %>% 
  group_by(packages) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

cat("The top 5 packages from which you use the most different functions are: ")
most_used[1:5,]

cat("Keep in mind, this is not top-5 per times you use the package but the top-5 of pacakges from which you use the most functions.")

most_used$packages <- fct_reorder(most_used$packages, most_used$n)

most_used %>% 
  ggplot() +
  geom_bar(aes(x = packages, y = n, fill = n), stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), 
        legend.position = 0) 

ggsave("used_packages.png")
