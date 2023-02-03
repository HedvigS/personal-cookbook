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

# dealing with instances where a package wasn't found. in pipe above this was listed as "" but it should be a proper NA
used_packages <- naniar::replace_with_na(data = used_packages, replace= list(packages = ""))
used_packages$packages <- trimws(used_packages$packages)


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


script_with_most_functions <-  used_packages %>% 
  distinct(scripts, functions) %>% 
  group_by(scripts) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

cat("The top 5 scripst which use the most differenty functions:\n ")
script_with_most_functions [1:5,]

packages_in_most_scripts <-  used_packages %>% 
  distinct(scripts, packages) %>% 
  group_by(packages) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

cat("The top 5 packages that are used in the most scripts:\n ")
packages_in_most_scripts[1:5,]

#generating bibtex file of all the packages where you've used at least one funciton
h_load("knitr")

output_fn <- "used_pkgs.bib"

knitr::write_bib(used_packages$packages, file = output_fn)

cat(paste0("Wrote citations for packages you've used to", output_fn, ".\n There were ", length(!is.na(used_packages$packages %>% unique()))
           , " entries.\n" ))

#optional part, this generates a text string with the bibtex citation KEYS from earlier that you can then paste into LaTeX in order to cite all

h_load("bib2df")

bibdf <- suppressMessages(bib2df(output_fn)) #there's an old bug in bib2df that warns that as.tibble is deprecated, I've suppress this to spare you. the package maintainer has been informed. it's not a serious warning

bibdf$BIBTEXKEY %>% 
  writeLines(sep = ", ", con = "citation_keys.txt")

