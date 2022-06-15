
#function to check if a pkg is installed or not, if not it installs it and either way it's loaded.
#inspired by pacman::p_load()

h_load <- function(pkg, verbose = F, version = NULL, repos = "http://cran.us.r-project.org"){
#  p <- "ggpubr"
#  pkg <- c("geiger", "ape")
      for(p in pkg){
        
      if(is.null(version) & (!(p %in% rownames(installed.packages())))){ #if no version is specified, check if it's installed and if not then go and install it as normal
        
        install.packages(p, dependencies = T, repos = repos)
        
        if(verbose == T){
          cat(paste0("Installed ", p, ".\n"))}

      }
      if(!is.null(version) & (!(p %in% rownames(installed.packages())))){    
      
      if(!"devtools"%in% rownames(installed.packages())){ #install devtools if it isn't already installed
          install.packages(devtools, dependencies = T, repos = repos)
      }
        
        library(devtools, quietly = T, verbose = F, warn.conflicts = F)
        devtools::install_version(p, version = version, dependencies = T, repos = repos)
        
        if(verbose == T){
          cat(paste0("Installed ", p, ", version ", version, ".\n"))}
      }
      
  if(verbose == T){
    library(p, character.only = T, quietly = F)
    cat(paste0("Loaded ", p, ", version ",packageVersion(p),".\n"))
  }else{
    suppressMessages(library(p, character.only = T, quietly = T, verbose = F, warn.conflicts = F))}
  
      }
}
