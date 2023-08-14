#Script was written by Hedvig Skirgård
#This script downloads datasets from zenodo that have been released via github, exemplfiying with grambank as the zenodo URL, but you can insert any valid Zenodo download URL.

#at the end of this script is a function-version of the same code.

#if you need to download something like grambank-analysed which has inside of it other datasets, look at this script: https://github.com/OlenaShcherbakova/Sociodemographic_factors_complexity/blob/main/get_external_data.R


#setting up a tempfile path where we can put the zipped files before unzipped to a specific location
filepath <- file.path(tempfile())

##grambank: downloading, zipping and moving
Zenodo_url <- c("https:\\\\zenodo.org\\record\\7740140\\files\\grambank\\grambank-v1.0.zip")
exdir <- "output\\grambank\\"

utils::download.file(file.path(Zenodo_url), destfile = filepath)
utils::unzip(zipfile = filepath, exdir = exdir)

#Zenodo locations contain a dir with the name of the repos and the commit in the release. This is not convenient for later scripts, so we move the contents up one level
 
old_fn <- list.files(exdir, full.names = T)
old_fn_files <- list.files(old_fn, full.names = T, recursive = T)

file.copy(from = old_fn_files,to = exdir, recursive = T, overwrite = T)

#remove old dir and all of its contents
unlink(old_fn, recursive = T)

cat("
      |\\      _,,,---,,_
      /,`.-'`'    -.  ;-;;,_
     |,4-  ) )-,_..;\\ (  `'-'
    '---''(_/--'  `-'\\_) \n" ,
    "Hurray!")


##function version. You give the function the URL and the name you want of the dir that it expands it into. if you set remove_git_commit_dir it will remove the first order dir you've downloaded.
get_zenodo_dir <- function(url, exdir, remove_git_commit_dir = T){
  
  #  url <- c("https://zenodo.org/record/7740822/files/grambank/grambank-analysed-v1.0.zip")
  #  exdir = c("grambank-analysed")
  #  remove_git_commit_dir = T
  #setting up a tempfile path where we can put the zipped files before unzipped to a specific location
  filepath <- file.path(tempfile())
  
  utils::download.file(file.path(url), destfile = filepath)
  utils::unzip(zipfile = filepath, exdir = exdir)
  
  if(remove_git_commit_dir == T){
    #Zenodo locations contain a dir with the name of the repos and the commit in the release. This is not convenient for later scripts, so we move the contents up one level
    old_fn <- list.files(exdir, full.names = T)
    old_fn_files <- list.files(old_fn, full.names = T, recursive = T)
    
    x <- file.copy(from = old_fn_files,to = exdir, recursive = T, overwrite = T)
    #remove old dir
    unlink(old_fn, recursive = T)
  }
  cat(paste0("Done with downloading ", exdir, ".\n"))
}
