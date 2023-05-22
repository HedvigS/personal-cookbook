#Script was written by Hedvig Skirgård
#This script downloads datasets from zenodo that have been released via github, exemplfiying with grambank as the zenodo URL, but you can insert any valid Zenodo download URL.


#if you need to download something like grambank-analysed which has inside of it other datasets, look at this script: https://github.com/OlenaShcherbakova/Sociodemographic_factors_complexity/blob/main/get_external_data.R

#loading packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.
pacman::p_load(
  tidyverse #for data wrangling
)


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
