library(tidyverse)
library(Amelia)
library(rcldf) #package for reading in CLDF-datasets
library(rgrambank) #package with functions for common tasks with grambank-data
library(reshape2) # not necessary package for grambank, but used below for making glottolog ValueTable wide before joining to Grambank as an example.


grambank_cldf_object <- rcldf::cldf(mdpath = "https://zenodo.org/records/7844558/files/grambank/grambank-v1.0.3.zip",
                                    load_bib = FALSE)

before_cropping <- grambank_cldf_object$tables$ValueTable %>%
    filter(Value != "?") %>%
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>%
    Amelia::missmap()

source("../rgrambank/R/crop_missing_data.R")

grambank_ValueTable_cropped <- grambank_cldf_object$tables$ValueTable %>% crop_missing_data()

after_cropping <- grambank_ValueTable_cropped %>%
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>%
    Amelia::missmap()


grambank_ValueTable_cropped %>%
    group_by(Language_ID) %>%
    summarise(n = n()) %>% View()


glottolog_cldf <- rcldf::cldf(mdpath = "https://zenodo.org/records/8131091/files/glottolog/glottolog-cldf-v4.8.zip")
