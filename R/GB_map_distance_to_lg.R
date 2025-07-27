
#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

library(patchwork)
library(scales)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#install_github("HedvigS/SH.misc")
library(SH.misc)

library(cluster)
library(reshape2)
library(viridis)
#install.packages("ggstar")
library(ggstar)

dir <- "R_grambank_cookbook"
if(!dir.exists(dir)){dir.create(dir)}

#fetching from head at main origin
SH.misc::fetch_lines(url = "https://github.com/HedvigS/R_grambank_cookbook/raw/main/functions/make_binary_ValueTable.R", 
                     out_dir = dir)
source("R_grambank_cookbook/make_binary_ValueTable.R")

SH.misc::fetch_lines(url = "https://github.com/HedvigS/R_grambank_cookbook/raw/main/functions/reduce_ValueTable_to_unique_glottocodes.R", 
                     out_dir = dir)
source("R_grambank_cookbook/reduce_ValueTable_to_unique_glottocodes.R")

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

LanguageTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(Glottocode, ID, Language_level_ID = Language_ID, Longitude, Latitude, Name) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID)) %>%   mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) #shifting the longlat of the dataframe to match the pacific centered map
  
ValueTable <- GB_rcldf_obj_v1$tables$ValueTable %>% 
  make_binary_ValueTable(keep_raw_binary = T, keep_multistate = F) %>% 
  reduce_ValueTable_to_unique_glottocodes(LanguageTable = LanguageTable, merge_dialects = T, method = "singular_least_missing_data", treat_question_mark_as_missing = T)

ValueTable_wide <-     ValueTable %>% 
    mutate(Value = as.numeric(Value)) %>% 
    filter(Value != "?") %>% 
    group_by(Glottocode) %>% 
    mutate(n = n()) %>% 
    filter(n > 201*0.75) %>% 
  reshape2::dcast(Glottocode ~ Parameter_ID, value.var = "Value") %>% 
  column_to_rownames("Glottocode") %>% 
  as.matrix()

dists <- cluster::daisy(x = ValueTable_wide, metric = "gower", warnBin = F)

lg_spec <- "dutc1256"

dist_to_lg_spec <- dists %>% 
  as.matrix() %>% 
  as.data.frame() %>%
  dplyr::select(lg_spec = all_of(lg_spec)) %>% 
  rownames_to_column("Glottocode") %>% 
  dplyr::left_join(LanguageTable, by = "Glottocode") %>% 
  filter(Glottocode != {{lg_spec}})

write_csv(dist_to_lg_spec, "distances_to_dutch.csv", na = "", quote = "all")

dist_to_lg_spec$lg_spec %>% mean()

lg_spec_df <- LanguageTable %>% 
  filter(Glottocode == lg_spec)



#
viridis_option <- "plasma"
viridis_begin = 0.15
viridis_end = 0.1

################

basemap <- SH.misc::basemap_EEZ(south = "down", colour_border_eez = "lightgray", colour_border_land = "white", padding = 0) 

map <- basemap + 
  geom_jitter(data = dist_to_lg_spec, mapping = aes(x = Longitude, y = Latitude, colour = lg_spec), size = 5, alpha = 0.8) +
  scale_color_viridis(direction = -1, option = viridis_option) +
  geom_star(data = lg_spec_df, mapping = aes(x = Longitude, y = Latitude), fill = "black", size = 5) +

  ggtitle(paste0("Language similarity to ",   lg_spec_df$Name, " [", lg_spec, "], given the Grambank questionnaire (v1.0)")) 
  
#  ggtitle(paste0("This plot shows the grammatical similarity of languages (represented by points) to ", lg_spec_df$Name, " [", lg_spec, "] specifically. \nPlease note that this shows specifically grammatical similarities as measured by Grambank (v1). \nIt does not show vocabulary similarity. Languages can be similar grammatically because they are related to each other, \nbecause they've been in contact or out of coincidence (there are only so many different ways a grammar can work, \nsometimes different languages hit  on the same patterns independently). Please interpret carefully.\nLanguage location data is taken from Glottolog 5.0. Visualisation by Hedvig Skirgård, using the R-packages ggplot and SH.misc." ), ) +
#  theme(plot.title = element_text(vjust=-225,face = "bold", color = "#481567ff", size = 12))

  
  #making histogram
  
  dist_to_lg_spec_summary <- dist_to_lg_spec %>% 		
  transform(bin = cut(lg_spec, 20)) %>% #grouping the scores into 20 bins		
  group_by(bin) %>% 		
  dplyr::summarize(n = n())		

# Tidy up the tick labels for the bin columns in the graph		
# as the plain output from cut() was a little bit messy.		
dist_to_lg_spec_summary$bin_display <- dist_to_lg_spec_summary$bin %>% 		
  str_replace_all("[\\(|\\[|\\]]", "") 		

dist_to_lg_spec_summary <- dist_to_lg_spec_summary %>% 		
  tidyr::separate(col = bin_display, sep = ",", into = c("low", "high")) %>% 		
  mutate(bin_display = round(as.numeric(low), digits = 2)) %>% 		
  mutate(high = round(as.numeric(high), digits = 2))# %>% 		
 # unite(low, high, col = bin_display, sep = "-") 		

hist <-   ggplot() +
    geom_bar(data = dist_to_lg_spec_summary, aes(x = factor(bin_display), y = n, fill = factor(bin_display),
                                                 color  = factor(bin_display)
                                                 ), 
             stat = "identity", 
#            colour = "grey25", 
            linewidth = 0.5, alpha = 0.9) +
  scale_color_viridis(direction = -1, discrete = T, option = viridis_option) +
  scale_fill_viridis(direction = -1, discrete = T, option = viridis_option) +
  theme_minimal()+		
  theme(      plot.background = element_rect(fill = "#edeff2",
                                       colour = "#edeff2"),
              axis.text.x = element_text(size = 9, angle = 30, 
                                         hjust=0.95
                                         ), 		
              axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        legend.position = "None") 		+
  scale_x_discrete(breaks= dist_to_lg_spec_summary$bin_display[c(1,7, 13, 19)]  ) +
#  xlab(label = paste0("Distance to ",   lg_spec_df$Name, " [", lg_spec, "]")) + 
  ylab("Number of Languages") +
  ylim(0, 300)
 
hist


mh <- map + inset_element(hist, right = 0.37, bottom = 0.01, left = 0.225, top = 0.32) 
  
mh

ggsave("distance_to_lg_spec.png", width = 20, height = 10, dpi = 300, units = "in")

