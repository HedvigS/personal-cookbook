lg_spec <- "dutc1256"

#install.packages("remotes")
library(remotes)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("patchwork")
library(patchwork)
library(scales)

#remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE, upgrade = "never", ref = "v1.2.0")
library(rcldf)

#remotes::install_github("HedvigS/SH.misc", upgrade = "never")
library(SH.misc)

#remotes::install_github("HedvigS/rgrambank", upgrade = "never")
library(rgrambank)

#install.packages("cluster")
library(cluster)
#install.packages("reshape2")
library(reshape2)

#install.packages("viridis")
library(viridis)
#install.packages("ggstar")
library(ggstar)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

LanguageTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(Glottocode, ID, Language_level_ID = Language_ID, Longitude, Latitude, Name) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID)) %>%   mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) #shifting the longlat of the dataframe to match the pacific centered map

ValueTable <- GB_rcldf_obj_v1$tables$ValueTable %>% 
  rgrambank::make_binary_ValueTable(keep_multistate = F) %>% 
  rgrambank::reduce_ValueTable_to_unique_glottocodes(GlottologLanguageTable = LanguageTable,
                                                     merge_dialects = T,
                                                     replace_missing_language_level_ID = T,
                                                     LanguageTable = GB_rcldf_obj_v1$tables$LanguageTable, 
                                                     method = "singular_least_missing_data")

ValueTable_wide <-     ValueTable %>% 
  dplyr::mutate(Value = as.numeric(Value)) %>% 
  dplyr::filter(Value != "?") %>% 
  dplyr::group_by(Glottocode) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n > 201*0.75) %>% 
  reshape2::dcast(Glottocode ~ Parameter_ID, value.var = "Value") %>% 
  tibble::column_to_rownames("Glottocode") %>% 
  as.matrix()

dists <- cluster::daisy(x = ValueTable_wide, metric = "gower", warnBin = F)

dist_to_lg_spec <- dists %>% 
  as.matrix() %>% 
  as.data.frame() %>%
  dplyr::select(lg_spec = all_of(lg_spec)) %>% 
  rownames_to_column("Glottocode") %>% 
  dplyr::left_join(LanguageTable, by = "Glottocode") %>% 
  filter(Glottocode != {{lg_spec}})

readr::write_csv(dist_to_lg_spec, 
                 file = paste0("distances_to_", lg_spec, ".csv"), 
                 na = "", quote = "all")

#dist_to_lg_spec$lg_spec %>% mean()

lg_spec_df <- LanguageTable %>% 
  dplyr::filter(Glottocode == lg_spec)


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

#hist


mh <- map + inset_element(hist, right = 0.37, bottom = 0.01, left = 0.225, top = 0.32) 

#mh

ggsave(filename = paste0("distance_to_", lg_spec, ".png"), width = 20, height = 10, dpi = 300, units = "in")

