library(tidyverse)

lgs_locs <- read_tsv("output_tables/cldf_wide_df.tsv") %>% 
  dplyr::mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

load("example_data/south_down_woldmap_bw_flat.RData")

p_south_down +
  geom_jitter(data = lgs_locs, 
             mapping = aes(x = Longitude, y = Latitude, 
                           color = Family_ID, 
                           fill = Family_ID), 
             alpha = 0.7,
             shape = 21,
             inherit.aes = F) +
#  coord_map(projection = "vandergrinten") +
  xlim(c(78, 255)) +
  ylim(c(-56, 27)) +
  coord_equal(xlim = c(78, 255), ylim = c(-56,27)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.background = element_rect(colour = NA, fill = "#ebebeb"))

ggsave(filename = "output/pacific_map.png")
