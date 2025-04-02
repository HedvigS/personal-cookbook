library(tidyverse)
library(rnaturalearth)
library(sf)
library(lwgeom)

world_whole <- sf::st_read("example_data/World_EEZ_v12_20231025/eez_v12.shp")

# Step 2: Filter for countries you are interested in (NZL, FRA, GBR)
world_selected <- world_whole %>% 
  filter(ISO_SOV1 %in% c("GBR", "BRA", "ATA", "FRA", "NZL", "AUS")) #%>% 
#  filter(TERRITORY1 == "South Georgia and the South Sandwich Islands")

ggplot(data = world_whole) +
  geom_sf(aes(fill = ISO_SOV1)) +
  coord_sf(crs = "+proj=eqearth +lon_0=160") + 
  theme_minimal()

world$GEONAME # New Zealand Exclusive Economic Zone (Tokelau)

world_whole$GEONAME %>% as.data.frame() %>% View()

#  dplyr::mutate(region = ifelse(region == "Virgin Islands" & subregion == " British",
#                                "British Virgin Islands", region)) %>% 





df <- data.frame(ISO_SOV1 = world_whole$ISO_SOV1, 
                 GEONAME = world_whole$GEONAME, 
                 X_1 = world_whole$X_1, 
                 Y_1 = world_whole$Y_1)
df <- data.frame(region = c("Antigua",  
"Barbuda",
"Bolivia",
"Chile",
"China",
"Republic of Congo",
"Ivory Coast", #Côte d'Ivoire",
"Cuba",
"Dominica",
"Ecuador",
"Ethiopia",
"Fiji",
"Grenada",
"India",
"Indonesia",
"Iran",
"Iraq",
"Mali",
"Nicaragua",
"Papua New Guinea",
"Russia",
"Saint Kitts",
"Nevis",
"Saint Lucia",
"Saint Vincent",   
"Grenadines",
"Sierra Leone",
"Syria",
"Tanzania",
"Timor-Leste",
"Tunisia",
"Venezuela"), in_data = "yes"
)


df_non_goverened <- data.frame( region = c("American Samoa", #United States Exclusive Economic Zone (American Samoa)
"Anguilla",#British Exclusive Economic Zone (Anguilla)
"Bermuda", #British Exclusive Economic Zone (Bermuda)
"British Virgin Islands", #	British Exclusive Economic Zone (Pitcairn)
"Cayman Islands", #British Exclusive Economic Zone (Cayman Islands) #Joint regime area: Honduras / United Kingdom (Cayman Islands)
"Falkland Islands", #Overlapping claim Falkland / Malvinas Islands: United Kingdom / Argentina
"French Polynesia", #French Exclusive Economic Zone (French Polynesia)
"Gibraltar", #Overlapping claim Gibraltar: United Kingdom / Spain
"Guam", #United States Exclusive Economic Zone (Guam)
"Montserrat", #British Exclusive Economic Zone (Montserrat)
"New Caledonia", #French Exclusive Economic Zone (New Caledonia)
"Pitcairn Islands", #British Exclusive Economic Zone (Pitcairn)
"Saint Helena", #British Exclusive Economic Zone (Saint Helena)
"Tokelau", #New Zealand Exclusive Economic Zone (Tokelau)
"Turks and Caicos Islands", #British Exclusive Economic Zone (Turks and Caicos Islands)
"U.S. Virgin Islands", #United States Exclusive Economic Zone (United States Virgin Islands)
"Western Sahara"), #Overlapping claim Western Sahara: Western Sahara / Morocco
in_data_non_governed = "yes"
)

world$region %>% unique() %>% sort()


df_non_goverened  %>% 
  left_join(world) %>% 
  filter(is.na(order)) %>% View()
