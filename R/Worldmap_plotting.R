#This is a script for plotting points on a pacific-centered worldmap with points colored for a variable. I have exemplified with languages from glottolog-cldf, but it can easily be adapted to other data which has a long and a lat

#This script produces one map, with different settings depending on wether variable is ordered or not (viridis color scale or distinct colors) and depending on how many distinct values it has (legend or not). I have exemplified with glottolog's "med" (which has 5 values) and Family_ID (which has over 200).



##to note:
  # if this dataset has the glottolog parameters "level" and "Family_ID" then bookkeeping, pidgin, artificial, pidgin, speech registers, dialects and proto-languages are excluded because they do not meet the criteria of being a language per se.
  # if you are displaying language families, you can specify whether you want isolates to be lumped together into one category or distinct
  # you can specify wether the variable has an order or not, and set that order

#author: Hedvig Skirgård (github-user HedvigS)
#date: 2021-03-23

#This script can be adapted to fit to other CLDF-formatted data. Change the URLs below to the path of other values.csv and languages.csv if need be

#defining the data tables
values_csv_url <- "https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/values.csv"
languages_csv_url <- "https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv"

#defining the variables to be plotted (i.e. determine fill color of points)

#variable_to_be_plotted <- "med" #this string refers to a particular parameter with few distinct values in the dataset that is to determine the color in the plot
variable_to_be_plotted <- "Family_ID"  #this string refers to a particular parameter with many distinct values in the dataset that is to determine the color in the plot

#order of smaller value set
is_variable_ordered <- "No" #if the smaller value set is not categorical but has a order, set this to "Yes" and define the order below
variable_to_be_plotted_order_vec <-  c("long grammar", "grammar", "grammar sketch", "phonology/text",  "Wordlist or less") #this vector defines the desired order of unique values in the vector. It will be ignored if is_variable_ordered != "Yes".

#display isolates with distinct colors or all same color (only relevant if you're plotting Family_ID but can be specified regardless)
display_isolates_all_same_color <- "Yes"

##installing and loading necessary packages
#pacman is a package that facilitates neatly checking if pacakges are installed, installs them if not and loads them.
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  readr, #for reading in data files
  randomcoloR, #for creating large distinct color palette
  ggplot2, #for plotting
  maps, #necessary package for map plotting
  mapproj, #necessary package for map plotting
  viridis, #for color blind friendly palettes
  reshape2 #for making data wide/long with dcast (long -> wide) and melt (wide -> long)
  )

options(tidyverse.quiet = TRUE)

#creating a folder for outputting plots
if (!dir.exists("plots")) { dir.create("plots") }

#reading in data and making it wide
values <- readr::read_csv(values_csv_url, na = c("","<NA>"), col_types = cols()) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") #making long data wide

languages <- readr::read_csv(languages_csv_url, na = c("","<NA>"), col_types = cols()) %>% 
  rename(Language_level_ID = Language_ID) %>%  #The languages-table from glottolog-cldf contains a paramter called "Language_ID" which is NOT the same as the parameter "Language_ID" in the values tables. This parameter is in fact the language leveled parent of a dialect. In order to avoid confusion, let's rename the paramteer in the languages tables to the more transparent "Language_level_ID". This set-up works for other cldf-datasets, altough they shouldn't have two different parameters called "Language_ID" to start with. 
rename(Language_ID = ID)

cldf_wide_df <- dplyr::full_join(values,languages) 
 
#if isolates should all have same color or be displayed as distinct families 
if(display_isolates_all_same_color == "Yes" &  variable_to_be_plotted == "Family_ID"){cldf_wide_df <-  cldf_wide_df %>% 
  dplyr::mutate(Family_ID = ifelse(is.na(Family_ID), "Isolate", Family_ID)) #replacing NA in Family_ID with the string "Isolate". Only languoids which are isolates have missing values in Family_ID (if they are "language"-level.)
}

if(display_isolates_all_same_color == "No" & 
   variable_to_be_plotted == "Family_ID"){cldf_wide_df <-  cldf_wide_df %>% 
  dplyr::mutate(Family_ID = ifelse(is.na(Family_ID), Language_ID, Family_ID)) }

#If we are indeed wrangling data with the glottolog Family_ID and level parameters specifically, we can do some extra subsetting to remove languoids which do not qualify as "languages" per se.
if("level" %in% colnames(cldf_wide_df) & "Family_ID" %in% colnames(cldf_wide_df)) {cldf_wide_df <- cldf_wide_df %>% 
  dplyr::filter(Family_ID != 'book1242') %>% #removing bookkeeping languages (does not qualify as "real language")
  dplyr::filter(Family_ID != 'unat1236') %>% #removing unattested languages (does not qualify as "real language")
  dplyr::filter(Family_ID != 'arti1236') %>% #removing artificial languages (does not qualify as "real language")
  dplyr::filter(Family_ID != 'pidg1258') %>% #removing pidgin languages (does not qualify as "real language")
  dplyr::filter(Family_ID != 'spee1234') %>% #removing speech register (does not qualify as "real language")
  dplyr::filter(level =="language") #removing proto-languages and dialects
} 

cat("There are", sum(is.na(cldf_wide_df$Longitude)), "languages in your dataset with missing longitude/latitude.")

if(cldf_wide_df[[variable_to_be_plotted]] %>% unique() %>% length() > 10) {
small_or_large_set_distinct_values <- "large" } else{
  small_or_large_set_distinct_values <- "small"}

#ordering values after specified order
if(is_variable_ordered == "Yes"){
cldf_wide_df[[variable_to_be_plotted]] <- factor(cldf_wide_df[[variable_to_be_plotted]], levels = variable_to_be_plotted_order_vec)
}

##creating distinct color palette 
#This will create a set of distinct colors that corresponds to the number of unique values in the larger value parameter set and create an extra col in the df with HEX-codes corresponding to each color for each row.

if(is_variable_ordered == "No") {
set.seed(17) #random color will randomise the order of the colors. I find that for Family_ID, setting the random seed to 17 looks quite nice for the larger families.

n <- length(unique(cldf_wide_df[[variable_to_be_plotted]])) #counting how many distcint colors we need. Note that "NA" is also counted, and represents Isolates.

color_vector <- randomcoloR::distinctColorPalette(n)

cldf_wide_df$distinct_color_vec <- color_vector[as.factor(cldf_wide_df[[variable_to_be_plotted]])]
}


#worldmap
#rendering a base worldmap that is pacific centered

#fetching datasets
world <- ggplot2::map_data('world2', 
                           wrap=c(-25,335), #rewrapping the worldmap, i.e. shifting the center. I prefer this to world2 because I like to adjust the wrapping a bit differently, and world2 results in polygons leaking
                           ylim=c(-55,90)) #cutting out antarctica (not obligatory) and the northermost part where there are no language points in glottolog

lakes <- ggplot2:: map_data("lakes", 
                            wrap=c(-25,335), 
                            col="white", border="gray",  
                            ylim=c(-55,90))

#shifting the longlat of the dataframe to match the pacific centered map
cldf_wide_df_long_shifted <-cldf_wide_df %>% 
  dplyr::mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  dplyr::filter(!is.na(Longitude))

#Basemap
basemap <- ggplot(cldf_wide_df_long_shifted) +
  geom_polygon(data=world, aes(x=long, #plotting the landmasses
                               y=lat,group=group),
               colour="gray90",
               fill="gray90", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long,#plotting lakes
                               y=lat,group=group),
               colour="gray90",
               fill="white", size = 0.3)  +
  theme(#all of theme options are set such that it makes the most minimal plot, no legend, not grid lines etc
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") + #a non-rectangular world map projection that is a decen compromise between area and distances accuracy
  ylim(-55,90) #cutting out antarctica (not obligatory) 

#filename of image_file
fn <- paste0("plots/worldmap_", variable_to_be_plotted, ".png")
  
#plotting the variable with small set of values and non-ordered
#this plot will have distinct color values from randomcoloR and visible legend
if(is_variable_ordered == "No"){

p <- basemap + geom_point(stat = "identity", 
                            size = 0.6,
                            position = position_jitter(width = 1, height = 1 #jittering the points to prevent overplotting
                            ),
                            aes(x=Longitude, 
                                y=Latitude), 
                          fill = cldf_wide_df_long_shifted$distinct_color_vec, 
                          shape = 21, 
                          alpha = 0.6, 
                          stroke = 0.4, 
                          color = "grey44")

if(small_or_large_set_distinct_values =="small"){ #if there are few distinct values, we can afford a legend, otherwise let's omit it
p <- p + 
    theme(title  = element_text(size = 8),
          legend.text = element_text(size = 6), 
          legend.key  = element_blank()) } else {
            p <- p + theme(legend.position="none")
          }
plot(p)  
  ggsave(fn,  width =15, height = 10, units = "cm", dpi = 300)
}

#plotting small set (i.e. with legend) and ordered parameter
if(is_variable_ordered == "Yes"){
    
p <- basemap + geom_point(stat = "identity", 
                          size = 0.6,
                          position = position_jitter(width = 1, height = 1 #jittering the points to prevent overplotting
                            ),
                          aes_string(x="Longitude", #using aes_string in order to be able to dynamically refer to arguments (see "fill")
                                     y="Latitude", 
                                     fill =variable_to_be_plotted), 
                          
                          shape = 21, 
                          alpha = 0.6, 
                          stroke = 0.4, 
                          color = "grey44") +   
    scale_fill_viridis(discrete = T) 

if(small_or_large_set_distinct_values =="small"){ #if there are few distinct values, we can afford a legend, otherwise let's omit it
    p <- p + 
      theme(title  = element_text(size = 8),
            legend.text = element_text(size = 6), 
            legend.key  = element_blank()) } else {
              p <- p + theme(legend.position="none")
            }
  plot(p) 
  
ggsave(fn,  width =15, height = 10, units = "cm", dpi = 300)
}