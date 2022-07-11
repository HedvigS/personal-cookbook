source("requirements.R")

Language_meta_data <-  read_tsv("output/non_GB_datasets/glottolog-cldf_wide_df.tsv", col_types = cols()) %>% 
  dplyr::select(Language_ID, level, Family_ID, Name, Macroarea) %>% 
  mutate(Family_ID = ifelse(is.na(Family_ID), "Isolate", Family_ID)) %>% 
  mutate(Macroarea = ifelse(Family_ID == "Isolate", "Isolate", Macroarea)) %>% 
  filter(!is.na(Macroarea)) %>% 
  filter(level == "language") %>% 
    group_by(Family_ID, Macroarea) %>%
  summarise(n = n(), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  distinct(Family_ID, .keep_all = T)
  
df_long <- read_tsv("output/dist_fixation_scores/cfx_Family_ID_cut_off_1_list.tsv", show_col_types = F) %>% 
  separate("Vars", into = c("Var1", "Var2"), sep = " - ")

#recover symmetric distance matrix
h_load("igraph")
g <- igraph::graph.data.frame(df_long, directed=FALSE)

Family_ID_matrix <- igraph::get.adjacency(g, attr="Value_cfx", sparse=FALSE)

mds <- MASS::isoMDS(Family_ID_matrix)

mds_df <- mds$points %>% 
  as.data.frame() %>% 
  rownames_to_column("Family_ID") %>% 
  left_join(Language_meta_data, by = "Family_ID")

# The palette with black:
cbbPalette <- c( "#E69F00", "#009E73", "#F0E442", "#D55E00", "#CC79A7","#000000")


mds_plot <- mds_df %>% 
  ggplot() +
  geom_point(aes(x = V1, y = V2, color = Macroarea)) +
  theme_minimal() +
  coord_fixed() +
  scale_colour_manual(values=cbbPalette)

png("output/dist_fixation_scores/MDS_plot_family_cutoff_10.png", width = 6, height = 6, units = "in", res = 300)
plot(mds_plot)
x <- dev.off()
