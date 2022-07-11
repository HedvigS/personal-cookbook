source("requirements.R")

#reading in lg meta data
Language_meta_data <-  read_csv(GRAMBANK_LANGUAGES, col_types=LANGUAGES_COLSPEC) %>%		
  dplyr::select(Language_ID = Language_level_ID, Family_name, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name))

GB_mds <- read_tsv(file.path("MDS", "MDS_table.tsv")) %>% 
  dplyr::select(Language_ID, "MDS_classic_\nV1" = V1, "MDS_classic_\nV2" = V2)

GB_MDS_non_metric <- read_tsv(file.path("MDS", "MDS_non-metric_table.tsv")) %>% 
  dplyr::select(Language_ID, "MDS_non_metric\n_V1" = V1, "MDS_non_metric\n_V2" = V2)

GB_UMAP <- read_tsv("UMAP/umap_table.tsv") %>% 
  dplyr::select(Language_ID, "UMAP\n_V1" = V1, "UMAP\n_V2" = V2)

GB_PCA_df <- suppressMessages(read_tsv(file.path("PCA", 'PCA_language_values.tsv'))) %>%
  dplyr::select(Language_ID, PC1, PC2, PC3) 

joined <- full_join(GB_mds, GB_PCA_df) %>% 
  full_join(GB_MDS_non_metric) %>% 
  full_join(GB_UMAP) %>% 
  dplyr::select(PC1,  "MDS_classic_\nV1","MDS_non_metric\n_V1", "UMAP\n_V1", "PC2", "MDS_classic_\nV2","MDS_non_metric\n_V2", "UMAP\n_V2")

png("MDS/MDS_PCA_splom.png", height =20, width = 20, units = "cm", res = 400)
pairs.panels(joined, 
             method = "pearson", # correlation method
             hist.col = "#a3afd1",# "#a9d1a3","",""),
             density = TRUE,  # show density plots
             ellipses = F, # show correlation ellipses
             cex.labels= 1,
             #           smoother= T,
             cor=T,
             lm=T,
             ci = T, cex.cor = 0.9,stars = T
)

x <- dev.off()

###

joined %>% 
  ggplot(aes(x = "MDS_classic_\nV1", y = PC1)) +
  geom_point() +
  geom_point(color = "turquoise3") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme_classic() +
  labs(title="",		
       x ="MDS - V1", y = "PCA - PC1")

ggsave("MDS/mds_PCA_1_plot.png")

joined %>% 
  ggplot(aes(x = "MDS_classic_\nV1", y = PC2)) +
  geom_point() +
  geom_point(color = "sienna1") +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme_classic() +
  labs(title="",		
       x ="MDS - V2", y = "PCA - PC2")

ggsave("MDS/mds_PCA_2_plot.png")


