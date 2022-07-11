source("requirements.R")

### MACROAREA

#reading in data
phist_macroarea_rdata <- readRDS("output/dist_fixation_scores/phist_macroarea.rdata")

#extracting matrix
phist_macroarea_matrix <- phist_macroarea_rdata$PhiST 

#making symmetric
phist_macroarea_matrix[upper.tri(phist_macroarea_matrix)] <- t(phist_macroarea_matrix)[upper.tri(phist_macroarea_matrix)]

  
phist_macroarea_matrix %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = value, y = Var1)) +
  geom_label_repel(aes(label = Var2, col = Var2), label.size = 1, size = 7) +
  geom_point(aes(col = Var2)) +
  theme_classic() +
  theme(legend.position = "None", 
        text = element_text(size = 20), 
        axis.title.y = element_blank()) +
  xlab("PHiST score")

ggsave("output/dist_fixation_scores/phist_macroarea_point_plot.png")
  