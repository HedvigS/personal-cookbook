library(tidyverse)
library(reshape2)


# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

df_wide <- GB_rcldf_obj$tables$ParameterTable %>% 
  dplyr::select(ID, Boundness, Locus_of_Marking, Word_Order, Informativity, Flexivity, Gender_or_Noun_Class) %>% 
  dplyr::mutate(Boundness = ifelse(Boundness == 0, NA, Boundness)) %>% 
  filter(!is.na(Boundness) |
           !is.na(Locus_of_Marking) |
           !is.na(Informativity) |
           !is.na(Flexivity) |
           !is.na(Gender_or_Noun_Class) |
           !is.na(Word_Order))

df_long <- df_wide %>% 
  reshape2::melt(id.vars = "ID") %>% 
  mutate(value = ifelse(is.na(value), 0, 1))

sort_order <- df_wide %>%
  arrange(Boundness,  Locus_of_Marking, Informativity, Word_Order) %>%
  pull(ID)

# Reorder Sample levels in long-format data
df_long$ID <- factor(df_long$ID, levels = sort_order)

df_long$variable <-   factor(df_long$variable, 
                                levels = c("Boundness", 
                                           "Locus_of_Marking" ,
                                           "Informativity",
                                           "Flexivity", 
                                           "Gender_or_Noun_Class", 
                                           "Word_Order"))  

p <- df_long %>% 
ggplot(aes(x = ID, y = variable, fill = value)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Heatmap of Values", x = "", y = "") +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 30, hjust = 1),
        title = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_text(size = 4),
        axis.ticks = element_blank()) +
  coord_flip()


p
ggsave(plot = p, "GB_parameters_indices.png", height = 8, width = 7)

