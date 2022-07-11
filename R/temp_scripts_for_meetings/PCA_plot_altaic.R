source("requirements.R")



#Turkic, Mongolic and Tungusic language families and possibly also the Japonic and Koreanic languages.[1


OUTPUTDIR <- file.path('.', 'PCA')

Language_meta_data <-  read_csv(GRAMBANK_LANGUAGES, col_types=LANGUAGES_COLSPEC) %>%		
  dplyr::select(Language_ID = Language_level_ID, Family_name, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name))

#reading in the dataframe with PCA values for each language
GB_PCA_df <- suppressMessages(read_tsv(file.path(OUTPUTDIR, 'PCA_language_values.tsv'))) %>%
  dplyr::select(Language_ID, PC1, PC2, PC3) %>% 
  left_join(Language_meta_data, by = "Language_ID" ) %>%
  dplyr::select(Language_ID, everything()) %>% 
  mutate(Family_name = ifelse(Family_name == "Koreanic" | Family_name == "Japonic", "Koreanic-Japonic", Family_name))

# function to plot the PCA locations of a given language `family``
# highlighting the selected family using `color`.
plotPCAFamily <- function(df, family, color="orange", key="Family_name") {
  df$InGroup <- df[[key]] == family
  p <- ggplot(df, aes(x = PC1, y = PC2), alpha = 1)
  p <- p + geom_jitter(color="snow3", size = 0.5, alpha = 0.4, shape = 16)
  p <- p + geom_jitter(
    data = filter(df, df$InGroup),
    aes(x = PC1, y = PC2), color=color,
    size = 0.5, alpha = 1
  )
  p <- p + geom_density2d(
    data = filter(df, df$InGroup),
    aes(x = PC1, y = PC2), color = color, alpha=0.4
  )
  p <- p + coord_fixed()
  p <- p + theme_classic()
  p <- p + theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
  p <- p + ggtitle(family)
  p
}

GB_PCA_multiGrid_family <- ggarrange(
  plotPCAFamily(GB_PCA_df, "Turkic", "#FF5733"),
  plotPCAFamily(GB_PCA_df, "Koreanic-Japonic", "steelblue2"),
  plotPCAFamily(GB_PCA_df, "Tungusic", "midnightblue"),
  plotPCAFamily(GB_PCA_df, "Mongolic-Khitan", "purple4"))

ggsave(filename = file.path(OUTPUTDIR, "Altaic_plot.png"))
