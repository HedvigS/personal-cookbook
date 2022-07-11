source("requirements.R")
p_load("umap")

#reading in lg meta data
Language_meta_data <-  read_csv(GRAMBANK_LANGUAGES, col_types=LANGUAGES_COLSPEC) %>%		
  dplyr::select(Language_ID = Language_level_ID, Family_name, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name))

#outputdir
OUTPUTDIR <- file.path("UMAP")
if (!dir.exists(OUTPUTDIR)) { dir.create(OUTPUTDIR) }	

GB_imputed <- read_tsv(file.path("GB_wide", "GB_wide_imputed_binarized.tsv"), col_types= cols()) %>%
  column_to_rownames("Language_ID") %>%
  as.matrix()

GB_umap <- umap::umap(GB_imputed) 

GB_umap_df <- GB_umap %>% 
  .$layout %>% 
  as.data.frame() %>% 
  rownames_to_column("Language_ID") %>% 
  left_join(Language_meta_data)

GB_umap_df %>% 
  write_tsv("UMAP/umap_table.tsv", na = "")


##plotting
plotUMAPFamily <- function(df, family, color="orange", key="Family_name") {
  df$InGroup <- df[[key]] == family
  p <- ggplot(df, aes(x = V1, y = V2), alpha = 1)
  p <- p + geom_jitter(color="snow3", size = 0.5, alpha = 0.4, shape = 16)
  p <- p + geom_jitter(
    data = filter(df, df$InGroup),
    aes(x =V1, y = V2), color=color,
    size = 0.5, alpha = 1
  )
  p <- p + geom_density2d(
    data = filter(df, df$InGroup),
    aes(x = V1, y = V2), color = color, alpha=0.4
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


#Mayan, Turkic, Ta-Ne-Omotic Algic, Chibchan
GB_UMAP_Families <- list(
  plotUMAPFamily(GB_umap_df, "Afro-Asiatic", "#FF5733"),
  plotUMAPFamily(GB_umap_df, "Atlantic-Congo", "steelblue2"),
  plotUMAPFamily(GB_umap_df, "Austroasiatic", "midnightblue"),
  plotUMAPFamily(GB_umap_df, "Austronesian", "turquoise3"),
  plotUMAPFamily(GB_umap_df, "Central Sudanic", "purple4"),
  plotUMAPFamily(GB_umap_df, "Chibchan", "#E08B00"),
  plotUMAPFamily(GB_umap_df, "Dravidian", "blue"),
  plotUMAPFamily(GB_umap_df, "Indo-European", "seagreen"),
  plotUMAPFamily(GB_umap_df, "Mayan", "#00ABFD"),
  plotUMAPFamily(GB_umap_df, "Nakh-Daghestanian", "orange"),
  plotUMAPFamily(GB_umap_df, "Nuclear Trans New Guinea", "springgreen"),
  plotUMAPFamily(GB_umap_df, "Pama-Nyungan", "darkgreen"),
  plotUMAPFamily(GB_umap_df, "Sino-Tibetan", "violetred3"),
  plotUMAPFamily(GB_umap_df, "Turkic", "#FF689F"),
  plotUMAPFamily(GB_umap_df, "Uralic", "#DC71FA"),
  plotUMAPFamily(GB_umap_df, "Uto-Aztecan", "red1")
)



# save each one
for (p in GB_UMAP_Families) {
  filename <- file.path(OUTPUTDIR, sprintf("UMAP_family_%s.tiff",  p$labels$title))
  cat(paste("writing", filename, "\n"))
  ggsave(filename, p, width = 4, height = 4)
}


GB_UMAP_multiGrid_family <- ggarrange(
  plotUMAPFamily(GB_umap_df, "Afro-Asiatic", "#FF5733"),
  plotUMAPFamily(GB_umap_df, "Atlantic-Congo", "steelblue2"),
  plotUMAPFamily(GB_umap_df, "Austroasiatic", "midnightblue"),
  plotUMAPFamily(GB_umap_df, "Austronesian", "turquoise3"),
  plotUMAPFamily(GB_umap_df, "Central Sudanic", "purple4"),
  plotUMAPFamily(GB_umap_df, "Chibchan", "#E08B00"),
  plotUMAPFamily(GB_umap_df, "Dravidian", "blue"),
  plotUMAPFamily(GB_umap_df, "Indo-European", "seagreen"),
  plotUMAPFamily(GB_umap_df, "Mayan", "#00ABFD"),
  plotUMAPFamily(GB_umap_df, "Nakh-Daghestanian", "orange"),
  plotUMAPFamily(GB_umap_df, "Nuclear Trans New Guinea", "springgreen"),
  plotUMAPFamily(GB_umap_df, "Pama-Nyungan", "darkgreen"),
  plotUMAPFamily(GB_umap_df, "Sino-Tibetan", "violetred3"),
  plotUMAPFamily(GB_umap_df, "Turkic", "#FF689F"),
  plotUMAPFamily(GB_umap_df, "Uralic", "#DC71FA"),
  ncol = 3, nrow = 5)


GB_UMAP_multiGrid_family <- GB_UMAP_multiGrid_family + rremove("x.text")
GB_UMAP_multiGrid_family <- annotate_figure(
  GB_UMAP_multiGrid_family,
  bottom = text_grob("V1", size = 18, vjust = -0.1),
  left = text_grob("V2", size = 18, rot = 90, hjust = -1, vjust = 1)
)

filename <- file.path(OUTPUTDIR, "UMAP_multigrid_major_families.tiff")
filename_png <- file.path(OUTPUTDIR, "UMAP_multigrid_major_families.png")
cat(paste("writing", filename, "\n"))
ggsave(filename, GB_UMAP_multiGrid_family, width = 8, height = 8)
ggsave(filename_png, GB_UMAP_multiGrid_family, width = 8, height = 8)