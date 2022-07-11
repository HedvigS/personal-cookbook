source("requirements.R")

#script written by Hedvig Skirgård

#outputdir
OUTPUTDIR <- file.path("MDS")
if (!dir.exists(OUTPUTDIR)) { dir.create(OUTPUTDIR) }		

#reading in GB
GB <- read.delim(file.path("GB_wide", "GB_cropped_for_missing.tsv"), sep ="\t") %>%
  column_to_rownames("Language_ID") %>%
  as.matrix()

#reading in lg meta data
Language_meta_data <-  read_csv(GRAMBANK_LANGUAGES, col_types=LANGUAGES_COLSPEC) %>%		
  dplyr::select(Language_ID = Language_level_ID, Family_name, Name, Macroarea) %>% 
  distinct(Language_ID, .keep_all = T) %>% 
  mutate(Family_name = ifelse(is.na(Family_name), "Isolate", Family_name))

#calculating gower distances %>% 
GB_dist <- GB %>% 
  cluster::daisy(metric = "gower", warnBin = F) %>% 
  as.matrix()

rownames(GB_dist) <- rownames(GB)
colnames(GB_dist) <- rownames(GB)

#mds
GB_MDS <- cmdscale(GB_dist) %>% 
  as.data.frame() %>% 
  rownames_to_column("Language_ID") %>% 
  left_join(Language_meta_data, by = "Language_ID") 

GB_MDS %>% 
write_tsv(file.path(OUTPUTDIR, "MDS_table.tsv"), na = "")

#plotting


plotMDSFamily <- function(df, family, color="orange", key="Family_name") {
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
GB_MDS_Families <- list(
  plotMDSFamily(GB_MDS, "Afro-Asiatic", "#FF5733"),
  plotMDSFamily(GB_MDS, "Atlantic-Congo", "steelblue2"),
  plotMDSFamily(GB_MDS, "Austroasiatic", "midnightblue"),
  plotMDSFamily(GB_MDS, "Austronesian", "turquoise3"),
  plotMDSFamily(GB_MDS, "Central Sudanic", "purple4"),
  plotMDSFamily(GB_MDS, "Chibchan", "#E08B00"),
  plotMDSFamily(GB_MDS, "Dravidian", "blue"),
  plotMDSFamily(GB_MDS, "Indo-European", "seagreen"),
  plotMDSFamily(GB_MDS, "Mayan", "#00ABFD"),
  plotMDSFamily(GB_MDS, "Nakh-Daghestanian", "orange"),
  plotMDSFamily(GB_MDS, "Nuclear Trans New Guinea", "springgreen"),
  plotMDSFamily(GB_MDS, "Pama-Nyungan", "darkgreen"),
  plotMDSFamily(GB_MDS, "Sino-Tibetan", "violetred3"),
  plotMDSFamily(GB_MDS, "Turkic", "#FF689F"),
  plotMDSFamily(GB_MDS, "Uralic", "#DC71FA"),
  plotMDSFamily(GB_MDS, "Uto-Aztecan", "red1")
)



# save each one
for (p in GB_MDS_Families) {
  filename <- file.path(OUTPUTDIR, sprintf("MDS_family_%s.tiff",  p$labels$title))
  cat(paste("writing", filename, "\n"))
  ggsave(filename, p, width = 4, height = 4)
}


GB_MDS_multiGrid_family <- ggarrange(
  plotMDSFamily(GB_MDS, "Afro-Asiatic", "#FF5733"),
  plotMDSFamily(GB_MDS, "Atlantic-Congo", "steelblue2"),
  plotMDSFamily(GB_MDS, "Austroasiatic", "midnightblue"),
  plotMDSFamily(GB_MDS, "Austronesian", "turquoise3"),
  plotMDSFamily(GB_MDS, "Central Sudanic", "purple4"),
  plotMDSFamily(GB_MDS, "Chibchan", "#E08B00"),
  plotMDSFamily(GB_MDS, "Dravidian", "blue"),
  plotMDSFamily(GB_MDS, "Indo-European", "seagreen"),
  plotMDSFamily(GB_MDS, "Mayan", "#00ABFD"),
  plotMDSFamily(GB_MDS, "Nakh-Daghestanian", "orange"),
  plotMDSFamily(GB_MDS, "Nuclear Trans New Guinea", "springgreen"),
  plotMDSFamily(GB_MDS, "Pama-Nyungan", "darkgreen"),
  plotMDSFamily(GB_MDS, "Sino-Tibetan", "violetred3"),
  plotMDSFamily(GB_MDS, "Turkic", "#FF689F"),
  plotMDSFamily(GB_MDS, "Uralic", "#DC71FA"),
  ncol = 3, nrow = 5)


GB_MDS_multiGrid_family <- GB_MDS_multiGrid_family + rremove("x.text")
GB_MDS_multiGrid_family <- annotate_figure(
  GB_MDS_multiGrid_family,
  bottom = text_grob("V1", size = 18, vjust = -0.1),
  left = text_grob("V2", size = 18, rot = 90, hjust = -1, vjust = 1)
)

filename <- file.path(OUTPUTDIR, "MDS_multigrid_major_families.tiff")
filename_png <- file.path(OUTPUTDIR, "MDS_multigrid_major_families.png")
cat(paste("writing", filename, "\n"))
ggsave(filename, GB_MDS_multiGrid_family, width = 8, height = 8)
ggsave(filename_png, GB_MDS_multiGrid_family, width = 8, height = 8)
