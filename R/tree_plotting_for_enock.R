library(phytools)
library(tidyverse)
library(ape)
library(viridis)

sahul <- read_tsv("https://raw.githubusercontent.com/HedvigS/personal-cookbook/main/R/example_data/Sahul_structure_wide.tsv")  %>% 
  dplyr::select(glottocode, SAHUL003, SAHUL031) 

tree <- ape::read.nexus("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/gray_et_al2009/summary.trees")

taxa <- read_csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/gray_et_al2009/taxa.csv")

tree$tip.label <- tree$tip.label %>% 
  as.data.frame() %>% 
  rename(taxon = ".") %>% 
  left_join(taxa) %>% 
  dplyr::select(glottocode) %>% 
  .[,1]


tree_df <- tree$tip.label %>% 
  as.data.frame() %>% 
  rename(glottocode = ".") %>% 
  inner_join(sahul) %>% 
  group_by(glottocode) %>% 
  sample_n(1)

tree <- ape::keep.tip(tree, tree_df$glottocode)

df_for_dottree <- tree_df %>% 
  column_to_rownames("glottocode") %>% 
  as.matrix()

png(filename = "output/enock_tree_plot.png", units = "cm", height = 50, width = 30, res = 400)

phytools::phylo.heatmap(tree = ladderize(tree), X = df_for_dottree, colors = c("#482878FF","#FDE725FF"), fsize = 0.3, ftype = "off", lwd = 2, pts = 0) 

cladelabels(text="Oceanic",node =   getMRCA(tree, tip = c("rotu1241", "samo1305")), offset = 0.00000000003)

x <- dev.off()