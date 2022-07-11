source("requirements.R")
#This script was written by Hedvig Skirgård, with assistance from caper-maintainer David Orme and phytools-maintainer Liam Revell.

OUTPUTDIR <- file.path("output", "phylosig")
if (!dir.exists(OUTPUTDIR)) { dir.create(OUTPUTDIR) }	

#reading in global tree. This tree has been processed already by spatiophylogenetic_modelling/processing/pruning_jagertree.R
tree <- read.tree("output/spatiophylogenetic_modelling/processed_data/EDGE_pruned_tree.tree")
tree_tips <- tree$tip.label %>% 
  as.data.frame() %>% 
  dplyr::rename(Language_ID = ".")

GB_wide <- tree_tips %>% #merging the GB dataset to the tree tip df to ensure same ordering
  inner_join(read_tsv("output/GB_wide/GB_wide_imputed_binarized.tsv", show_col_types = FALSE), by = "Language_ID")

#table with full names and abbreviations of features
GB_id_desc <- read_tsv("output/GB_wide/parameters_binary.tsv", show_col_types = FALSE) %>% 
  dplyr::select(Feature_ID = ID, Grambank_ID_desc)

# Build the comparative dataset for the phylo.d function
df_comp <- comparative.data(tree, GB_wide, names.col=Language_ID)
features <- colnames(df_comp$data)

# Create the rows all at once, to avoid rbind (real performance hit in larger examples) although it does mean having to loop over indices not names below.

#make an empty tibble to populate in the for-loop
nfeature <- length(features)
result_df <- tibble(Feature_ID = character(nfeature), 
                    `D-estimate` = numeric(length = nfeature), 
                    Pval1 = numeric(length = nfeature), 
                    Pval0 = numeric(length = nfeature))

cat("Calculating the phylogenetic signal (D-estimate) of",nfeature, "features. Current progress:.\n")

pb = txtProgressBar(min = 0, max = length(seq_along(features)), initial = 0, style = 3, width = 50) 

#for loop for running phylo.d over each feature given the global tree
for (idx in seq_along(features)) {
  feature <- features[idx]
  #feature <- features[103]
  setTxtProgressBar(pb, idx)
  output <- eval(substitute(phylo.d(data = df_comp, binvar = this_feature),
                            list(this_feature=as.name(feature))))
  
  result_df[idx, 1] <- feature
  result_df[idx, 2] <- output$DEstimate
  result_df[idx, 3] <- output$Pval1
  result_df[idx, 4] <- output$Pval0 

}

close(pb)

result_df_with_desc <- result_df %>%  
  left_join(GB_id_desc, by = "Feature_ID") 

result_df_with_desc %>% 
  write_tsv("output/phylosig/pylosig_table.tsv")
#result_df_with_desc <- read_tsv("phylosig/pylosig_table.tsv")  

#filtering to only features that are not statistically significantly different from 0
result_df_with_desc_for_plot<- result_df_with_desc %>% 
  arrange(`D-estimate`) %>% 
  mutate(fill = ifelse(Pval1>= 0.05, "#593d9cff", "grey")) %>% 
  mutate(color = ifelse(Pval0 >= 0.05, "#f68f46ff", "grey")) 

#ordering the col that is to become the y axis in the plot
result_df_with_desc_for_plot$Grambank_ID_desc <- fct_reorder(result_df_with_desc_for_plot$Grambank_ID_desc, desc(result_df_with_desc_for_plot$`D-estimate`))

#making scatterplot of the features that have a D-estimate that is not statistically different from 0
result_df_with_desc_for_plot %>% 
  ggplot() +
  geom_point(mapping = aes(x = Grambank_ID_desc, y = `D-estimate`, 
                           ), color = result_df_with_desc_for_plot$color, 
             fill = result_df_with_desc_for_plot$fill,
             shape = 21, stroke = 1.7, size = 3
                           ) +
  coord_flip() +
  theme_classic() +
  theme(legend.title = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none", 
        axis.title.x = element_text(size = 18),
        axis.text = element_text(size = 14)) 

ggsave("output/phylosig/plot_phylo_signal_pre_feature.tiff", height =22, width = 10)
ggsave("output/phylosig/plot_phylo_signal_pre_feature.png", height =22, width = 10)

###Tree plots of feature with most phylogenetic signal, and the one with least.

#reading in table with language names for plotting
glottolog_cldf_df <- read_tsv("output/non_GB_datasets/glottolog-cldf_wide_df.tsv",show_col_types = FALSE)

#imputing branch lengths for the visualisation so that the tree is ultrametric
tree_for_plots <- tree %>% 
  ape::compute.brlen()

#renaming tips to names instead of glottocodes
tree_for_plots$tip.label <- tree_for_plots$tip.label %>% 
  as.data.frame() %>% 
  dplyr::rename(Language_ID = ".") %>% 
  left_join(glottolog_cldf_df, by = "Language_ID") %>% 
  dplyr::select(Name) %>% 
  .[,1]

color_vector <-c("#593d9cff", "#f68f46ff")

##feature with most phylogenetic signal (as estimated by caper::phylo.d())

most_signal_features <- result_df_with_desc%>% 
  filter(Pval0 >= 0.05) %>% 
  arrange(`D-estimate`) %>% 
  top_n(n = 5, wt = -`D-estimate`) %>% 
  dplyr::select(Feature_ID) %>% 
  as.matrix() %>% 
  as.vector()

index <- 0

cat("Generating tree plots of the top 5 features with the most the phylogenetic signal (D-estimate). Current progress:.\n")
pb = txtProgressBar(min = 0, max = length(most_signal_features), initial = 0, style = 3, width = 50) 

for(feature in most_signal_features) {

index <- index + 1
setTxtProgressBar(pb, index)
  

feature_df <-GB_wide %>% 
  dplyr::select(Language_ID, all_of(feature)) %>% 
  mutate(tip.color = ifelse(.[,2] == 1, color_vector[1],  color_vector[2]))

plot_title <- GB_id_desc %>% 
  filter(Feature_ID == feature) %>% 
  dplyr::select(Grambank_ID_desc) %>% 
  as.matrix() %>% 
  as.vector() 

filename <- paste("output/phylosig/most_signal_", as.character(index), "_" , str_replace(plot_title, " ", "_"), ".tiff", sep = "")
filename_png <- paste("output/phylosig/most_signal_", as.character(index), "_" , str_replace(plot_title, " ", "_"), ".png", sep = "")

x <- feature_df[,2]

#running the contrasting algorithm reconstruction. Note: for the analysis we are using the tree with the original branch lengths even if we're visualizing using the imputed branch lengths.
asr_most_signal<- ape::ace(x = x, phy = tree,method = "pic")

tiff(file = filename, width = 15.27, height = 15.69, units = "in", res = 600)

plot.phylo(ladderize(tree_for_plots  , right = F), 
           col="grey", 
           tip.color = feature_df$tip.color, 
           type = "fan", 
           cex = 0.25,
           label.offset = 0.02)

lastPP<-get("last_plot.phylo",env=.PlotPhyloEnv)
ss<-unique(x) %>% sort(decreasing = T)
par(fg="black")
colors<-setNames(color_vector[1:length(ss)],ss)
add.simmap.legend(colors=colors,
                  vertical=T,
                  x=--0.7,
                  y=-1,
                  prompt=F)

nodelabels(node=1:tree_for_plots$Nnode+Ntip(tree_for_plots),
           pie=asr_most_signal$ace,
           piecol=color_vector, cex = 0.3)

x <- dev.off()


png(file = filename_png, width = 15.27, height = 15.69, units = "in", res = 600)

plot.phylo(ladderize(tree_for_plots  , right = F), 
           col="grey", 
           tip.color = feature_df$tip.color, 
           type = "fan", 
           cex = 0.25,
           label.offset = 0.02)

lastPP<-get("last_plot.phylo",env=.PlotPhyloEnv)
ss<-unique(x) %>% sort(decreasing = T)
par(fg="black")
colors<-setNames(color_vector[1:length(ss)],ss)
add.simmap.legend(colors=colors,
                  vertical=T,
                  x=--0.7,
                  y=-1,
                  prompt=F)

nodelabels(node=1:tree_for_plots$Nnode+Ntip(tree_for_plots),
           pie=asr_most_signal$ace,
           piecol=color_vector, cex = 0.3)

x <- dev.off()


}
close(pb)
