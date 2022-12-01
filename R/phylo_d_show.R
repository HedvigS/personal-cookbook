library(ape)
library(caper)
library(tidyverse)
library(phytools)
library(beepr)


set.seed(67)

#read in tree
tree <- read.tree(text = "(((((1:0.619136,2:0.619136):0.617801,(3:0.61546,4:0.61546):0.621477):0.550836,(5:1.415766,6:1.415766):0.372007):1.104319,(7:2.271433,8:2.271433):0.620659):0.71217,(((((9:2.447733,(10:1.418763,(11:1.4065,(((((12:0.612769,(13:0.482425,14:0.482425):0.130344):0.524653,((15:0.601128,16:0.601128):0.22195,17:0.823078):0.314344):0.13666,(((18:1.000006,19:1.000006):0.238626,(((20:0.634936,21:0.634936):0.491156,22:1.126092):0.023914,(((23:0.76698,(24:0.709445,(25:0.31043,26:0.31043):0.399015):0.057535):0.111102,27:0.878082):0.123671,(28:0.75429,29:0.75429):0.247463):0.148253):0.088626):0.013922,(((30:0.226455,31:0.226455):0.273728,(32:0.382181,(33:0.245055,34:0.245055):0.137126):0.118002):0.1661,35:0.666283):0.586271):0.021528):0.018484,36:1.292566):0.031097,(37:0.495976,38:0.495976):0.827687):0.082837):0.012263):1.02897):0.559769,39:3.007502):0.495538,(((((40:2.010157,41:2.010157):0.77792,42:2.788077):0.465055,(43:2.895472,(44:1.331195,45:1.331195):1.564277):0.35766):0.169371,(((((46:1.205182,47:1.205182):0.626226,48:1.831408):0.473659,(((49:1.703353,((50:0.370682,51:0.370682):1.109659,((52:1.060912,53:1.060912):0.17655,(54:0.911239,55:0.911239):0.326223):0.242879):0.223012):0.385318,(56:1.896917,(57:1.785681,58:1.785681):0.111236):0.191754):0.144893,((59:1.344783,60:1.344783):0.546109,(61:0.763744,62:0.763744):1.127148):0.342672):0.071503):0.116825,(63:1.697937,(64:0.803538,65:0.803538):0.894399):0.723955):0.521862,((66:0.537212,67:0.537212):1.7406,(((68:0.34127,69:0.34127):0.284045,70:0.625315):0.984165,71:1.60948):0.668332):0.665942):0.478749):0.059618,(72:2.200014,((73:1.453894,(74:1.182255,75:1.182255):0.271639):0.042074,(76:1.475088,((((77:0.188916,(78:0.099663,79:0.099663):0.089253):0.047338,80:0.236254):0.033791,(81:0.227562,82:0.227562):0.042483):0.088213,83:0.358258):1.11683):0.02088):0.704046):1.282107):0.020919):0.065395,((((84:1.643468,85:1.643468):1.356547,86:3.000015):0.449325,((87:2.176984,(88:2.011687,89:2.011687):0.165297):0.345023,(90:1.381611,(91:1.285409,92:1.285409):0.096202):1.140396):0.927333):0.114184,((93:3.341017,((((94:0.998906,((95:0.722783,96:0.722783):0.125625,97:0.848408):0.150498):0.407625,98:1.406531):0.301957,99:1.708488):0.508148,((100:0.745818,101:0.745818):0.973238,(102:1.144107,103:1.144107):0.574949):0.49758):1.124381):0.128845,((((104:3.081157,(105:2.738232,(106:1.848817,107:1.848817):0.889415):0.342925):0.134203,((108:1.279143,(109:0.914448,(110:0.789873,111:0.789873):0.124575):0.364695):1.835374,((112:1.927951,((113:1.043502,114:1.043502):0.231512,115:1.275014):0.652937):0.707925,((116:0.925694,117:0.925694):0.317767,(118:0.674419,119:0.674419):0.569042):1.392415):0.478641):0.100843):0.145983,((120:2.295601,(121:1.895638,(122:1.247308,123:1.247308):0.64833):0.399963):0.689518,(124:2.928258,((125:1.975084,(126:1.573942,127:1.573942):0.401142):0.611467,(128:2.006048,129:2.006048):0.580503):0.341707):0.056861):0.376224):0.032782,130:3.394125):0.075737):0.093662):0.004911):0.031583,((131:3.505884,((132:1.881834,133:1.881834):0.482313,134:2.364147):1.141737):0.007811,(((((135:2.823391,136:2.823391):0.42146,(137:2.651968,138:2.651968):0.592883):0.074615,139:3.319466):0.022924,((140:1.092497,(141:0.688247,142:0.688247):0.40425):0.448374,143:1.540871):1.801519):0.139419,((144:2.729987,(145:2.367385,146:2.367385):0.362602):0.299223,(((((147:0.766506,148:0.766506):0.81334,(149:1.205284,150:1.205284):0.374562):0.759979,(151:0.869143,152:0.869143):1.470682):0.306906,153:2.646731):0.104287,(154:1.932608,155:1.932608):0.81841):0.278192):0.452599):0.031886):0.086323):0.004244);")

#specifying sets of tips that represent specific patterns and making a dataframe that matches to the phylogeny
tips_singleton_outlier <- c(8)
tips_singleton_middle<- c(112)
tips_sisters_a <-  c(8,7)
tips_sisters_b <-  c(96, 95)
tips_sisters_c <-  c(142, 141)
tips_triplets_a <- c(144, 145, 146)
tips_triplets_b <- c(142, 141, 140)
tips_triplets_c <- c(79, 78, 77)
tips_quadruplets_a <- c(150, 149, 148, 147)
tips_quadruplets_b <- c(1, 2, 3, 4)
tips_quadruplets_c <- c(119, 118, 117, 116)
tips_cluster <- c(39, 9, 10, 11, 38, 36, 37, 17, 16, 15, 12, 14, 13, 35, 31, 30, 32, 34, 33, 19, 18, 22, 21, 20, 29, 28 ,27, 23, 24, 26, 25)

df <- tree$tip.label %>%
  as.data.frame() %>%
  rename(tip.label = ".") %>% 
  mutate(singleton_outlier = ifelse(tip.label %in% tips_singleton_outlier, 0, 1),  
        singleton_middle = ifelse(tip.label %in% tips_singleton_middle, 0, 1),
        sisters_a = ifelse(tip.label %in% tips_sisters_a, 0, 1),  
        sisters_b = ifelse(tip.label %in% tips_sisters_b, 0, 1), 
        sisters_c = ifelse(tip.label %in% tips_sisters_c, 0, 1),  
        triplets_a = ifelse(tip.label %in% tips_triplets_a, 0, 1),  
        triplets_b = ifelse(tip.label %in% tips_triplets_b, 0, 1), 
        triplets_c = ifelse(tip.label %in% tips_triplets_c, 0, 1),
        quadruplets_a = ifelse(tip.label %in% tips_quadruplets_a, 0, 1),  
        quadruplets_b = ifelse(tip.label %in% tips_quadruplets_b, 0, 1), 
        quadruplets_c = ifelse(tip.label %in% tips_quadruplets_c, 0, 1),
        cluster = ifelse(tip.label %in% tips_cluster, 0, 1)
        )  %>% 
  column_to_rownames("tip.label")

# making random versions of singleton, sister, triplet, quad and cluster distributions
df$singleton_random <- sample(df$singleton_outlier)
df$two_random <- sample(df$sisters_a)
df$three_random <- sample(df$triplets_a)
df$four_random <- sample(df$quadruplets_a)
df$cluster_random <- sample(df$cluster)

df <- df %>%
  dplyr::select("singleton_outlier" ,"singleton_middle" ,  "singleton_random", "sisters_a"    ,     "sisters_b",         "sisters_c" ,    "two_random",     
               "triplets_a"      ,  "triplets_b"   ,    "triplets_c"   ,      "three_random" , "quadruplets_a"   ,  "quadruplets_b"   , "quadruplets_c"  ,  "four_random" , "cluster"   ,             
              "cluster_random"  ) 

#make a tree viz to show the groupings
png(filename = "output/phylo_d_heatmap_tree.png", width = 10, height = 10, res = 300, units = "in")
phytools::phylo.heatmap(tree, df,legend = F, tip.labels = F, outline = F, pts = F, offset = 0,  show.tip.label = F, dot.legend= F, Ntip = F, colors = c("#83E1E5", "#F7FFDA"))
x <- dev.off()

for_dot_tree <- df %>% 
  dplyr::select(singleton_outlier) %>% 
  mutate(singleton_outlier = abs(singleton_outlier-1))

png(filename = "output/phylo_d_singleton_tree.png", width = 10, height = 10, res = 300, units = "in")
phytools::dotTree(tree = tree, x = for_dot_tree, tip.labels = F, standardize = T,	

                  colors = c("#83E1E5", "#F7FFDA"))
x <- dev.off()

#setting up looping over features
vars <- colnames(df)

#making comp ojbect
df <- df %>% 
  rownames_to_column("tip.label")

comp_data <- comparative.data(tree, df,names.col = "tip.label")

#running phylo d

#empy df to bind to
result_df <- data.frame(Feature = character(), 
                        Destimate = numeric(),
                        Pval0 = numeric(), 
                        Pval1 = numeric(), 
                        ones = as.numeric(), 
                        zeroes = as.numeric(), 
                        tree_tips = as.numeric(),
                        permut = as.numeric())

#trying it out with default permut
permut_vec <- c(1000, 20000, 30000)

iterations <- 1:8

for(permut in permut_vec){

  for(iter in iterations){
  
  for (var in vars) {
      #  var <- colnames(df)[1]
    cat("I'm on iter", iter, "and var", var, "with permut =", permut,".\n")
    
    value_table <- df %>% 
      dplyr::select(all_of(var)) %>% 
      table() %>% 
      t()
    
    zeroes = value_table[1,"0"]
    ones = value_table[1,"1"]
    
    output <- eval(substitute(caper::phylo.d(data = comp_data, binvar = this_var), list(this_var=as.name(var))))
    
    result_df_spec <- data.frame(Feature =   paste0(var, "_", iter), 
                                 Destimate =  output$DEstimate,
                                 Pval0 = output$Pval0, 
                                 Pval1 = output$Pval1, 
                                 tree_tips = Ntip(tree), 
                                 ones = ones,
                                 zeroes = zeroes, 
                                 permut = permut
    )
    
    result_df <- result_df  %>% full_join(result_df_spec, by = c("Feature", "Destimate", "Pval1", "Pval0","tree_tips", "ones", "zeroes", "permut"))
    
  }
}
}
beep()

result_df %>%
#  dplyr::filter(zeroes < 6) %>% 
  ggplot() +
  geom_point(mapping = aes(x = zeroes, y = Destimate, color = permut), size = 3, shape = 23) +
  facet_grid(~permut) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500)) +
  theme(legend.position = 0, 
        plot.background = element_rect(color = "white")) +
  ylab("D-estimate") +
  xlab("Number of tips with different value")

ggsave(filename = "output/phylo_d_permut_plot.png", width = 8, height = 5, units = "in")

result_df %>% 
  filter(permut == 30000) %>% 
  group_by(zeroes) %>% 
  summarise(var = var(Destimate)) %>% 
  ggplot() +
  geom_point(aes(x = zeroes, y = var)) +
  geom_text(aes(x = zeroes, y = var, label = round(var, 2)), angle = 70, nudge_y = 30)+
  theme_minimal() +
  xlab("Number of tips with different value") +
theme(legend.position = 0, 
      plot.background = element_rect(color = "white"))

ggsave(filename = "output/phylo_d_scatterplot_var.png", width = 8, height = 5, units = "in")
