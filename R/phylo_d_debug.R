library(ape)
library(caper)
library(tidyverse)
library(phytools)

#set.seed(43)

#testing it out with a tree with 28 tips

tree <- ape::read.tree(text = "((((((((North_Marquesan:0.03703703704,Mangareva:0.03703703704):0.03703703704,Rapanui:0.07407407407):0.2592592593,((((Hawaiian:0.1111111111,Maori:0.1111111111,(Austral:0.03703703704,Tahitian:0.03703703704):0.07407407407):0.03703703704,Māngarongaro:0.1481481481):0.03703703704,Southern_Cook_Island_Maori:0.1851851852):0.03703703704,Tuamotuan:0.2222222222):0.1111111111):0.07407407407,(Tuvalu:0.03703703704,Tokelau:0.03703703704):0.3703703704):0.2222222222,(((Nukuoro:0.03703703704,Kapingamarangi:0.03703703704):0.1111111111,((Luangiua:0.03703703704,Sikaiana:0.03703703704):0.03703703704,Takuu:0.07407407407):0.07407407407):0.03703703704,Tikopia:0.1851851852):0.4444444444):0.2962962963,((East_Uvean:0.03703703704,Niuean:0.03703703704):0.03703703704,East_Futuna:0.07407407407):0.8518518519,((Vaeakau-Taumako:0.03703703704,Mele-Fila:0.03703703704):0.07407407407,(Emae:0.03703703704,West_Uvean:0.03703703704):0.07407407407):0.8148148148,Rennell-Bellona:0.9259259259):0.03703703704,Anuta:0.962962963):0.03703703704,Tonga_-Tonga_Islands:1);")

is.ultrametric(tree)

df <- tree$tip.label %>%
  as.data.frame() %>%
  rename(tip.label = ".") %>% 
  mutate(outlier_end = c(1, 1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 0), 
         clustered= c(0, 0, 0, 0, 0, 0, 0, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
         
         outlier_middle = c(1, 1, 1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 1)
         )

df$outlier_random <- sample(df$outlier_end)
df$cluster_random <- sample(df$clustered)

df$outlier_end <- abs(df$outlier_end-1)
df$outlier_random <- abs(df$outlier_random -1)
df$outlier_middle <- abs(df$outlier_middle-1)

# Build the comparative dataset once

ds <- comparative.data(tree, df, names.col=tip.label)

vars <- colnames(ds$data)

# Create the rows all at once, to avoid rbind (real performance hit in

# larger examples) although it does mean having to loop over indices not names below.

result_df <- data.frame(Feature = character(), Destimate = numeric(),
                    Pval0 = numeric(), Pval1 = numeric(), ones = as.numeric(), zeroes = as.numeric(), tree_tips = as.numeric())

iterations <- 1:8

for(iter in iterations){
cat("I'm on iter", iter, ".\n")
  
for (idx in seq_along(vars)) {
  
  var <- vars[idx]
#  var <- vars[2]
  cat("I'm on iter", iter, "and var", var, ".\n")
  
  value_table <- df %>% 
    dplyr::select(all_of(var)) %>% 
    table() %>% 
    t()
  
    zeroes = value_table[1,"0"]
    ones = value_table[1,"1"]

  output <- eval(substitute(phylo.d(data = ds, binvar = this_var), list(this_var=as.name(var))))
  
  result_df_spec <- data.frame(Feature =   paste0(var, "_", iter), 
                               Destimate =  output$DEstimate,
                               Pval0 = output$Pval0, 
                               Pval1 = output$Pval1, 
                               tree_tips = Ntip(tree), 
                               ones = ones,
                               zeroes = zeroes
                               )
  
  result_df <- result_df  %>% full_join(result_df_spec, by = c("Feature", "Destimate", "Pval1", "Pval0","tree_tips", "ones", "zeroes"))
  
}
}

###testing with a tree with 110 tips

#down here is a newick string for a tree with 110 tips, where there is one tip that splits of directly from the root with no nodes in between (i.e. similar to Tongan in our example above).
tree <- read.tree(text = "(t78:1,(((t13:0.2018348624,((t66:0.04587155963,(((t82:0.009174311927,t6:0.009174311927):0.009174311927,t105:0.01834862385):0.01834862385,(t92:0.009174311927,t33:0.009174311927):0.02752293578):0.009174311927):0.1467889908,(((((t25:0.01834862385,(t67:0.009174311927,t88:0.009174311927):0.009174311927):0.02752293578,(t3:0.01834862385,(t52:0.009174311927,t103:0.009174311927):0.009174311927):0.02752293578):0.009174311927,t43:0.05504587156):0.02752293578,((t4:0.009174311927,t101:0.009174311927):0.009174311927,t29:0.01834862385):0.06422018349):0.05504587156,((t18:0.02752293578,(t26:0.01834862385,(t104:0.009174311927,t61:0.009174311927):0.009174311927):0.009174311927):0.01834862385,(t39:0.009174311927,t90:0.009174311927):0.03669724771):0.09174311927):0.05504587156):0.009174311927):0.3486238532,((((t11:0.009174311927,t107:0.009174311927):0.01834862385,(t108:0.009174311927,t36:0.009174311927):0.01834862385):0.07339449541,((t48:0.01834862385,(t51:0.009174311927,t87:0.009174311927):0.009174311927):0.04587155963,((t93:0.009174311927,t42:0.009174311927):0.02752293578,((t27:0.009174311927,t109:0.009174311927):0.009174311927,t16:0.01834862385):0.01834862385):0.02752293578):0.03669724771):0.2385321101,(((t56:0.009174311927,t5:0.009174311927):0.009174311927,t81:0.01834862385):0.2110091743,(((t55:0.02752293578,((t21:0.009174311927,t31:0.009174311927):0.009174311927,t46:0.01834862385):0.009174311927):0.1376146789,(((t49:0.01834862385,(t10:0.009174311927,t73:0.009174311927):0.009174311927):0.03669724771,((t9:0.01834862385,(t62:0.009174311927,t2:0.009174311927):0.009174311927):0.009174311927,t89:0.02752293578):0.02752293578):0.07339449541,(((t64:0.01834862385,(t84:0.009174311927,t75:0.009174311927):0.009174311927):0.03669724771,((t80:0.01834862385,(t14:0.009174311927,t74:0.009174311927):0.009174311927):0.009174311927,t100:0.02752293578):0.02752293578):0.009174311927,t98:0.06422018349):0.06422018349):0.03669724771):0.03669724771,(t106:0.02752293578,((t32:0.009174311927,t72:0.009174311927):0.009174311927,t44:0.01834862385):0.009174311927):0.1743119266):0.02752293578):0.1100917431):0.2110091743):0.4403669725,((((t40:0.009174311927,t76:0.009174311927):0.009174311927,t38:0.01834862385):0.2293577982,((((((t85:0.009174311927,t95:0.009174311927):0.02752293578,((t8:0.009174311927,t47:0.009174311927):0.009174311927,t60:0.01834862385):0.01834862385):0.07339449541,((t41:0.03669724771,((t99:0.009174311927,t19:0.009174311927):0.01834862385,(t20:0.009174311927,t68:0.009174311927):0.01834862385):0.009174311927):0.02752293578,(t63:0.01834862385,(t69:0.009174311927,t70:0.009174311927):0.009174311927):0.04587155963):0.04587155963):0.03669724771,((t102:0.009174311927,t97:0.009174311927):0.01834862385,(t58:0.009174311927,t34:0.009174311927):0.01834862385):0.119266055):0.02752293578,(t22:0.01834862385,(t30:0.009174311927,t86:0.009174311927):0.009174311927):0.1559633028):0.04587155963,((t94:0.009174311927,t7:0.009174311927):0.02752293578,((t17:0.009174311927,t35:0.009174311927):0.009174311927,t15:0.01834862385):0.01834862385):0.1834862385):0.02752293578):0.1834862385,(((t91:0.009174311927,t77:0.009174311927):0.1559633028,(t54:0.1467889908,(((t24:0.01834862385,(t28:0.009174311927,t71:0.009174311927):0.009174311927):0.009174311927,t65:0.02752293578):0.1100917431,((t23:0.009174311927,t37:0.009174311927):0.09174311927,((((t83:0.009174311927,t57:0.009174311927):0.01834862385,(t79:0.009174311927,t45:0.009174311927):0.01834862385):0.009174311927,t59:0.03669724771):0.04587155963,(((t96:0.01834862385,(t110:0.009174311927,t53:0.009174311927):0.009174311927):0.009174311927,t50:0.02752293578):0.009174311927,t1:0.03669724771):0.04587155963):0.01834862385):0.03669724771):0.009174311927):0.01834862385):0.009174311927,t12:0.1743119266):0.2568807339):0.5596330275):0.009174311927);")

plot(tree)

df <- tree$tip.label %>%
  as.data.frame() %>%
  rename(tip.label = ".") %>%
  mutate(outlier_end = c(0, rep(x = 1, times = 109)), 
         clustered= c(rep(x = 1, times = 62), rep(x = 0, times = 48)),
         
         outlier_middle = c(rep(x = 1, times = 50), 0, rep(x = 1, times = 59))
  )

df$outlier_random <- sample(df$outlier_end)
df$cluster_random <- sample(df$clustered)

ds <- comparative.data(tree, df, names.col=tip.label)

vars <- colnames(ds$data)

for(iter in iterations){
  cat("I'm on iter", iter, ".\n")
  
  for (idx in seq_along(vars)) {
    
    var <- vars[idx]
    #  var <- vars[2]
    cat("I'm on iter", iter, "and var", var, ".\n")
    
    value_table <- df %>% 
      dplyr::select(all_of(var)) %>% 
      table() %>% 
      t()
    
    zeroes = value_table[1,"0"]
    ones = value_table[1,"1"]
    
    output <- eval(substitute(phylo.d(data = ds, binvar = this_var), list(this_var=as.name(var))))
    
    result_df_spec <- data.frame(Feature =   paste0(var, "_", iter), 
                                 Destimate =  output$DEstimate,
                                 Pval1 = output$Pval1, 
                                 Pval0 = output$Pval0, 
                                 tree_tips = Ntip(tree), 
                                 ones = ones,
                                 zeroes = zeroes
    )
    
    result_df <- result_df  %>% full_join(result_df_spec, by = c("Feature", "Destimate", "Pval1", "Pval0","tree_tips", "ones", "zeroes"))
    
  }
}


#GB116_gray_et_al_2009_posterior_tree_pruned_98.txt

GB_df <- read_tsv("example_data/feature_98_df.tsv", show_col_types = F) %>% 
  dplyr::select(tip.label, Value) %>% 
  as.data.frame() 

tree <- read.tree("example_data/tree_98.txt")

ds <- comparative.data(tree, GB_df, names.col=tip.label)

destimate_vec <- c()

for(iter in 1:40){
  cat("I'm on iter", iter, ".\n")
output <- caper::phylo.d(ds, binvar = Value, permut = 20000)

destimate_vec <- c(destimate_vec, output$DEstimate[[1]])
}


destimate_vec

matrix_for_dottree <- GB_df %>% 
  column_to_rownames("tip.label") %>% 
  as.matrix()

vec <- ds$data 
names(vec) <- rownames(ds$data)

plotTree(tree,fsize=0.5)
