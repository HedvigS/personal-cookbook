source("requirements.R")
p_load(ggridges)

#dist_matrix <- GB_dist
#group_df <- Language_meta_data
#group <- "AUTOTYP_area"
#title <- group
#fn <- file.path(OUTPUTDIR, "ridgeplot_AUTOTYP_area.png")

###FUNCTION FOR RIDGEPLOTS
dists_ridgeplot <- function(dist_matrix, group_df, group, title, fn = "") {
    
  #make diagonal and upper triangle missing data
  dist_matrix_asym <- dist_matrix
  dist_matrix_asym[upper.tri(dist_matrix_asym, diag = T)] <- NA
  
  #making grouping dfs
    left <- group_df %>% 
      dplyr::select(Var1 = Language_ID, group_var1 = all_of(group))
    
    right <- group_df %>% 
      dplyr::select(Var2 = Language_ID, group_var2 = all_of(group))
  
    dist_list_asym <- dist_matrix_asym  %>% 
      reshape2::melt()  %>% 
      filter(!is.na(value)) 
    
    #make dataframe for the global distribution ridge
    dist_list_gobal <-dist_list_asym %>% 
      dplyr::select(value) %>% 
      mutate(group_var1 = "global", 
             group_var2 = "global")
    
    dist_list_ridgeplot <-dist_list_asym %>% 
      left_join(left, by = "Var1") %>% 
      left_join(right, by = "Var2") %>% 
      mutate(same = ifelse(group_var1 == group_var2,"same", "diff")) %>% 
      filter(same == "same") %>% 
      full_join(dist_list_gobal, by = c("value", "group_var1", "group_var2")) %>% 
      group_by(group_var1) %>% 
      mutate(mean_value = mean(value)) 
    
    dist_list_ridgeplot$group_var1 <- fct_reorder(dist_list_ridgeplot$group_var1, dist_list_ridgeplot$mean_value)
    
    mean_labels <- dist_list_ridgeplot %>% 
      distinct(group_var1, mean_value)
    
p <-  dist_list_ridgeplot%>% 
      ggplot() +
      geom_density_ridges(aes(x = value, y =group_var1, fill = group_var1), 
                          quantile_lines = T, 
                          quantile_fun = mean, 
                          jittered_points = TRUE, 
                          point_size = 2, 
                          point_shape = 21  ,  
                          position = position_points_jitter(height = 0))  +
      geom_label(data = mean_labels, aes(x = mean_value, y = group_var1,
                                         label = round(mean_value, 2)), size = 8, nudge_x = 0.01, nudge_y = 0.2, alpha = 0.7, label.padding = unit(0.1, "lines")) +
      theme_classic() +
      theme(axis.title = element_blank(),
            text = element_text(size = 20),
            legend.position = "None") +
      ggtitle(label = title)

if(fn != "") {
  png(filename = fn,  units = "in", width = 8, height = 9, res = 300)
  plot(p)
  x <- dev.off()
} else{
  plot(p)
}
  
}

##FUNCTION FOR HEATMAP
dists_heatmap <- function(dist_matrix, group_df, group, title, fn = "") {

  dist_sym <- dist_matrix
  diag(dist_sym) <- NA
  
  
  left <- group_df %>% 
    dplyr::select(Var1 = Language_ID, group_var1 = all_of(group))
  
  right <- group_df %>% 
    dplyr::select(Var2 = Language_ID, group_var2 = all_of(group))
  
  dist_sym_grouped <- dist_sym %>% 
    reshape2::melt()  %>% 
    filter(!is.na(value)) %>% 
    left_join(left, by = "Var1") %>% 
    left_join(right, by = "Var2")  %>%
    group_by(group_var1, group_var2) %>% 
    summarise(mean = mean(value, na.rm = T), .groups = "drop") %>% 
    ungroup() %>% 
    reshape2::dcast(group_var1 ~ group_var2, value.var = "mean") %>% 
    column_to_rownames("group_var1") %>% 
    as.matrix() 
  
  n_cols <- right$group_var2 %>% unique() %>% length()
  
  
  if(fn != "") {
    png(filename = fn, width = 2000, height = 2000, units = "px", pointsize = 34)}
    dist_sym_grouped %>%
      gplots::heatmap.2(  key = F, symm = T,
                          dendrogram = "none",
                          revC = T,
                          trace = "none",
                          margin=c(15,15),
                          cellnote = round(dist_sym_grouped, 2),
                          col=viridis(n = n_cols, direction = -1))
    if(fn != "") {
      x <- dev.off() }

  }
