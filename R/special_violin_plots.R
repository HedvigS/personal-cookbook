library(tidyverse, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library(FamilyRank)
library(ggpubr)

dark_colors <- c("deeppink4", "coral4", "black", "brown", "darkmagenta", "darkorchid4", "#702d0f", "#0A5231")
light_colors <- c("pink", "lavenderblush", "lavender", "deeppink3", "#e6aa8e", "#855958", "#781c29", "#c98791", "#ebabb5", "#a1747a", "#e6a75a", "#f2b66d", "#efd0f5", "#2B8A5F","#51CF96")

point1_pos <- c(55, 57, 60)
point2_pos <- c(64, 70, 72)
point3_pos <- c(20, 24, 30)

good_plot <- function(){

df <- data.frame(var1 = c(0,0,1, FamilyRank::rbinorm(n = 96, mean1 = 20, mean2 = 60, sd1 = 15, sd2 = 5, prop = 0.7), 100),
                 var2 = c(0,0,1, FamilyRank::rbinorm(n = 96, mean1 = 25, mean2 = 60, sd1 = 15, sd2 = 5, prop = 0.7), 100),
                 var3 = c(0,0,1, FamilyRank::rbinorm(n = 96, mean1 = 18, mean2 = 70, sd1 = 15, sd2 = 5, prop = 0.7), 100),
                 v = rep(x = "a", 100)) %>% 
  mutate(var1 = ifelse(var1 < 0, 0, var1)) %>% 
  mutate(var2 = ifelse(var2 < 0, 0, var2)) %>% 
  mutate(var3 = ifelse(var3 < 0, 0, var3)) %>% 
  mutate(var1 = ifelse(var1 > 100, 100, var1)) %>% 
  mutate(var2 = ifelse(var2 > 100, 100,var2)) %>% 
  mutate(var3 = ifelse(var3 > 100, 100, var3)) 
  

p <- suppressWarnings(
  ggplot(data = df) +
    geom_point(aes(x = "a", 
                   y = sample(point1_pos, size = 1, replace = FALSE)), 
             size = 14, 
             shape = 21, 
             fill = sample(dark_colors, size = 1, replace = FALSE),
             stroke = 3,
             alpha = 0.6,
             color = sample(dark_colors, size = 1, replace = FALSE)) +
  geom_point(aes(x = "a", 
                 y = sample(point2_pos, size = 1, replace = FALSE)), 
             size = 20, 
             shape = 21, 
             fill = "deeppink4",
             stroke = 3,
             alpha = 0.6,
             color = sample(dark_colors, size = 1, replace = FALSE))+
  geom_point(aes(x = "a", 
                 y = sample(point3_pos, size = 1, replace = FALSE)), 
             size = 40, 
             shape = 21, 
             fill = "black",
             stroke = 5,
             alpha = 0.6,
             color = sample(dark_colors, size = 1, replace = FALSE))+
    geom_violin(mapping = aes(y = var2, x = v), alpha = 0.3,
              fill = sample(light_colors, size = 1, replace = FALSE), 
              color = sample(light_colors, size = 1, replace = FALSE), linewidth = 2) +
  geom_violin(mapping = aes(y = var1, x = v), 
              fill = sample(light_colors, size = 1, replace = FALSE), 
              color = sample(dark_colors, size = 1, replace = FALSE), linewidth = 3, alpha = 0.4) +
  geom_violin(mapping = aes(y = var3, x = v), 
              fill = sample(light_colors, size = 1, replace = FALSE), 
              color = sample(dark_colors, size = 1, replace = FALSE), linewidth = 1, alpha = 0.2) +
    theme_void()+
  theme(panel.background = element_rect(fill = sample(light_colors, size = 1, replace = FALSE))) +
  ylim(c(0, 100))

)
}

p_all <- ggarrange(
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot()),
    suppressWarnings(good_plot())
)
  
  
suppressWarnings(  ggsave(plot = p_all , filename = "good_plots.png", width = 25, height = 48, dpi = 200) )



