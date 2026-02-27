library(tidyverse)

dark_colors <- c("deeppink4", "coral4", "black", "brown", "darkmagenta", "darkorchid4", "#702d0f"#, "#0A5231"
                 )
light_colors <- c("pink", "lavenderblush", "lavender", "deeppink3", "#e6aa8e", "#855958", "#781c29", "#c98791", "#ebabb5", "#a1747a", "#e6a75a", "#f2b66d", "#efd0f5"
                  #, "#2B8A5F","#51CF96"
                  )

point1_pos <- c(55, 57, 60)
point2_pos <- c(64, 70, 72)
point3_pos <- c(20, 24, 30)

rbinorm_h <- function(n, mean1, mean2, sd1, sd2, prop) {
  z <- rbinom(n, size = 1, prob = prop)
  rnorm(n,
        mean = ifelse(z == 1, mean1, mean2),
        sd   = ifelse(z == 1, sd1,  sd2))
}

df <- data.frame(var1 = c(0,0,1, rbinorm_h(n = 96, mean1 = 20, mean2 = 60, sd1 = 15, sd2 = 5, prop = 0.7), 100),
                 var2 = c(0,0,1, rbinorm_h(n = 96, mean1 = 25, mean2 = 60, sd1 = 15, sd2 = 5, prop = 0.7), 100),
                 var3 = c(0,0,1, rbinorm_h(n = 96, mean1 = 18, mean2 = 70, sd1 = 15, sd2 = 5, prop = 0.7), 100),
                 v = rep(x = "a", 100)) |> 
  dplyr::mutate(var1 = ifelse(var1 < 0, 0, var1)) |>
  dplyr::mutate(var2 = ifelse(var2 < 0, 0, var2)) |>
  dplyr::mutate(var3 = ifelse(var3 < 0, 0, var3)) |>
  dplyr::mutate(var1 = ifelse(var1 > 100, 100, var1)) |>
  dplyr::mutate(var2 = ifelse(var2 > 100, 100,var2)) |>
  dplyr::mutate(var3 = ifelse(var3 > 100, 100, var3)) 

df_long <- df |>  
  dplyr::select(var1, var2, v) |> 
  reshape2::melt(id.vars = "v")

df_long  |> 
  ggplot() +
  geom_boxplot(aes(y = variable, x = value)) 
ggsave("special_boxplot.png")

df_long |> 
  ggplot() +
  geom_violin(aes(y = variable, x = value))
ggsave("special_violin.png")

df_long |> 
  ggplot() +
  ggridges::geom_density_ridges(aes(x= value, y = variable)) 
ggsave("special_ridge.png")

df_long |> 
  ggplot() +
  ggridges::geom_density_ridges(aes(x= value, y = variable), quantile_lines = T, quantile_fun = mean) 
ggsave("special_ridge_with_mean.png")

df_long |> 
  ggplot() +
  ggridges::geom_density_ridges(aes(x= value, y = variable), quantile_lines = T, quantile_fun = mean, 
                                jittered_points = TRUE,scale = 0.6,
                                position = "raincloud") 
ggsave("special_ridge_with_mean_and_rain.png")


good_plot <- function(df = df){

p <- ggplot(data = df) +
  annotate("point",
           x = "a",
           y = sample(point1_pos, size = 1),
           size = 14,
           shape = 21,
           fill = sample(dark_colors, size = 1),
           stroke = 3,
           alpha = 0.6,
           color = sample(dark_colors, size = 1)) +
  
  annotate("point",
           x = "a",
           y = sample(point2_pos, size = 1),
           size = 20,
           shape = 21,
           fill = "deeppink4",
           stroke = 3,
           alpha = 0.6,
           color = sample(dark_colors, size = 1)) +
  
  annotate("point",
           x = "a",
           y = sample(point3_pos, size = 1),
           size = 40,
           shape = 21,
           fill = "black",
           stroke = 5,
           alpha = 0.6,
           color = sample(dark_colors, size = 1)) +
  
  geom_violin(aes(y = var2, x = v),
              alpha = 0.3,
              fill = sample(light_colors, size = 1),
              color = sample(light_colors, size = 1),
              linewidth = 2) +
  
  geom_violin(aes(y = var1, x = v),
              fill = sample(light_colors, size = 1),
              color = sample(dark_colors, size = 1),
              linewidth = 3,
              alpha = 0.4) +
  
  geom_violin(aes(y = var3, x = v),
              fill = sample(light_colors, size = 1),
              color = sample(dark_colors, size = 1),
              linewidth = 1,
              alpha = 0.2) +
  
  theme_void() +
  theme(panel.background =
          element_rect(fill = sample(light_colors, size = 1))) +
  ylim(c(0, 100))


}

p_all <- ggarrange(
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df),
   good_plot(df = df)
)
  
  ggsave(plot = p_all , filename = "good_plots.png", width = 25, height = 48, dpi = 200) 



