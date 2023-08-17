library(tidyverse)
library(ggplot2)
library(Unicode)
library(viridis)

#script for how to plot geometric unicode shapes in R.

#solution found by Jon Spring: https://stackoverflow.com/questions/52902946/using-unicode-characters-as-shape

#for other unicode character shapes: https://jrgraphix.net/r/Unicode/25A0-25FF

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_text(mapping = aes(color = vs), label = "\u25D6",
            size=10, family = "Arial Unicode MS") +
  geom_text(mapping = aes(color = am), 
            label = "\u25D7",
            size=10, 
            family = "Arial Unicode MS") +  
  theme_bw() +
  viridis::scale_color_viridis(begin = 0, end = 0.95) +
  theme(legend.position = "None")

ggsave("output/half_circle_plot_mtcars.png", height = 5, width = 5)