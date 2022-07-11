library(qs)
library(purrr)
library(dplyr)

qs_files <- list.files("output/featurewise_old", full.names = TRUE, pattern = ".qs")

mods <- map(qs_files, qread)

get_waics <- function(mods) {
  waics <- sapply(mods, function(x) x$waic$waic)
  tibble(phy_only = waics[[1]],
         spat_only = waics[[2]],
         autotyp_only = waics[[3]],
         phy_spat = waics[[4]])
}

all_waics <- map_dfr(mods, get_waics)

summ <- all_waics %>%
  rowwise() %>%
  mutate(best_mod = which.min(c_across()))

table(summ$best_mod)
sum(table(summ$best_mod)[c(1, 4)]) / 113
apply(all_waics, 2, mean)
apply(all_waics, 2, median)
apply(apply(all_waics, 1, rank), 1, mean)
