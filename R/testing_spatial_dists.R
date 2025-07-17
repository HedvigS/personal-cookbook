library(tidyverse)
library(fields)
library(reshape2)



coords <- data.frame(
  Glottocode = c("fiji1242", "bisl1239", "samo1305"), 
  Longitude = c(177.772, 166.890, -171.830 ),
    Latitude = c(-17.8148, -15.4000, -13.9200)) %>% 
  column_to_rownames("Glottocode") %>% 
  as.matrix() 

rdist.earth_dists <-   fields::rdist.earth(x1 = coords, x2 = coords, miles = FALSE)

rdist.earth_dists_long <- rdist.earth_dists %>% 
  reshape2::melt() %>% 
  filter(Var1 == "fiji1242" & Var2 == "bisl1239"|
           Var1 == "fiji1242" & Var2 == "samo1305") %>% 
  arrange(Var2)

percent <- paste0(round(rdist.earth_dists_long[1,3]  / rdist.earth_dists_long[2,3] * 100, digits = 2), "%")

cat(paste0("fields::rdist.earth thinks that the distance Fijian <-> Bislama is ", percent, " of the distance Fijian <-> Samoan" ))

dist_dists <- dist(coords) %>% 
  as.matrix() %>% 
  reshape2::melt() %>% 
  filter(Var1 == "fiji1242" & Var2 == "bisl1239"|
           Var1 == "fiji1242" & Var2 == "samo1305") %>% 
  arrange(Var2)

percent <- paste0(round(dist_dists[1,3]  / dist_dists[2,3] * 100, digits = 2), "%")

cat(paste0("stats:dist thinks that the distance Fijian <-> Bislama is ", percent, " of the distance Fijian <-> Samoan" ))


source("varcov_spatial.R")


#letting varcov spatial do it's own dists
spatial_covar_mat  <- varcov.spatial(coords = coords, cov.pars = c(1, 4.15), kappa = 5)$varcov

typical_variance_spatial = exp(mean(log(diag(spatial_covar_mat))))
spatial_cov_std = spatial_covar_mat / typical_variance_spatial
spatial_prec_mat = solve(spatial_cov_std)
dimnames(spatial_prec_mat) = list(rownames(coords), rownames(coords))

spatial_prec_mat <- spatial_prec_mat %>% 
  reshape2::melt() %>%
  filter(Var1 == "fiji1242" & Var2 == "bisl1239"|
                                  Var1 == "fiji1242" & Var2 == "samo1305") %>% 
  arrange(Var2)

n <- spatial_prec_mat[1,3]  /spatial_prec_mat[2,3]

percent <- paste0(n * 100, "%")


#giving varcov spatial dists

rdist.earth_dists <-   fields::rdist.earth(x1 = coords, x2 = coords, miles = FALSE)

rdist.earth_dists[upper.tri(rdist.earth_dists, diag = TRUE)] <- NA

dists_vector <- as.vector(rdist.earth_dists) %>% na.omit()
  
spatial_covar_mat  <- varcov.spatial(dists.lowertri = dists_vector, cov.pars = c(1, 4.15), kappa = 5)$varcov

typical_variance_spatial = exp(mean(log(diag(spatial_covar_mat))))
spatial_cov_std = spatial_covar_mat / typical_variance_spatial
spatial_prec_mat = solve(spatial_cov_std)
dimnames(spatial_prec_mat) = list(rownames(coords), rownames(coords))

spatial_prec_mat <- spatial_prec_mat %>% 
  reshape2::melt() %>%
  filter(Var1 == "fiji1242" & Var2 == "bisl1239"|
           Var1 == "fiji1242" & Var2 == "samo1305") %>% 
  arrange(Var2) 

n <- spatial_prec_mat[1,3]  /spatial_prec_mat[2,3]
