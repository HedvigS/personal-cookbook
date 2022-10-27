library(tidyverse)
library(cluster)
library(reshape2)
library(stringdist)

forms <- read_csv("https://raw.githubusercontent.com/lexibank/abvd/master/cldf/forms.csv", show_col_types = F)

unknown_cognacy <-  forms %>% 
  filter(is.na(Cognacy)) %>% 
  dplyr::select(ID_Var1= ID, Var1 = Form)  %>% 
  distinct()

known_cognacy <- forms %>% 
  filter(!is.na(Cognacy)) %>% 
  dplyr::select(Var2 = Form, Var2_Cognacy = Cognacy)  %>% 
  distinct()
  
#make df to join to in loop
dist_full <- matrix(nrow = 0, ncol = 7) %>% 
  as.data.frame() %>% 
  rename("Var1" = V1, "Var2" = V2, "lv_dist"= V3, "Form_1" = V4, "Cognacy_1" = V5, "Form_2" = V6, "Cognacy_2" = V7) %>% 
  mutate_if(.predicate = is.logical, as.character) %>% 
  mutate(lv_dist = as.numeric(lv_dist))

##
#df to join info on the side of dists df
left <- forms %>%
dplyr::select(Var1 = ID, Form_1 = Form, Cognacy_1 = Cognacy)
right <- forms %>%
dplyr::select(Var2 = ID,Form_2 = Form, Cognacy_2 = Cognacy)

#vector of unique concepts to loop over
Parameters_ID_unique_vector <- forms$Parameter_ID %>% unique()

#index to start loop at
index <- 0

#for loop, calcuating the lv dist each time for all words within each concept
for(Parameter in Parameters_ID_unique_vector){

index <- index + 1

cat(paste0("I'm on ", Parameters_ID_unique_vector[index], ". Which is index ", index, " out of ", length(Parameters_ID_unique_vector), ".\n"))

forms_spec <- forms %>%
filter(Parameter_ID == Parameters_ID_unique_vector[index])

form_vec <- as.vector(forms_spec$Form)

names(form_vec) <- forms_spec$ID

dists <- stringdistmatrix(a = form_vec, b = form_vec, method = "lv",  useNames = "names")

dists[upper.tri(dists, diag = T)] <- NA

dists_long <- dists %>%
reshape2::melt() %>%
filter(!is.na(value)) %>%
filter(value <= 1)  %>%
distinct() %>%
mutate(Var1 = as.character(Var1)) %>%
mutate(Var2 = as.character(Var2)) %>%
rename(lv_dist = value) %>%
left_join(left, by = "Var1") %>%
left_join(right, by = "Var2") %>%
distinct()

dist_full <- full_join(dist_full, dists_long, by = c("Var1", "Var2", "lv_dist", "Form_1", "Cognacy_1", "Form_2", "Cognacy_2"))  %>%
  distinct()

}


joined %>%
  filter(lv_dist ==0) %>% 
  distinct(Var1) %>% nrow()


#different cognate same form
different_cognate_same_form <- forms %>% 
  distinct(Form_1, Parameter_ID, Cognacy) %>% 
  group_by(Form_1, Parameter_ID) %>%
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  distinct(Form_1, Parameter_ID)


#compare all dists

dists_sided <- dist_full %>% 
  left_join(left) %>% 
  left_join(right)