library(tidyverse)
library(cluster)
library(reshape2)
library(stringdist)

forms <- read_csv("https://github.com/lexibank/abvd/raw/ccff2bc86c30b102cd5b95174fafb378ddc0d3eb/cldf/forms.csv", show_col_types = F) %>% 
  filter(Loan == FALSE)

unknown_cognacy <-  forms %>% 
  filter(is.na(Cognacy)) %>% 
  dplyr::select(ID_Var1= ID, Var1 = Form)  

known_cognacy <- forms %>% 
  filter(!is.na(Cognacy)) %>% 
  dplyr::select(Var2 = Form, Var2_Cognacy = Cognacy)  

percentage_unknown <- round(nrow(unknown_cognacy)/ (nrow(forms)), digits = 2) 

percentage_unknown <- paste0(percentage_unknown*100, "%")

cat(paste0(percentage_unknown, " of forms do not have cognacy specified."))

known_cognacy <- known_cognacy %>% distinct()
unknown_cognacy <- unknown_cognacy %>% distinct()

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
#filter(Parameter_ID == "122_water")

form_vec <- as.vector(forms_spec$Form)

names(form_vec) <- forms_spec$ID

dists <- stringdistmatrix(a = form_vec, b = form_vec, method = "lv",  useNames = "names")

dists[upper.tri(dists, diag = T)] <- NA

dists_long <- dists %>%
reshape2::melt() %>%
filter(!is.na(value)) %>%
filter(value <= 2)  %>%
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

#different cognate same form
different_cognate_same_form_incl_multiple <- forms %>% 
  distinct(Form, Parameter_ID, Cognacy) %>% 
  group_by(Form, Parameter_ID) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1) 

different_cognate_same_form_excl_multiple <- forms %>% 
  filter(!str_detect(Cognacy, ",")) %>% 
  distinct(Form, Parameter_ID, Cognacy) %>% 
  group_by(Form, Parameter_ID) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1) 

cat("There are ", nrow(different_cognate_same_form_excl_multiple), " concept-form matchings ('144_toburn' - 'sunu') where there are identical forms assigned to different cognacy classes. If you also include cases of multiple cognacy (e.g. '1, 50', there are ", nrow(different_cognate_same_form_incl_multiple), " of this kind.\n", sep = "")


#forms with missing cognacy that could probably be filled in easily
possible_matches <- dist_full %>% 
  filter(is.na(Cognacy_2)) %>% 
  filter(!is.na(Cognacy_1)) %>% 
  filter(lv_dist <= 0)

cat("There are ", nrow(possible_matches 
), " words where you could easily fill in the cognacy because they are identical to other words which are already filled in for cognacy. For example, 'tangan' for the concept hand is assigned cognacy class 18 in some languages but no cognacy in others. The amount that can be filled in like this are ",round(nrow(possible_matches 
) / nrow(forms), 2) *100, "% of the entire dataset.\n", sep = "")

