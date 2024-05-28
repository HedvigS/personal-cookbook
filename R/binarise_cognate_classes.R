library(tidyverse)
library(reshape2)

cognates <- read_csv("example_data/pretend_cognate_table_for_eze.csv", show_col_types = FALSE)

cognates_long <- cognates %>%  
  reshape2::melt(id.vars = "Language_ID") 

if(any(str_detect(string = cognates_long$value, "NA"), na.rm = TRUE)){
stop("The string NA is inside the cognacy, cannot proceed.\n")  
}

if(any(str_detect(string = cognates_long$value, "€"), na.rm = TRUE) |
   any(str_detect(string = cognates_long$variable, "€"), na.rm = TRUE))
  {
  stop("The string € occurrs, cannot proceed.\n")  
}

binarised <-  cognates_long %>% 
  unite(variable, value, col = "Cognateset_ID", sep = "€") %>% 
  mutate(Value = 1) %>% 
  complete(Language_ID, Cognateset_ID, fill = list(Value = 0)) %>% #putting in NA where it should be
  separate(Cognateset_ID, into = c("Concept", "Cognacy"), sep = "€", remove = FALSE) %>% 
  filter(!str_detect(Cognateset_ID, "€NA")) %>% 
  group_by(Language_ID, Concept) %>%
  mutate(n = sum(Value)) %>% 
  mutate(Value = ifelse(n == 0, NA, Value)) %>%
  reshape2::dcast(Language_ID ~ Cognateset_ID, value.var = "Value")
