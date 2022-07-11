source("fun_def_h_load.R")

h_load(pkg = c("tidyverse", "reshape2", "ggplot2"))

OUTPUTDIR <- "output/dist_fixation_scores/"
if (!dir.exists(OUTPUTDIR)) {dir.create(OUTPUTDIR)}

cfx_macroarea_list <-   read_tsv(file.path(OUTPUTDIR, "cfx_AUTOTYP_macroarea_list.tsv"))

cfx_macroarea_list %>% 
  ggplot(aes(x = Vars, y = Value_cfx)) +
  geom_bar(aes(fill = 1 - Value_cfx), stat = "identity") +
  theme_classic() +
  theme(text = element_text(angle = 70, hjust = 1, size = 20), 
        legend.position = "None", 
        axis.title.x = element_blank())

mean(cfx_macroarea_list$Value_cfx)

ggsave(filename = file.path(OUTPUTDIR, "cfx_barplot_macroarea.png"), height =  7.89, width =  8.61)

cfx_AUTOTYP_area_list <-  read_tsv(file.path(OUTPUTDIR, "cfx_AUTOTYP_area_list.tsv"))

cfx_AUTOTYP_area_list %>% 
  ggplot(aes(x = Vars, y = Value_cfx)) +
  geom_bar(aes(fill = 1 - Value_cfx), stat = "identity") +
  theme_classic() +
  theme(text = element_text(angle = 70, hjust = 1, size = 20), 
        legend.position = "None", 
        axis.title.x = element_blank())

mean(cfx_AUTOTYP_area_list$Value_cfx)

ggsave(filename = file.path(OUTPUTDIR, "cfx_barplot_AUTOTYP_area.png"), height =  7.89, width =  8.61)


fns <- list.files(path = OUTPUTDIR, pattern = ".*cfx_Family_ID_list_cut_off.*")

for(fn in fns){

cfx_Family_ID_list <- read_tsv(fn)

plot_title <- fn

cfx_Family_ID_list %>% 
  ggplot(aes(x = Vars, y = Value_cfx)) +
  geom_bar(aes(fill = 1 - Value_cfx), stat = "identity") +
  theme_classic() +
  theme(legend.position = "None", 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) +
  ggtitle(plot_title) 

mean(cfx_Family_ID_list$Value_cfx)

ggsave(filename = paste0(OUTPUTDIR, "cfx_barplot", fn, ".png"), height =  7.89, width =  8.61)

}
