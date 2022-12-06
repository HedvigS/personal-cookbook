library(tidyverse)
library(bib2df)

#making a bibtex file to wrangle. Note that for the first entry, there aren't spaces around the equal sign for the author-field, which bib2df will struggle with.

write_lines(x = c("@misc{s_OaPaul_Gabadi,"       ,                                                                                                                 
 "    author={Oa, Morea and Paul, Ma'oni},"              ,                                              
"    glottolog_ref_id = {320579},"       ,                                                             
 "    hhtype = {grammar_sketch},"      ,                                                                
 "    howpublished = {Ms.},"   ,                                                                        
 "    inlg = {English [eng]},"  ,                                                                       
 "    lgcode = {Gabadi = Kabadi [kbt]},"  ,                                                             
 "    macro_area = {Papua},"       ,                                                                    
 "    pages = {40},"          ,                                                                         
 "    title = {Tentative Grammar Description for the Gabadi Language},"    ,                            
 "    url = {http://www.sil.org/pacific/png/abstract.asp?id=928474556178},"   ,                         
 "    year = {2013}"                                                    ,                               
 "}"                        ,                                                                           
 ""                     ,                                                                               
 "@article{ld_Timoteo_Kabadi,"       ,                                                                  
 "    author = {Timoteo, Pastor},"      ,                                                               
 "    glottolog_ref_id = {16778},"          ,                                                           
"    hhtype = {minimal},"            ,                                                                 
 "    inlg = {English [eng]},"         ,                                                                
 "    journal = {Journal of the Polynesian Society},"       ,                                           
 "    lgcode = {Kabadi = Abadi [kbt]},"          ,                                                     
 "    macro_area = {Papua},"              ,                                                             
"    pages = {201-208},"                                    ,                                          
 "    title = {Notes on the Kabadi dialect of New Guinea},"    ,                                        
 "    volume = {6},"                    ,                                                     
 "    year = {1897}"      ,
"}"         ), file = "sources.bib")

#making a function that just inserts spaces before and after equal signs in bibflies
add_spaces_for_bib2df <- function(bib_fn){

new_fn <- paste0( str_replace(bib_fn, ".bib", ""), "_sep", ".bib")

  read_lines(bib_fn) %>% 
  str_replace_all(regex("\\=\\{"), regex(" \\= \\{")) %>% 
  write_lines(new_fn)
}

#running function 
add_spaces_for_bib2df(bib_fn = "sources.bib")

#reading in bib-file 
bib <-  bib2df::bib2df("sources_sep.bib", separate_names = F)

