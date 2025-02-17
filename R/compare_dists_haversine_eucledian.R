library(tidyverse)
library(rcldf)
library(fields)
library(reshape2)
library(rgrambank)
library(viridis)

lgs <- c( "abun1252", "area1240", "axam1237", "chuk1273", "ersu1241", "esto1258", "finn1318", "gahr1239", "ingr1248", "kemb1249",
"lena1238", "lezg1247", "livv1243", "livv1244", "lule1254", "mand1415", "mode1248", "moks1248", "motl1237", "nana1257",
 "nenn1238", "noct1238", "nucl1301", "panj1256", "pite1240", "poli1260", "seme1247", "sout2674", "stan1290", "stan1293",
 "supy1237", "tami1289", "tojo1241", "kare1335", "west2519", "west2418", "wutu1241", "agob1244", "anci1242", "arak1252",
 "aton1241", "cosa1234", "east2330", "enuu1235", "faro1244", "kaya1315", "kxoe1243", "lati1261", "mana1298", "matu1261",
"maun1240", "ngan1291", "nort2747", "nort2942", "pahn1237", "pela1242", "sout2959", "tibe1272", "udih1248", "vure1239",
 "youn1235", "baim1244", "balu1257", "bola1250", "chip1261", "chon1284", "chuv1255", "emai1241", "futu1245", "gayo1244",
"geba1237", "halk1245", "hoaa1235", "kenu1236", "sout2728", "njan1240", "nort2690", "oriy1255", "rapa1245", "russ1263",
"samo1305", "thak1245", "toab1237", "tsix1234", "vera1241", "zaye1235", "akab1249", "alab1254", "crow1244", "ludi1246",
"mala1464", "mang1381", "mwag1236", "nyah1250", "saar1237", "seim1238", "shos1248", "sing1264", "skol1241", "tuwa1243",
"unua1237", "wara1294", "wuch1236", "wusa1235", "yoga1237", "youl1235", "baba1267", "buwa1243", "chak1271", "czec1258",
 "guan1266", "iatm1242", "koro1298", "mana1288", "maor1246", "maru1249", "nucl1302", "nucl1305", "kuma1276", "onon1246",
 "otta1242", "papu1250", "ruka1240", "sout2956", "woii1237", "anuu1241", "buga1247", "digo1243", "east2342", "east2773",
"gyel1242", "hadz1240", "kunb1251", "lewo1244", "mapu1245", "mara1378", "mund1325", "nene1249", "selk1253", "torr1261",
 "west2396", "yaku1245", "dege1246", "iris1253", "mual1241", "nort2885", "puri1258", "shab1252", "wall1257", "wamb1257",
 "west2516", "xamt1239", "xara1244", "yuca1254", "abkh1244", "afar1241", "arap1274", "bwat1240", "cham1312", "gheg1238",
 "itza1241", "kach1284", "kile1243", "kinn1249", "maee1241", "ngal1292", "noon1242", "nort2697", "papa1265", "popt1235",
"sais1237", "sout2856", "tahi1242", "tanz1241", "tiri1258", "wawa1246", "burj1242", "carn1240", "cent2314", "daka1243",
"dima1251", "haaa1252", "here1253", "mpad1242", "nort2722", "saka1289", "bumt1240", "yaki1237", "dusn1237", "kagu1239",
 "kats1235", "kwar1239", "lolo1259", "munc1235", "nava1243", "phai1238", "surg1248", "tama1365", "tief1244", "waim1251",
 "icel1247", "japh1234", "kore1280", "loni1238", "mina1276", "pend1242", "toho1245", "west2466", "adan1251", "argo1244",
 "bebe1248", "boro1282", "chil1280", "djam1255", "kaik1246", "kash1277", "kuku1273", "marg1265", "mart1255", "oksa1245",
 "pile1238", "sout2807", "tutu1241", "wari1268", "kett1243", "komb1274", "mbul1263", "mere1242", "pila1245", "tetu1246",
 "djee1236", "furr1244", "kala1399", "kama1351", "koda1255", "lama1277", "niua1240", "quio1240", "yems1235", "bant1281",
 "chal1275", "lavu1241", "luis1253", "maxa1247", "anei1239", "buru1320", "cemu1238", "daza1242", "gube1234", "khas1269",
 "lave1248", "sout2857", "beka1241", "crim1257", "east2516", "huic1243", "idun1242", "taus1251", "copt1239", "mini1251",
"rung1258", "waiw1244", "west2635", "yaka1277", "ulit1238", "huas1242", "mais1250", "qanj1241", "taba1263", "biao1253",
 "cowl1242", "koor1239", "lamk1238", "lemb1266", "mari1440", "russ1264", "eyak1241", "maib1239", "sunw1242", "mara1386",
 "take1257", "yoku1256", "bimi1240", "shol1240", "chuj1250", "nucl1240", "paul1238", "pang1290", "mayo1261", "ugar1238",
 "ghan1242", "timn1235", "baro1253", "kalm1243", "achi1256", "tsis1238", "meru1245", "awet1244", "auuu1241", "mwan1247", "amar1274", "lega1249")

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

Glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable

Glottolog_LanguageTable <- glottolog_rcldf_obj$tables$LanguageTable 

Glottolog_LanguageTable_ValueTable <- rgrambank::combine_ValueTable_LanguageTable(LanguageTable = Glottolog_LanguageTable, ValueTable = Glottolog_ValueTable, Is_Glottolog = T)

Glottolog_Table <- Glottolog_LanguageTable_ValueTable %>% 
#  filter(Level == "language") %>% 
#  filter(category == "Spoken_L1_Language") %>% 
#  filter(med == "0"|med == "1"| med == "2") %>% 
  filter(Glottocode %in% lgs) %>% 
  filter(!is.na(Longitude)) %>% 
  column_to_rownames("Glottocode") %>% 
  dplyr::select(Longitude, Latitude)

Glottolog_Table_Var1 <- Glottolog_Table %>% 
  rownames_to_column("Var1") %>% 
  rename(Longitude_Var1 = Longitude, Latitude_Var1 = Latitude)

Glottolog_Table_Var2 <- Glottolog_Table %>% 
  rownames_to_column("Var2")  %>% 
  rename(Longitude_Var2 = Longitude, Latitude_Var2 = Latitude)

dists_Euklides_mat  <- stats::dist(x = Glottolog_Table) %>% 
  as.matrix()

dists_Euklides_mat[upper.tri(dists_Euklides_mat, diag = T)] <- NA

dists_Euklides <- dists_Euklides_mat  %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  rename(dist_Euklides  = value) 

dist_haversine_mat <- fields::rdist.earth(x1 = Glottolog_Table, x2 = Glottolog_Table, miles = F) 
dist_haversine_mat[upper.tri(dist_haversine_mat, diag = T)] <- NA

rownames(dist_haversine_mat) <- rownames(Glottolog_Table)
colnames(dist_haversine_mat) <- rownames(Glottolog_Table)

dist_haversine <- dist_haversine_mat %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  rename(dist_haversine = value)

joined <- full_join(dists_Euklides, dist_haversine, by = join_by(Var1, Var2)) %>% 
  left_join(Glottolog_Table_Var1) %>% 
  left_join(Glottolog_Table_Var2)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

joined$dist_Euklides_km_est <- joined$dist_Euklides *110
joined$dist_Euklides_scaled <- range01(joined$dist_Euklides)
joined$dist_haversine_scaled <- range01(joined$dist_haversine)

joined <- joined %>% 
  tidyr::unite(Var1, Var2, col= "pair", remove = F) %>% 
  mutate(diff = abs( dist_Euklides_scaled - dist_haversine_scaled)) %>% 
  mutate(diff_km = abs(joined$dist_Euklides_km_est - dist_haversine))

joined  %>% 
  ggplot() +
  geom_point(aes(dist_haversine, dist_Euklides_km_est, color = diff ), alpha = 0.2, shape =16) +
  theme_classic() +   
  coord_fixed() +
  viridis::scale_color_viridis(option = "inferno", end = 0.9)

ggsave("output/dist_haversien_euclide_comparison_scatterplot.png")

#rendering a worldmap that is pacific centered
world <- map_data('world', ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", col="white", border="gray", ylim=c(-56,80), margin=T)


joined %>% 
  filter(dist_haversine <= 100) %>% 
  ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group),
               colour="gray87",
               fill="gray87", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long, y=lat, group=group),
               colour="gray87",
               fill="white", size = 0.3) +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank() ) +
  geom_segment(aes(x = Longitude_Var1, y = Latitude_Var1, 
                   xend = Longitude_Var2, yend = Latitude_Var2, color = diff),
                   alpha = 1, size = 2) +
#  geom_point(aes(x = Longitude_Var1, y = Latitude_Var1), colour = "blue", size = 0.5) +  # First set of points
#  geom_point(aes(x = Longitude_Var2, y = Latitude_Var2), colour = "blue", size = 0.5) +    # Second set of points
  viridis::scale_color_viridis(option = "inferno", end = 0.9)

ggsave("output/dist_haversien_euclide_comparison_map.png")

joined %>% 
  transform(bin = cut(diff, 40)) %>% 
  group_by(bin) %>% 		
  dplyr::summarize(n = n())		%>% 
  ggplot() +
  geom_bar(aes(x = bin, y = n, fill = bin), stat = "identity") +
  theme_minimal() +
  theme(     legend.position = "None",    axis.text.x = element_text( angle = 70, hjust=0.95)	) +
  viridis::scale_fill_viridis(option = "inferno", end = 0.9, discrete = T)
  
  ggsave("output/dist_haversien_euclide_comparison_hist.png")
  
  
x <-  joined %>% 
  filter(dist_haversine <= 100)

x$diff_km %>% mean()

joined %>% 
  ggplot() +
  geom_point(aes(x = dist_haversine, y = diff), alpha = 0.2, shape = 16)

