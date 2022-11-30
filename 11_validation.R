# ------------------------------------------------------------------------------
# Cr?e par Francis Lessard dans le cadre du projet portant sur la cartographie
# de l'habitat de la Grive de Bicknell avec la Fondation de la faune Qu?bec et
# l'universit? Laval
# Mis ? jour le 2022-01-20

# Ce script permet d'effectuer une resource selection function (RSF) selon
# plusieurs mod?les candidats (r?gression logistique) ayant ?t? soumis ? un AIC.
# Le meilleur mod?le ? ?t? s?lectionn? pour effectuer une pr?diction.
# ------------------------------------------------------------------------------

# Installation des modules ====
library(tidyverse)
library(magrittr)
library(sp)
library(sf)
library(raster)
library(corrplot)
library(AICcmodavg)
library(PresenceAbsence)
library(caret)
library(ggfortify)
library(pROC)



# # Prediction du modele au niveau des points d'ecoute (avec une correction de la hauteur des arbres) / a faire une seule fois ----
# # Raster stack des metriques
# setwd("D:/04_metriques/04_projet_fill")
# coef(mod) # Pour voir si toutes les metriques sont presentes
# stack(raster("altitude.tif"),
#       raster("mhcmean.tif"),
#       raster("ufcgap.tif"),
#       raster("psab.tif"),
#       raster("D:/01_data_autre/index_lidar_10m.tif"),
#       raster("D:/01_data_autre/aah_id_10m.tif")) -> metriques
# # Lecture des polygones ecoforestiers avec la courbe de croissance (aah0 = aah1)
# st_read("D:/01_data_autre/Data.gdb", "AAH") %>%
#   st_drop_geometry %>%
#   dplyr::select(-SHAPE_Length, -SHAPE_Area) %>%
#   mutate(aah_id_10m = ID) %>% 
#   mutate(across(matches("aah"), as.numeric)) -> AAH
# 
# # Lecture des points d'ecoute
# st_read("D:/02_data_bird/point_ecoute_clip_decalage.shp") %>%
#   mutate(GRBI = GRBI %>% as.factor) -> pointcounts
# 
# for(i in 1:nrow(pointcounts)){ # Iteration d'un point d'ecoute pour la correction du mhc puis la prediction
#   cat(paste0("Points d'ecoute no. ",i, "\n"))
#   pointcounts %>%
#     slice(i) %>% # Extraction d'un point d'ecoute
#     st_buffer(50) -> pointcountsbuffer # Buffer de 50 m
# 
#   metriques %>%
#     crop(extent(pointcountsbuffer)) %>% # Etendue des metriques du point d'ecoute buffer 50
#     mask(pointcountsbuffer) -> metriques_pcb # Masque des metriques du point d'ecoute buffer 50
# 
#   metriques_pcb %>%
#     raster::subset(c(2,5,6)) %>% # Selection du metrique mhcmean de index_lidar_10m et de aah_id_10m
#     as.data.frame(xy=TRUE) %>% # Conversion en dataframe avec les coordonnees
#     merge(AAH, by=c("aah_id_10m")) %>% # Jointure des courbes de croissance selon l'ID unique AAH (peuplement)
#     mutate(decalage = pointcountsbuffer$decalage) %>% # Decalage lidar
#     dplyr::select(-index_lidar_10m) -> mhcmean_pcb # Retrait de l'index lidar
# 
#   # Eventuellement en faire une seule fonction qui ne fait que corriger les valeur dans une table ayant déjà les courbes de croissance aah
#   while (!all(mhcmean_pcb$decalage == 0)) {
#     mhcmean_pcb %<>%
#       rowwise %>% # Permet de faire les corrections une ligne a la fois
#       mutate(grow = if(decalage == 0){ # Ne va pas chercher l'accroissement annuel si le champ "decalage" est de 0
#         0
#       } else if (mhcmean < 0.5 & sign(decalage) == -1){ # Ne fait pas reduire le mhc s'il fait moins de 50 cm
#         0
#       } else if (mhcmean > 40){ # Si la hauteur de la canopee est superieure a 40 m, la courbe de croissance de 40 m est sélectionnee (hauteur max de la courbe de croissance)
#         aah40
#       } else {
#         get(paste0("aah",floor(mhcmean))) # Permet d'aller chercher le bon champ de la courbe de croissance
#       }) %>% 
#       mutate(mhcmean = mhcmean + (grow*sign(decalage)/100)) %>% # Correction de la hauteur de canopee, le metrique est en m et le grow en cm d'ou le /100
#       mutate(decalage = ifelse(decalage == 0, 0, decalage - 1*sign(decalage))) # Permet de faire reduire le decalage de 1 an, s'il est de 0, il restera le meme et ne sera pas corrigee a la prochaine iteration
#   }
#   
#   mhcmean_pcb %>%
#     dplyr::select(x, y, mhcmean) %>%
#     rasterFromXYZ(10, crs = 2949) %>% # Conversion en raster
#     resample(metriques_pcb) -> metriques_pcb$mhcmean # Mise a jour du mhc corriga dans le raster stack
#   
#   names(metriques_pcb)[2] <- 'mhcmean_corr' # Mise a jour du nom du metrique mhcmean vers mhcmean_corr pour que le mod?le fonctionne
#   predict(metriques_pcb, mod, type = "response") %>% # Prediction du modele dans un rayon de 50 m
#     {.@data@max} -> pointcounts$predict[[i]] # Extraction de la valeur la plus elevee dans la BD
# }
# 
# # Ecriture des points d'ecoute avec la valeur de prediction
# pointcounts %>%
#   mutate(predict = predict %>% as.numeric())  %>%
#   st_write("D:/02_data_bird/point_ecoute_predict.shp")
# 
# 
# 
# 
# 
# # Pour enlever les presence avec 10 m de precision des donnees de points d'ecoute / a faire une seule fois ----
# # Lecture des points d'ecoute avec la valeur de pr?diction
# st_read("D:/02_data_bird/point_ecoute_predict.shp") %>%
#   mutate(GRBI = GRBI %>% as.factor) -> pointcounts
# 
# st_read("D:/02_data_bird/point_ecoute.shp") %>% # Points d'ecoutes
#   filter(abs(decalage) <= 5) %>% # Tri a 5 ans du releve lidar
#   filter(GRBI > 0) -> presence_pe # Retrait des points d'ecoute sans presente de GRBI
# 
# st_read("D:/02_data_bird/presence.shp") %>% # Donnees de presence a plus ou moins 10 m
#   st_transform(crs = 2949) -> presence
# 
# drop <- list() # Creation d'une liste des doublons a enlever
# id <- 1 # Iterateur de la liste precedente
# for(i in 1:nrow(presence_pe)){ # Regarde les points d'ecoute avec presence de GRBI un a un
#   if(presence_pe %>%
#      slice(i) %T>%
#      {d <<- .$Date_} %>% # Garde en memoire la date du point d'ecoute
#      st_buffer(110) %>%  # Buffer de 110 (plus grande distance de presence issue des azimuts)
#      st_intersection(presence) %>%  # Le point d'ecoute est-il a moins de 110 m d'un point de presence precise
#      filter(date == d) %>% # Possede-t-il la meme datee
#      nrow > 0) { # S'il possede la meme date le nrow sera de 1 car il sera conserve (plus grand que 0)
#     cat(paste0(i," doublon\n"))
#     presence_pe %>% slice(i) -> drop[[id]] # On garde donc en memoire ce point d'ecoute qui est un doublon et qu'il faudra enlever
#     id <- id + 1 # L'iterateur de la bd de doublons est incremente
#   } else {
#     cat(i," pas de doublon\n") # Rien si nrow == 0 car pas un doublon
#   }
# }
# drop %<>% 
#   bind_rows() # On combine les observations de la liste dans une seule BD
# 
# drop %>% 
#   st_zm %>% 
#   st_write("D:/02_data_bird/point_ecoute_predict_drop.shp")
# 
# # On retire les doublons selon un id (I)
# pointcounts %>% 
#   rowid_to_column() %>% 
#   {pointcounts <<- .} %>% 
#   filter(GRBI == 1) %>% 
#   st_intersection(drop) %>% 
#   pull(rowid) -> pointcounts_drop
# 
# pointcounts %<>% 
#   filter(!rowid %in% pointcounts_drop)
# 
# # On conserve uniquement les observations a plus de 600 m
# pointcounts %<>%
#   st_intersection(st_read("D:/01_data_autre/altitude_600m_poly.shp")) 
# 
# pointcounts %>%
#   st_write("D:/02_data_bird/point_ecoute_predict_tri.shp")
# 
# 
# 
# 

# Resultats ----
st_read("D:/02_data_bird/point_ecoute_predict_tri.shp") %>%
  mutate(GRBI = GRBI %>% as.factor) -> pointcounts

# Raster de pr?diction 2022 pour avoir les superficies
raster("D:/05_predictions/prediction_2022.tif") %>% 
  as.vector %>% 
  na.omit -> prediction_vector
  

# Determination des seuils et matrice de confusion
pointcounts %>%
  st_drop_geometry %>%   
  mutate(ID = 1:nrow(pointcounts)) %>% 
  dplyr::select(ID, GRBI, predict) %>% 
  mutate(ID = ID %>% as.factor,
         GRBI = GRBI %>% as.character %>% as.numeric,
         predict = predict %>% as.numeric) %>%
  optimal.thresholds(req.sens = 0.95, req.spec = 0.95) -> seuils

# Sens = Spec (0.2)
pointcounts %>% 
  mutate(predict_class = ifelse(predict > seuils[2,2], 1, 0) %>% factor) %>% 
  {confusionMatrix(data=.$predict_class, reference=.$GRBI, positive = "1")}
# Pourcentage de l'aire d'etude
(sum(prediction_vector > seuils[2,2])/100)/(length(prediction_vector)/100)*100

# 95 % spec = hotpots (0.3)
pointcounts %>% 
  mutate(predict_class = ifelse(predict > seuils[11,2], 1, 0) %>% factor) %>% 
  {confusionMatrix(data=.$predict_class, reference=.$GRBI, positive = "1")}
# Pourcentage de l'aire d'etude
(sum(prediction_vector > seuils[11,2])/100)/(length(prediction_vector)/100)*100

# ROC-AUC presence et pseudo absence (points d'ecoute)
par(pty = "s") # Pour ajuster la largeur du graphique sur l'axe des x
roc(pointcounts$GRBI, pointcounts$predict) %>% 
  {r_roc <<- .} %>% 
  plot(legacy.axes = TRUE, percent = TRUE,
       xlab = "False positive rate (1 - Specificity)",
       ylab = "True positive rate (Sensibility)",
       cex.lab = 2, cex.axis = 2)
text(0.37, 0.5, paste0("AUC: ",r_roc$auc %>% round(3)), cex = 2)
