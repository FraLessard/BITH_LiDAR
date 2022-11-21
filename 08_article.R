# ------------------------------------------------------------------------------
# Crée par Francis Lessard dans le cadre du projet portant sur la cartographie
# de l'habitat de la Grive de Bicknell avec la Fondation de la faune Québec et
# l'université Laval
# Mis à jour le 2022-01-20

# Ce script permet d'effectuer une resource selection function (RSF) selon
# plusieurs modèles candidats (régression logistique) ayant été soumis à un AIC.
# Le meilleur modèle à été sélectionné pour effectuer une prédiction.
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





# Lecture des données finales ====
# Données de présence-absence
st_read("D:/02_data_bird/presence_absence_data.shp") %>%
  mutate(GRBI = GRBI %>% factor) %>% 
  dplyr::select(GRBI,
                mhcmean_corr = mhcmn_c,
                var_corr = var_crr,
                altitude = altitud,
                psab,
                ufcgap) %>% 
  st_transform(crs = 2949) -> data





# Visualisation (matrice de corrélation, boxplot, pca) ====
# Matrice de corrélation
jpeg("C:/0_Donnees/03_Projets/03_GRBI/Rapports/Article/Figures/corrplot.jpg",width=800, height=800)
data %>% 
  st_drop_geometry %>%  
  dplyr::select(chm_corr = mhcmean_corr, var_corr, ufcgap, pbalsamfir = psab, elevation = altitude ,-GRBI) %>%
  cor %>% 
  corrplot(method = "circle", addCoef.col = "black", type = 'lower', 
           tl.srt = 45, tl.cex = 2, number.cex = 1.5, axis.cex = 1.5)
dev.off()

# Boxplot
jpeg("C:/0_Donnees/03_Projets/03_GRBI/Rapports/Article/Figures/boxplot.jpg",width=1000, height=800)
data %>% 
  st_drop_geometry %>% 
  dplyr::select(chm_corr = mhcmean_corr, var_corr, ufcgap, pbalsamfir = psab, elevation = altitude, GRBI) %>%
  pivot_longer(!GRBI, names_to = "metrique", values_to = "value") %>%
  mutate(metrique = metrique %>% factor(levels = c("chm_corr", "var_corr", "ufcgap", "pbalsamfir", "elevation"))) %>% 
  arrange(metrique) %>%
  ggplot() +
  guides(fill=guide_legend(title="Type")) +
  ylab("Value") +
  theme(text = element_text(size=20)) +
  geom_boxplot(aes(GRBI, value, fill = GRBI)) +
  scale_fill_manual(values = c("#FF6666","green"),
                    labels = c("Pseudo-absence (0)", "Presence (1)")) +
  facet_wrap(~metrique, scales="free", nrow = 1)
dev.off()

# PCA
data %>%
  dplyr::select(chm_corr = mhcmean_corr, var_corr, elevation = altitude, pbalsamfir = psab, ufcgap) %>% # Sélection des variables du meilleur modèles
  st_drop_geometry %>% 
  drop_na %>% # À corriger plus tard dans la bd originale pour ne pas avoir de métrique NA
  prcomp(scale = TRUE) -> PCA
plot(PCA, col = "red")
summary(PCA)
PCA$rotation
jpeg("C:/0_Donnees/03_Projets/03_GRBI/Rapports/Article/Figures/pca.jpg",width=800, height=550)
autoplot(PCA, data = data %>% st_drop_geometry, colour = 'GRBI', label = FALSE, size = 'GRBI',
         loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 8, loadings.label.colour = 'black',
         frame = TRUE, frame.type = 'norm', loadings.label.repel=T) + 
  theme(text = element_text(size=20)) +
  guides(size = "none") +
  guides(fill = "none") +
  guides(color=guide_legend(title="Type")) +
  scale_fill_manual(values = c("#FF6666","green")) +
  scale_size_manual(values = c(1.5,4)) +
  scale_color_manual(values = c("#FF6666","green"),
                     labels = c("Pseudo-absence (0)", "Presence (1)"))
dev.off()





# AICc et sélection du meilleur modèle et prédiction ====
# Creation des équations
# Autre manière d'écrire le terme quadratique : poly(mhcmean_corr, degree=2, raw=TRUE) vs. mhcmean_corr + I(mhcmean_corr^2)
equation <- c(GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + var_corr + ufcgap + psab, # 1
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + var_corr + ufcgap, # 2
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + ufcgap + psab, # 3
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + ufcgap, # 4
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + psab, # 5     
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + var_corr, # 6
              GRBI ~ 1) # 7

# Création de tous les modèles
for(id in seq_along(equation)){
  assign(paste0("mod_", id),
         glm(formula = equation[[id]],
               data = data,
               family = binomial)) 
}

mget(ls(pattern = "mod_\\d+")) %>%
  {mod_list <<- .} %>% 
  aictab(second.ord = FALSE) -> table_aicc
table_aicc

# Permet d'aller chercher le meilleur modèle
mod <- get(table_aicc %>% slice(1) %>% pull(1))
# Pour garder les p.values et faire un tableau résumé
summary(mod)

# Prédiction
predict(mod, data, type = "response") -> data$predict





# Pour visualiser la relation des variables et si certaines ont des effets non linéaires ====
for(v in c("mhcmean_corr", "var_corr", "altitude", "psab", "ufcgap")){
  print(data %>% 
    rename(var = v) %>% 
    ggplot(aes(var, predict)) +
    xlab(label = v) +
    ylab(label = "Probabilité de présence") +
    geom_point() +
    geom_smooth())
}





# Seuillage (80 % sensibilité et spécificité) ====
# Permet de trouver les seuils optimaux selon différents paramètres de performance (fonction "optimal.thresholds")
data %>%
  st_drop_geometry %>%   
  mutate(ID = 1:nrow(data)) %>% 
  dplyr::select(ID, GRBI, predict) %>% 
  mutate(ID = ID %>% as.factor,
         GRBI = GRBI %>% as.character %>% as.numeric,
         predict = predict %>% as.numeric) %>%
  optimal.thresholds(req.sens = 0.80, req.spec = 0.80) -> seuils
seuils

# 80 % sensibilité
ifelse(data$predict > seuils[10,2], 1, 0) %>% 
  as.factor %>% 
  confusionMatrix(data=., reference=data$GRBI, positive = "1")

# 80 % spécificité
ifelse(data$predict > seuils[11,2], 1, 0) %>% 
  as.factor %>% 
  confusionMatrix(data=., reference=data$GRBI, positive = "1")





# ROC-AUC présence et pseudo absence (dispositif de calibration) ====
jpeg("C:/0_Donnees/03_Projets/03_GRBI/Rapports/Article/Figures/ROC_AUC_presence_pseudoabsence.jpg",width=800, height=800)
par(pty = "s") # Pour ajuster la largeur du graphique sur l'axe des x
roc(data$GRBI, data$predict) %>% 
  {r_roc <<- .} %>% 
  plot(legacy.axes = TRUE, percent = TRUE,
       xlab = "False positive rate (1 - Specificity)",
       ylab = "True positive rate (Sensibility)",
       cex.lab = 2, cex.axis = 2)
text(0.37, 0.5, paste0("AUC: ",r_roc$auc %>% round(3)), cex = 2)
dev.off()





# # Prédiction du modèle au niveau des points d'écoute (avec une correction de la hauteur des arbres) / à faire une seule fois ====
# # Raster stack des métriques
# setwd("D:/04_metriques/04_projet_fill")
# coef(mod) # Pour voir si toutes les métriques sont présentes
# stack(raster("altitude.tif"),
#       raster("mhcmean.tif"),
#       raster("ufcgap.tif"),
#       raster("psab.tif"),
#       raster("D:/01_data_autre/index_lidar_10m.tif"),
#       raster("D:/01_data_autre/aah_id_10m.tif")) -> metriques
# # Lecture des polygones écoforestiers avec la courbe de croissance (aah0 = aah1)
# st_read("D:/01_data_autre/Data.gdb", "AAH") %>% 
#   st_drop_geometry %>% 
#   dplyr::select(-SHAPE_Length, -SHAPE_Area) %>% 
#   mutate(aah_id_10m = ID) -> AAH
# 
# # Lecture des points d'écoute
# st_read("D:/02_data_bird/validation.shp") %>% 
#   mutate(GRBI = GRBI %>% as.factor) -> pointcounts
# 
# for(i in 1:nrow(pointcounts)){ # Itération d'un point d'écoute pour la correction du mhc puis la prédiction
#   cat(paste0("Points d'écoute no. ",i, "\n"))
#   pointcounts %>% 
#     slice(i) %>% # Extraction d'un point d'écoute
#     st_buffer(50) -> pointcountsbuffer # Buffer de 50 m
#   
#   metriques %>% 
#     crop(extent(pointcountsbuffer)) %>% # Étendue des métriques du point d'écoute buffer 50
#     mask(pointcountsbuffer) -> metriques_pcb # Masque des métriques du point d'écoute buffer 50
#   
#   metriques_pcb %>% 
#     raster::subset(c(2,5,6)) %>% # Sélection du métrique mhcmean de index_lidar_10m et de aah_id_10m
#     as.data.frame(xy=TRUE) %>% # Conversion en dataframe avec les coordonnées
#     merge(AAH, by=c("aah_id_10m")) %>% # Jointure des courbes de croissance selon l'ID unique AAH (peuplement)
#     mutate(decalage = pointcountsbuffer$decalage) %>% # Décalage du point d'écoute avec l'année lidar
#     dplyr::select(-index_lidar_10m) -> mhcmean_pcb # Retrait de l'index lidar
#   
#   # Correction du mhc
#   c <- 1 # Itérateur du nombre d'années à corriger
#   while (!all(mhcmean_pcb$decalage == 0)) { # Tant que le décalage n'est pas de 0
#     cat(paste0("Passe no. ",c, "\n"))
#     mhcmean_pcb %<>%
#       rowwise %>% # Pour effectuer les manipulation une ligne à la fois
#       mutate(grow = if(decalage != 0){ # Colonne d'accroissement
#         mhcmean %>% round(0) -> mhc # Extraction de la hauteur d'arbre actuelle
#         ifelse(mhc > 40, mhc <- 40, mhc <- mhc) # Si le mhc fait plus de 40 m, mettre la valeur de 40 comnme la courbe de croissance s'arrête à cette hauteur
#         ifelse(mhc == 0 & sign(decalage) == -1, 0, get(paste0("aah",mhc))) # Ne fait pas réduire le mhc s'il fait moins de 50 cm
#       }else{
#         0 # Ne fait pas changer le mhc s'il n'y a plus de décalage
#       }) %>% 
#       mutate(mhcmean = mhcmean+(grow*sign(decalage)/100)) %>% # Le métrique est en m et le grow en cm d'où le /100, le sign permet de réduire le mhc si cela s'applique
#       mutate(decalage = if(decalage != 0){ # Le décalage est-il différent de 0 ?
#         decalage - 1*sign(decalage) # Si oui, le rapproche de 0 d'une valeur de 1 ans
#       }else{
#         0 # Sinon, mettre 0 comme valeur de décalage
#       })
#     c <- c + 1 # Incrémentation de l'itérateur du nombre d'années à corriger, une année à été effectuée
#   }
#   
#   mhcmean_pcb %>% 
#     dplyr::select(x, y, mhcmean) %>% 
#     rasterFromXYZ(10, crs = 2949) %>% # Conversion en raster
#     resample(metriques_pcb) -> metriques_pcb$mhcmean # Mise à jour du mhc corrigé dans le raster stack
#   
#   names(metriques_pcb)[2] <- 'mhcmean_corr' # Mise à jour du nom du métrique mhcmean vers mhcmean_corr pour que le modèle fonctionne
#   predict(metriques_pcb, mod, type = "response") %>% # Prédiction du modèle dans un rayon de 50 m
#     {.@data@max} -> pointcounts$predict[[i]] # Extraction de la valeur la plus élevée dans la BD
# }
# 
# # Écriture des points d'écoute avec la valeur de prédiction
# pointcounts %>% 
#   mutate(predict = predict %>% as.numeric())  %>% 
#   mutate(GRBI = GRBI %>% as.factor) %>% 
#   st_write("D:/02_data_bird/validation_predict.shp")
#
#
#
#
#
# # Pour enlever les présence avec 10 m de précision des données de points d'écoute / à faire une seule fois ====
# # Lecture des points d'écoute avec la valeur de prédiction
# st_read("D:/02_data_bird/validation_predict.shp") %>% 
#   mutate(GRBI = GRBI %>% as.factor) -> pointcounts
# 
# st_read("D:/02_data_bird/Donnees_Inventaire_Clip.shp") %>% # Points d'écoutes
#   filter(abs(decalage) <= 5) %>% # Tri à 5 ans du relevé lidar
#   filter(GRBI > 0) -> presence_pe # Retrait des points d'écoute sans présente de GRBI
# 
# st_read("D:/02_data_bird/presence.shp") %>% # Données de présence à plus ou moins 10 m
#   st_transform(crs = 2949) -> presence
# 
# drop <- list() # Création d'une liste des doublons à enlever
# id <- 1 # Itérateur de la liste précédente
# for(i in 1:nrow(presence_pe)){ # Regarde les points d'écoute avec présence de GRBI un à un
#   if(presence_pe %>%
#      slice(i) %T>%
#      {d <<- .$Date_} %>% # Garde en mémoire la date du point d'écoute
#      st_buffer(110) %>%  # Buffer de 110 (plus grande distance de presence issue des azimuts)
#      st_intersection(presence) %>%  # Le point d'écoute est-il à moins de 110 m d'un point de présence précis?
#      filter(date == d) %>% # Possède-t-il la même date?
#      nrow > 0) { # S'il possède la même date le nrow sera de 1 car il sera conservé (plus grand que 0)
#     cat(paste0(i," doublon\n"))
#     presence_pe %>% slice(i) -> drop[[id]] # On garde donc en mémoire ce point d'écoute qui est un doublon et qu'il faudra enlever
#     id <- id + 1 # L'itérateur de la bd de doublons est incrémenté
#   } else {
#     cat(i," pas de doublon\n") # Rien si nrow == 0 car pas un doublon
#   }
# }
# 
# bind_rows(drop) -> drop # On combine les observations de la liste dans une seule BD
# 
# # On retire les doublons selon leur id (rownames)
# pointcounts %<>% 
#   slice(-(pointcounts %>% st_intersection(drop) %>% rownames %>% as.numeric %>% round(0)))
# 
# pointcounts %>% 
#   st_write("D:/02_data_bird/validation_predict_tri.shp")
# 
# 
# 
# 
#
# Validation ====
st_read("D:/02_data_bird/validation_predict_tri_600m.shp") %>% # Tri manuel des points d'écoute à plus de 600 m
  mutate(GRBI = GRBI %>% as.factor) -> pointcounts

# Raster de prédiction 2022 pour avoir les superficies
raster("D:/05_predictions/prediction_AICc.tif") %>% 
  as.vector %>% 
  na.omit -> prediction_vector
  

# Détermination des seuils et matrice de confusion
pointcounts %>%
  st_drop_geometry %>%   
  mutate(ID = 1:nrow(pointcounts)) %>% 
  dplyr::select(ID, GRBI, predict) %>% 
  mutate(ID = ID %>% as.factor,
         GRBI = GRBI %>% as.character %>% as.numeric,
         predict = predict %>% as.numeric) %>%
  optimal.thresholds(req.sens = 0.95, req.spec = 0.95)

# Sens = Spec
pointcounts %>% 
  mutate(predict_class = ifelse(predict > 0.206, 1, 0) %>% factor) %>% 
  {confusionMatrix(data=.$predict_class, reference=.$GRBI, positive = "1")}
(sum(prediction_vector > 0.206)/100)/(length(prediction_vector)/100)*100

# 95 % spec = hotpots
pointcounts %>% 
  mutate(predict_class = ifelse(predict > 0.288, 1, 0) %>% factor) %>% 
  {confusionMatrix(data=.$predict_class, reference=.$GRBI, positive = "1")}
(sum(prediction_vector > 0.288)/100)/(length(prediction_vector)/100)*100

# ROC-AUC présence et pseudo absence (points d'écoute)
jpeg("C:/0_Donnees/03_Projets/03_GRBI/Rapports/Article/Figures/ROC_AUC_pointcounts.jpg",width=800, height=800)
par(pty = "s") # Pour ajuster la largeur du graphique sur l'axe des x
roc(pointcounts$GRBI, pointcounts$predict) %>% 
  {r_roc <<- .} %>% 
  plot(legacy.axes = TRUE, percent = TRUE,
       xlab = "False positive rate (1 - Specificity)",
       ylab = "True positive rate (Sensibility)",
       cex.lab = 2, cex.axis = 2)
text(0.37, 0.5, paste0("AUC: ",r_roc$auc %>% round(3)), cex = 2)
dev.off()
