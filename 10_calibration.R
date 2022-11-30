# ------------------------------------------------------------------------------
# Cr?e par Francis Lessard dans le cadre du projet portant sur la cartographie
# de l'habitat de la Grive de Bicknell avec la Fondation de la faune Qu?bec et
# l'universite Laval
# Mis a jour le 2022-01-20

# Ce script permet d'effectuer une resource selection function (RSF) selon
# plusieurs modeles candidats (regression logistique) ayant ete soumis a un AIC.
# Le meilleur modele a ete selectionne pour effectuer une prediction.
# ------------------------------------------------------------------------------

# Installation des modules ====
library(tidyverse)
library(ggfortify)
library(magrittr)
library(sp)
library(raster)
library(sf)
library(AICcmodavg)
library(pROC)
library(PresenceAbsence)
library(corrplot)
library(caret)
library(corrplot)
library(lme4)




# Lecture des donnees finales ====
# Donnees de presence-absence
st_read("D:/02_data_bird/presence_absence_data.shp") %>%
  mutate(GRBI = GRBI %>% factor) %>% 
  dplyr::select(GRBI,
                mhcmean_corr = mhcmn_c,
                var_corr = var_crr,
                altitude = altitud,
                psab,
                ufcgap) %>% 
  st_transform(crs = 2949) -> data

# AICc et selection du meilleur modele ----
# Creation des equations
# Autre maniere d'ecrire le terme quadratique : poly(mhcmean_corr, degree=2, raw=TRUE) vs. mhcmean_corr + I(mhcmean_corr^2)
equation <- c(GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + var_corr + ufcgap + psab, # 1
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + var_corr + ufcgap, # 2
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + ufcgap + psab, # 3
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + ufcgap, # 4
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + psab, # 5     
              GRBI ~ altitude + mhcmean_corr + I(mhcmean_corr^2) + var_corr, # 6
              GRBI ~ 1) # 7

# Creation de tous les modeles
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

# Permet d'aller chercher le meilleur modele
mod <- get(table_aicc %>% slice(1) %>% pull(1))
# Pour garder les p.values et faire un tableau resume
summary(mod)





# Prediction pour 2022 -----
# Probabilite dans tous le jeux de donnees
predict(mod, data, type = "response") -> data$predict

# Creation du raster stack des metriques
setwd("D:/04_metriques/04_projet_fill")
coef(mod) # Pour voir si toutes les metriques sont presentes
stack(raster("altitude.tif"),
      raster("mhcmean_corr_2022.tif"),
      raster("ufcgap.tif"),
      raster("psab.tif")) -> metriques
# Pour avoir le bon nom de raster pour la prediction
names(metriques)[2] <- 'mhcmean_corr'
names(metriques)

# Probabilite dans toute l'aire d'etude en format raster
prediction <- predict(metriques, mod, type = "response")

# Ecriture du raster
writeRaster(prediction, "D:/05_predictions/prediction_2022.tif")





# Visualisation ----
# Correlation entre les variable
data %>% 
  st_drop_geometry %>%  
  dplyr::select(mhcmean_corr, var_corr, ufcgap, psab, altitude ,-GRBI) %>%
  cor %>% 
  corrplot(method = "circle", addCoef.col = "black", type = 'lower', 
           tl.srt = 45, tl.cex = 2, number.cex = 1.5)

# Etendue des variables
data %>% 
  st_drop_geometry %>% 
  dplyr::select(mhcmean_corr, var_corr, ufcgap, psab, altitude , GRBI) %>%
  pivot_longer(!GRBI, names_to = "metrique", values_to = "value") %>%
  mutate(metrique = metrique %>% factor(levels = c("mhcmean_corr", "var_corr", "ufcgap", "psab", "altitude"))) %>% 
  arrange(metrique) %>%
  ggplot() +
  guides(fill=guide_legend(title="Type de point")) +
  xlab("Type de point") +
  ylab("Valeur") +
  theme(text = element_text(size=20)) +
  geom_boxplot(aes(GRBI, value, fill = GRBI)) +
  scale_fill_manual(values = c("#FF6666","green"),
                    labels = c("Pseudo-absence (0)", "Presence de GRBI (1)")) +
  facet_wrap(~metrique, scales="free", nrow = 1)

# Relation des variables avec le predict et si certaines ont des effets non lineaires ---
for(v in c("mhcmean_corr", "var_corr", "altitude", "psab", "ufcgap")){
  print(data %>% 
          rename(var = v) %>% 
          ggplot(aes(var, predict)) +
          theme(text = element_text(size=30)) +
          xlab(label = v) +
          ylab(label = "Probabilite de presence") +
          geom_point() +
          geom_smooth())
}

# ROC-AUC
par(pty = "s") # Pour ajuster la largeur du graphique sur l'axe des x
# Pour le jeu de donn?es test
roc(data$GRBI, data$predict) %>% 
  {r_roc <<- .} %>% 
  plot(legacy.axes = TRUE, percent = TRUE,
       xlab = "Taux de faux positifs", ylab = "Taux de vrai positif",
       cex.lab = 2, cex.axis = 2)
text(0.37, 0.5, paste0("AUC: ",r_roc$auc %>% round(3)), cex = 2)

# PCA
data %>%
  dplyr::select(chm_corr = mhcmean_corr, var_corr, elevation = altitude, pbalsamfir = psab, ufcgap) %>% # S?lection des variables du meilleur mod?les
  st_drop_geometry %>% 
  drop_na %>% # ? corriger plus tard dans la bd originale pour ne pas avoir de m?trique NA
  prcomp(scale = TRUE) -> PCA
plot(PCA, col = "red")
summary(PCA)
PCA$rotation
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
dev.copy(pdf, "C:/0_Donnees/03_Projets/02_GRBI/04_Redaction/Article/Figures/Figure_4.pdf", width=12, height=10)
dev.off()