# Installation des modules ----
library(tidyverse)
library(magrittr)
library(sp)
library(raster)
library(sf)
library(whitebox)
library(XML)

# Traitements ----
setwd("D:/05_predictions")
wbt_fast_almost_gaussian_filter(input = "./prediction_2022.tif", output = "./gaus.tif", sigma = 1.8)
wbt_reclass(input = "./gaus.tif", output = "./reclass.tif", reclass_vals = '0;0;0.206;1;0.206;0.288;2;0.288;1')
wbt_majority_filter(input = "./reclass.tif", output = "./maj.tif", filterx = 5, filtery = 5)
wbt_set_nodata_value(input = "./maj.tif", output = "./nodata.tif")
wbt_raster_to_vector_polygons(input = "./nodata.tif", output = "./prediction_2022_poly.shp")
st_read("./prediction_2022_poly.shp") %>% 
  mutate(ID = 1:nrow(.)) %>% 
  st_write("./prediction_2022_poly.shp", append = FALSE)
wbt_vector_polygons_to_raster(input = "./prediction_2022_poly.shp", output = "./poly_id.tif", field = 'ID', base = "./prediction_2022.tif")
wbt_zonal_statistics(input = "./prediction_2022.tif", features = "./poly_id.tif", out_table = "./stats_table.html")
readHTMLTable("./stats_table.html") %>% 
  .[["NULL"]] %>% 
  mutate(ID = as.numeric(`Feature ID`) + 1) %>%
  dplyr::select(ID, Mean) %>% 
  merge(st_read("./prediction_2022_poly.shp"), by = "ID") %>% 
  mutate(VALUE = case_when(Mean >= 3 ~ 2,
                           TRUE ~ VALUE)) %>% 
  mutate(Habitat = case_when(VALUE == 1 ~ "Bon habitat",
                             VALUE == 2 ~ "Très bon habitat")) %>%
  mutate(Seuil = case_when(VALUE == 1 ~ "Sensibilité = Spécificité (80 %)",
                           VALUE == 2 ~ "Spécificité = 95 %")) %>% 
  dplyr::select(-FID, -VALUE) %>% 
  mutate(HA = as.numeric(st_area(st_read("./prediction_2022_poly.shp"))/10000)) %>% 
  filter(HA > 0.05) %>% 
  filter(Mean > 0.2) %>% 
  st_write("./prediction_2022_poly.shp", append = FALSE)

file.remove("./gaus.tif",
            "./reclass.tif",
            "./maj.tif",
            "./nodata.tif",
            "./poly_id.tif",
            "./stats_table.html")
