

### interpolating and extrapolating national data

## creator: matti.kummu@aalto.fi 

# load libraries

library(sf)
library(terra)
library(Rfast)
library(zoo)
library(stringr)
library(openxlsx) #
library(readxl)
library(ggplot2)

library(broom)
library(tidyr)
library(tidyverse)
library(dplyr) 


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. load data -----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

cntryID_reg <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso3, RegionID)


cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data))

GIS_layers_gini <- unique(cntry_metaData_gini$GIS_data)



subnat_gis_combined <- read_sf("results/gisData_gini_combined.gpkg")

subnat_data_combined <-  subnat_gis_combined %>% 
  st_drop_geometry()


cntryGIS <- st_read('results/swiidDataFilled_gis.gpkg')



##### 2 prepare for extrapolation ----

years_in = 1990:2023

# centroid of adm0

if (file.exists('data_GIS/p_adm0_centroids.gpkg')) {
  p_adm0_centroids <- vect('data_GIS/p_adm0_centroids.gpkg')
  
} else { # create it
  
  gadm_adm0 <- st_read('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>%
    rename(iso3 = GID_0) %>%
    filter(iso3 != "ALA") %>%
    filter(iso3 != "ATA") %>%
    left_join(cntryID[,c(1,3)])
  
  v_cntryGIS <- terra::simplifyGeom(vect(gadm_adm0))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  #terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg', overwrite = T)
}



indicDataFilled <- read_csv("results/swiidDataFilled.csv") %>% 
  drop_na() %>% 
  distinct(iso3, year, .keep_all = T) 


### 3. national data interpolation & extrapolation ----




source('functions/f_interpExtrap_gini.R')

interpExtrap_giniDisp <- f_interpExtrap_gini('gini_disp')
interpExtrap_giniMkt <- f_interpExtrap_gini('gini_mkt')



# then collect all data together
swiidDisp_extrap <- interpExtrap_giniDisp %>% 
  left_join(interpExtrap_giniMkt) %>% 
  select(iso3, year, gini_disp, gini_mkt) %>% 
  arrange(iso3, year)

write_csv(swiidDisp_extrap, 'results/gini_adm0_DispMkt_extrap.csv')

#swiidDisp_extrap <- read_csv('results/gini_adm0_DispMkt_extrap.csv')
