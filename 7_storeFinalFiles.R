

### store final files, round them

# code for subnational Gini dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(terra)
library(sf)

library(dplyr)
library(tidyverse)

library(tidyterra)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 1. read files -----

yearsIn <- 1990:2021


r_adm0 <- rast(paste0('results/rast_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 

r_adm1 <- rast(paste0('results/rast_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 

r_slope_gnic <- rast('results/rast_slope_adm1_perc_gnic.tif')
r_slope_gini <- rast('results/rast_slope_gini_disp_1990_2021.tif')

p_adm0 <- st_read(paste0('results/vect_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm0 <- vect(paste0('results/vect_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

p_adm1 <- st_read(paste0('results/vect_adm1_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm1 <- vect(paste0('results/vect_adm1_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))


#### 2. write rasters ----


# create folder if not exist

if (dir.exists('results_final/')) {
  
} else {
  dir.create('results_final/')  
}


terra::writeRaster(round(r_adm0,3),paste0('results_final/rast_adm0_gini_disp','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_adm1,3),paste0('results_final/rast_adm1_gini_disp','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


terra::writeRaster(round(r_slope_gnic,4),paste0('results_final/rast_slope_gni_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                          '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_slope_gini,4),paste0('results_final/rast_slope_gini_disp','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                          '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

#### 3. round and write polygons ----


v_gini_adm0 <- v_adm0 %>% 
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  select(-admID) 

v_gini_adm1 <- v_adm1 %>% 
  mutate(across(where(is.numeric), ~round(., 3)))




writeVector(v_gini_adm0,paste0('results_final/polyg_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)

writeVector(v_gini_adm1,paste0('results_final/polyg_adm1_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)



#### 4.  write tabulated results ----

t_adm0 <- as_tibble(v_gini_adm0)
t_adm1 <- as_tibble(v_gini_adm1)



write_csv(t_adm0, 'results_final/tabulated_adm0_gini_disp.csv')
write_csv(t_adm1, 'results_final/tabulated_adm1_gini_disp.csv')




#### 5. metadata

adm1_metaData <-read.csv('results/adm1_metaData_feb2024.csv')

write_csv(adm1_metaData, 'results_final/adm1_metaData.csv')

