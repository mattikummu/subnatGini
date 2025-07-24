

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

yearsIn <- 1990:2023


cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(cntry_code, iso2,  iso3, .keep_all = TRUE)


t_adminNames_adm1 <- read_csv('results/subnat_gis_combined.csv') %>% 
  rename(admID=GID_nmbr) %>% 
  select(Country, admID, Subnat)

r_adm0 <- rast(paste0('results/rast_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                      '.tif') ) 

r_adm1 <- rast(paste0('results/rast_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                      '.tif') ) 


r_adm0_WID <- rast(paste0('results/rast_adm0_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                      '.tif') ) 
r_adm1_WID <- rast(paste0('results/rast_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                      '.tif') ) 

r_slope_gnic <- rast('results/rast_slope_adm1_perc_gnic.tif')
r_slope_gini <- rast('results/rast_slope_gini_disp_1990_2023.tif')

r_slope_gini_WID <- rast('results/rast_slope_gini_WID_1990_2023.tif')

p_adm0 <- st_read(paste0('results/vect_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm0 <- vect(paste0('results/vect_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

p_adm1 <- st_read(paste0('results/vect_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm1 <- vect(paste0('results/vect_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))


p_adm0_WID <- st_read(paste0('results/vect_adm0_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm0_WID <- vect(paste0('results/vect_adm0_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

p_adm1_WID <- st_read(paste0('results/vect_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm1_WID <- vect(paste0('results/vect_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

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

terra::writeRaster(round(r_adm0_WID,3),paste0('results_final/rast_adm0_gini_WID','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                          '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_adm1_WID,3),paste0('results_final/rast_adm1_gini_WID','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                          '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


terra::writeRaster(round(r_slope_gnic,4),paste0('results_final/rast_slope_gni_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                                '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_slope_gini,4),paste0('results_final/rast_slope_gini_disp','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                                '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


terra::writeRaster(round(r_slope_gini_WID,4),paste0('results_final/rast_slope_gini_WID','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                                '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

#### 3. round and write polygons ----


v_gini_adm0 <- v_adm0 %>% 
  mutate(across(all_of(as.character(yearsIn)), ~ round(., 3))) %>% 
  mutate(across(all_of('slope'), ~ round(., 6))) %>% 
  #mutate(across(where(is.numeric), ~ ifelse(cur_column() == "slope", round(., 5), round(., 3)))) %>% 
  select(-admID) %>% 
  left_join(cntryID %>% select(-iso2, -iso3) ) %>% 
  select(iso3, Country, cntry_code, everything())%>% 
  select(-estimate, -p.value)

v_gini_adm1 <- v_adm1 %>% 
  #mutate(across(where(is.numeric), ~round(., 3))) %>% 
  mutate(across(all_of(as.character(yearsIn)), ~ round(., 3))) %>% 
  mutate(across(all_of('slope'), ~ round(., 6))) %>% 
  left_join(t_adminNames_adm1) %>% 
  left_join(cntryID %>% select(-iso2, -iso3) %>% rename(CountryName = Country) %>% rename(admID = cntry_code)) %>% 
  mutate(Country = ifelse(is.na(Country), CountryName, Country)) %>% 
  select(-CountryName) %>% 
  left_join(v_gini_adm0 %>% as_tibble() %>% select(iso3, cntry_code)) %>% 
  select(iso3, Country,cntry_code, Subnat, admID, everything()) %>% 
  select(-estimate, -p.value) #%>% 
  #filter(!is.na(cntry_code))


v_gini_adm0_WID <- v_adm0_WID %>% 
  mutate(across(all_of(as.character(yearsIn)), ~ round(., 3))) %>% 
  mutate(across(all_of('slope'), ~ round(., 6))) %>% 
  #mutate(across(where(is.numeric), ~ ifelse(cur_column() == "slope", round(., 5), round(., 3)))) %>% 
  select(-admID) %>% 
  left_join(cntryID %>% select(-iso2, -iso3) ) %>% 
  select(iso3, Country, cntry_code, everything())%>% 
  select(-estimate, -p.value)

v_gini_adm1_WID <- v_adm1_WID %>% 
  #mutate(across(where(is.numeric), ~round(., 3))) %>% 
  mutate(across(all_of(as.character(yearsIn)), ~ round(., 3))) %>% 
  mutate(across(all_of('slope'), ~ round(., 6))) %>% 
  left_join(t_adminNames_adm1) %>% 
  left_join(cntryID %>% select(-iso2, -iso3) %>% rename(CountryName = Country) %>% rename(admID = cntry_code)) %>% 
  mutate(Country = ifelse(is.na(Country), CountryName, Country)) %>% 
  select(-CountryName) %>% 
  left_join(v_gini_adm0 %>% as_tibble() %>% select(iso3, cntry_code)) %>% 
  select(iso3, Country,cntry_code, Subnat, admID, everything()) %>% 
  select(-estimate, -p.value) #%>% 
#filter(!is.na(cntry_code))




writeVector(v_gini_adm0,paste0('results_final/polyg_adm0_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)

writeVector(v_gini_adm1,paste0('results_final/polyg_adm1_gini_disp_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)




writeVector(v_gini_adm0_WID,paste0('results_final/polyg_adm0_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)

writeVector(v_gini_adm1_WID,paste0('results_final/polyg_adm1_gini_WID_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)



#### 4.  write tabulated results ----

t_adm0 <- as_tibble(v_gini_adm0)
t_adm1 <- as_tibble(v_gini_adm1)


t_adm0_WID <- as_tibble(v_gini_adm0_WID)
t_adm1_WID <- as_tibble(v_gini_adm1_WID)

temp <- t_adm1 %>% filter(is.na(Subnat))


write_csv(t_adm0, 'results_final/tabulated_adm0_gini_disp.csv')
write_csv(t_adm1, 'results_final/tabulated_adm1_gini_disp.csv')

write_csv(t_adm0_WID, 'results_final/tabulated_adm0_gini_WID.csv')
write_csv(t_adm1_WID, 'results_final/tabulated_adm1_gini_WID.csv')




#### 5. metadata

adm1_metaData <-read.csv('results/adm1_metaData_feb2024.csv')

write_csv(adm1_metaData, 'results_final/adm1_metaData.csv')

