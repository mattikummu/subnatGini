
## prepare spatial data and rasterise the values


library(sf)
library(terra)

library(zoo)
library(purrr)
library(broom)
library(openxlsx) #
library(readxl)
library(mblm)

library(tidyverse)
library(dplyr) 

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 1. load  data ----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(cntry_code, iso2,  iso3, .keep_all = TRUE)

cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data))

GIS_layers_gini <- unique(cntry_metaData_gini$GIS_data)



adm1_gis_combined <- read_sf("results/gisData_gini_combined.gpkg")

adm1_data_combined <-  adm1_gis_combined %>% 
  st_drop_geometry()


adm0_gis <- st_read('results/swiidDataFilled_gis.gpkg')
v_adm0_gis <- vect('results/swiidDataFilled_gis.gpkg')



# adm0 and adm1 data

adm0_comb_interpExtrap <- read_csv('results/gini_adm0_DispMkt_extrap.csv')
adm1_ratioAdm1Adm0_interp <- read_csv('results/subnat_data_combined_ratio_interp.csv')



# check if any missing data in adm1 data
NAtemp <- adm1_ratioAdm1Adm0_interp %>% 
  drop_na()

adm1Missing <- adm1_ratioAdm1Adm0_interp %>% 
  filter(!GID_nmbr %in% NAtemp$GID_nmbr)


#### 2. create / load polygon data -----


if (file.exists('data_gis/gini_comb_Adm0Adm1.gpkg')){
  # load it
  adm0adm1_polyg <- read_sf('data_gis/gini_comb_Adm0Adm1.gpkg') 
} else { 
  # create it
  
  # check which countries are not in HDI_polyg
  
  adm1_data_uniqueISO3 <- unique(adm1_ratioAdm1Adm0_interp$iso3) %>% 
    as_tibble() %>% 
    set_names('iso3') %>% 
    mutate(iso3_Adm1data = iso3)
  
  adm1_polyg_check <- adm1_gis_combined %>% 
    st_drop_geometry() %>% 
    select(iso3) %>% 
    distinct(iso3)%>% 
    full_join(adm1_data_uniqueISO3) %>% 
    full_join(cntryID %>% mutate(iso3_adm0Data = iso3),by='iso3') 
  
  iso3_onlyInAdm0 <- adm1_polyg_check %>% 
    
    filter(is.na(iso3_Adm1data)) %>% 
    select(iso3) %>% 
    drop_na()
  
  
  
  adm0_polyg_sel <- adm0_gis %>% 
    # as_tibble() %>% 
    # st_drop_geometry() %>% 
    filter(iso3 %in% iso3_onlyInAdm0$iso3) %>% # join the missing countries in HDI polyg; so that only those are selected
    filter(iso3 != 'ALA' & iso3 != 'XCA') %>%  # remove Åland (part of Finland); Caspian Sea (not needed)
    mutate(gdlcode = paste0(iso3,'t')) %>% 
    select(iso3, cntry_code, geom) #%>% 
  #set_names(c('gdlcode', 'continent', 'iso_code', 'geometry'))
  
  # HDI_polyg_updated_check <- HDI_polyg_updated %>% 
  #   st_drop_geometry()
  # combine sHDI and GADM datasets
  adm1_polyg_comb <- adm1_gis_combined %>% 
    filter(GID_nmbr %in% unique(adm1_ratioAdm1Adm0_interp$GID_nmbr)) %>%  # choose only those regions we have data for
    select(iso3, cntry_code, GID_nmbr, geom)
    
  adm0adm1_polyg <- bind_rows(adm1_polyg_comb,adm0_polyg_sel)
  
  GDLids_cntry <- unique(adm1_ratioAdm1Adm0_interp$iso3) %>% 
    as_tibble() %>% 
    rename(iso3 = value) %>% 
    left_join(.,cntryID)
  
  
  # create unique identified for each admin area in HDI_GADM_polyg
  
  adm0adm1_polyg <-  adm0adm1_polyg %>% 
    #st_drop_geometry() %>% 
    #left_join(cntry_info[,c(2,4)]) %>% 
    mutate(admID = ifelse(is.na(GID_nmbr), cntry_code, GID_nmbr) ) %>% 
    select(-GID_nmbr)
             
           
  test_adm0adm1_polyg <-adm0adm1_polyg  %>% 
    st_drop_geometry()
  
  write_sf(adm0adm1_polyg,'data_gis/gini_comb_Adm0Adm1.gpkg')
}


# 3. create adm0, adm1 raster -----------------------------------------------------

if (file.exists('data_gis/gini_comb_Adm0Adm1_5arcmin.tif')){
  # load it
  adm0adm1_raster_5arcmin <- rast('data_gis/gini_comb_Adm0Adm1_5arcmin.tif')
  adm0_raster_5arcmin <- rast('data_gis/gini_Adm0_5arcmin.tif')
} else { 
  # create it
  
  #create ref raster
  ref_raster_5arcmin <- raster::raster(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- raster::raster(ncol=360*60, nrow=180*60)
  # rasterise to 1 arc min resolutions
  
  adm0adm1_raster_1arcmin <-  rast(fasterize::fasterize(adm0adm1_polyg,ref_raster_1arcmin,field="admID"))
  adm0_raster_1arcmin <- rast(fasterize::fasterize(adm0_gis,ref_raster_1arcmin,field="cntry_code"))
  
  # aggregate to 5 arc-min
  adm0adm1_raster_5arcmin <- terra::aggregate(adm0adm1_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  adm0_raster_5arcmin <- terra::aggregate(adm0_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  
  # write raster
  terra::writeRaster(adm0adm1_raster_5arcmin,'data_gis/gini_comb_Adm0Adm1_5arcmin.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
  terra::writeRaster(adm0_raster_5arcmin,'data_gis/gini_Adm0_5arcmin.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}




### 4. put data to raster -----------------------------------------------------
# 
# inYears = 1990:2021
# IndexName = 'gini_mkt'
# inDataAdm0 = adm0_comb_interpExtrap
# inDataAdm1 = adm1_ratioAdm1Adm0_interp

source('functions/f_gini_data2raster.R')

varNames <- 'gini_disp' #c('gini_mkt', 'gini_disp')

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_gini_data2raster(inYears = 1990:2021, 
                                       IndexName = varNames[iVar], 
                                       inDataAdm0 = adm0_comb_interpExtrap, 
                                       inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}



### 5. adm0 data to raster ----


source('functions/f_gini_adm0_data2raster.R')

varNames <- c('gini_mkt', 'gini_disp')

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_gini_adm0_data2raster(inYears = 1990:2021, 
                                         IndexName = varNames[iVar], 
                                         inDataAdm0 = adm0_comb_interpExtrap) 
  
}





### 6. simplify polygon layer ----

if (file.exists('data_gis/gini_comb_Adm0Adm1_siml.gpkg')){
  # load it
  adm0adm1_polyg_simp <- st_read('data_gis/gini_comb_Adm0Adm1_siml.gpkg') %>% 
    rename(admID = layer)
  adm0_polyg_simp <- st_read('data_gis/adm0_polyg_simp.gpkg') %>% 
    rename(admID = layer)
} else { 
  # create it
  
  adm0adm1_polyg_simp <- as.polygons(adm0adm1_raster_5arcmin) 
  writeVector(adm0adm1_polyg_simp, 'data_gis/gini_comb_Adm0Adm1_siml.gpkg', overwrite=T)
  
  adm0_polyg_simp <- as.polygons(adm0_raster_5arcmin) 
  writeVector(adm0_polyg_simp, 'data_gis/adm0_polyg_simp.gpkg', overwrite=T)
  
  adm0adm1_polyg_simp <- st_read('data_gis/gini_comb_Adm0Adm1_siml.gpkg') %>% 
    rename(admID = layer)
  adm0_polyg_simp <- st_read('data_gis/adm0_polyg_simp.gpkg') %>% 
    rename(admID = layer)
  
  
}



#### 7. put data to gpkg (and slope to raster) -----
# 
# inYears = 1990:2021
# IndexName = 'gini_mkt'
# inDataAdm0 = adm0_comb_interpExtrap
# inDataAdm1 = adm1_ratioAdm1Adm0_interp

source('functions/f_gini_data2gpkg.R')

varNames <- 'gini_disp'#c('gini_mkt', 'gini_disp')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_gini_data2gpkg(inYears = 1990:2021, 
                                        IndexName = varNames[iVar], 
                                        inDataAdm0 = adm0_comb_interpExtrap, 
                                        inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}



### 8. adm0 data to gkpg ------

source('functions/f_gini_adm0data2gpkg.R')


varNames <- c('gini_mkt', 'gini_disp')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_gini_adm0data2gpkg(inYears = 1990:2021, 
                                           IndexName = 'gini_disp', 
                                           inDataAdm0 = adm0_comb_interpExtrap) 
  
}



#### 9. calculate % change for gini and gnic-----

slope_gini <- rast("results/rast_slope_gini_disp_1990_2021.tif")
slope_gnic <- rast("../hdi_subnat/results/rast_slope_gnic_1990_2021.tif")

r_gini <- rast("results/rast_gini_disp_1990_2021.tif")
r_gnic <- rast("../hdi_subnat/results/rast_gnic_1990_2021.tif")

perChange_gini <- (slope_gini / subset(r_gini,32)) * 32
perChange_gnic <- (slope_gnic / log10(subset(r_gnic,32))) * 32

plot(perChange_gini)
plot(perChange_gnic)

terra::writeRaster(perChange_gini,paste0('results/rast_slope_perc_gini.tif'), 
                   gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(perChange_gnic,paste0('results/rast_slope_adm1_perc_gnic.tif'), 
                   gdal="COMPRESS=LZW",overwrite=TRUE)
