
## prepare spatial data and rasterise the values


library(sf)
library(terra)

library(zoo)
library(purrr)
library(broom)
library(openxlsx) #
library(readxl)
library(mblm)

library(qgisprocess)
library(tidyterra)

library(tidyverse)
library(dplyr) 

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 1. load  data ----

timesteps <- seq(1990,2023)

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

adm0_comb_interpExtrap_SWIID <- read_csv('results/gini_adm0_DispMkt_extrap.csv')
adm0_comb_interpExtrap_WID <- read_csv('results/gini_adm0_WID_extrap_filled.csv')

adm0_comb_interpExtrap <- adm0_comb_interpExtrap_SWIID %>% 
  left_join(adm0_comb_interpExtrap_WID)

# test that data for both exist

temp <- adm0_comb_interpExtrap %>% 
  filter(is.na(gini_WID))

# # for missing countries in WID, let's use SWIID
# 
# adm0_comb_interpExtrap <- adm0_comb_interpExtrap_comb %>% 
#   mutate(gini_WID = ifelse(is.na(gini_WID), gini_disp, gini_WID))

adm1_ratioAdm1Adm0_interp <- read_csv('results/subnat_data_combined_ratio_interp.csv')



# check if any missing data in adm1 data
NAtemp <- adm1_ratioAdm1Adm0_interp %>% 
  drop_na()

adm1Missing <- adm1_ratioAdm1Adm0_interp %>% 
  filter(!GID_nmbr %in% NAtemp$GID_nmbr)


#### 2. create / load polygon data -----


if (file.exists('data_gis/gini_comb_Adm0Adm1_simpl.gpkg')){
  # load it
  sf_adm0adm1_polyg_topo <- read_sf('data_gis/gini_comb_Adm0Adm1_simpl.gpkg') 
  sf_adm0_polyg_topo <- read_sf('data_gis/gini_Adm0_simpl.gpkg') 
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
  
  #test <- adm0adm1_polyg %>% st_drop_geometry()
  
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
  
  write_sf(adm0adm1_polyg,'data_gis/gini_comb_Adm0Adm1.gpkg')
  ## simplify polygon
  
  
  
  # adm0adm1_polyg <- st_make_valid(adm0adm1_polyg)
  adm0adm1_polyg_antimer <- adm0adm1_polyg %>% 
    st_break_antimeridian(lon_0 = 0) 
  
  
  
  qgis_configure()
  
  result <- qgis_run_algorithm(
    "native:fixgeometries",
    INPUT = adm0adm1_polyg_antimer,
    OUTPUT = qgis_tmp_vector()  # creates a temporary output
  )
  
  # Read the result back into R
  fixed_sf <- st_read(result$OUTPUT) #%>% 
  #st_make_valid()
  
  #write_sf(fixed_sf,'data_gis/gini_comb_Adm0Adm1_fixed.gpkg')
  
  
  geojson_file <- "data_out/v_adm0adm1_polyg.geojson"
  sf::st_write(fixed_sf, geojson_file, driver = "GeoJSON", delete_dsn = TRUE)
  
  topoj_file <- "data_out/v_adm0adm1_polyg.topojson"
  topoj_file_simp <- "data_out/v_adm0adm1_polyg_simpl.topojson"
  
  
  command <- paste0(
    "node --max-old-space-size=8192 ",
    "\"", Sys.which("mapshaper"), "\" ",
    geojson_file,
    " -o format=topojson ",
    topoj_file
  )
  
  # Print the command to see what's being executed (useful for debugging)
  print(command)
  system(command)
  
  # Optional: You can add -wrap to the simplification step too,
  # but usually, fixing it during the initial GeoJSON to TopoJSON conversion is enough.
  command_simpl <- paste0(
    "node --max-old-space-size=8192 ", 
    "$(which mapshaper) ", 
    topoj_file, 
    " -clean -simplify 5% keep-shapes -o ", 
    topoj_file_simp
  )
  
  print(command_simpl)
  system(command_simpl)
  
  sf_adm0adm1_polyg_topo <- geojsonio::topojson_read(topoj_file_simp)
  
  
  #plot(sf_adm0adm1_polyg_topo)
  # 
  # size_mb <- object.size(sf_adm0adm1_polyg_topo) / (1024^2)
  # print(paste("Size in MB:", round(size_mb, 2)))
  
  #sf_adm0adm1_polyg_topo <- sf::st_make_valid(sf_adm0adm1_polyg_topo) 
  #sf::st_is_valid(sf_adm0adm1_polyg_topo)
  
  sf_adm0adm1_polyg_topo <- st_set_crs(sf_adm0adm1_polyg_topo, st_crs(adm0adm1_polyg)) %>% 
    st_make_valid()
  # 
  
  v_adm0adm1_polyg_topo <- vect(sf_adm0adm1_polyg_topo) %>% 
    makeValid()
  
  writeVector(v_adm0adm1_polyg_topo,'data_gis/gini_comb_Adm0Adm1_simpl.gpkg', overwrite=TRUE)
  #write_sf(adm0adm1_polyg,'data_gis/gini_comb_Adm0Adm1.gpkg')
  
  v_adm0_polyg_topo <- terra::aggregate(v_adm0adm1_polyg_topo, by = 'cntry_code') %>% 
    select(iso3, cntry_code)
  
  writeVector(v_adm0_polyg_topo,'data_gis/gini_Adm0_simpl.gpkg', overwrite=TRUE)
  
  sf_adm0adm1_polyg_topo <- read_sf('data_gis/gini_comb_Adm0Adm1_simpl.gpkg') 
  sf_adm0_polyg_topo <- read_sf('data_gis/gini_Adm0_simpl.gpkg') 
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
  
  
  adm0adm1_raster_1arcmin <-  rast(fasterize::fasterize(sf_adm0adm1_polyg_topo,ref_raster_1arcmin,field="admID"))
  adm0_raster_1arcmin <- rast(fasterize::fasterize(sf_adm0_polyg_topo,ref_raster_1arcmin,field="cntry_code"))
  
  # aggregate to 5 arc-min
  adm0adm1_raster_5arcmin <- terra::aggregate(adm0adm1_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  adm0_raster_5arcmin <- terra::aggregate(adm0_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  
  # write raster
  terra::writeRaster(adm0adm1_raster_5arcmin,'data_gis/gini_comb_Adm0Adm1_5arcmin.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
  terra::writeRaster(adm0_raster_5arcmin,'data_gis/gini_Adm0_5arcmin.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}




### 4. adm0 data to raster ----


source('functions/f_gini_adm0_data2raster.R')

varNames <- c('gini_disp', 'gini_WID')

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_gini_adm0_data2raster(inYears = timesteps, 
                                          IndexName = varNames[iVar], 
                                          inDataAdm0 = adm0_comb_interpExtrap) 
  
}

### 5. put data to raster -----------------------------------------------------
# 
# inYears = 1990:2021
# IndexName = 'gini_mkt'
# inDataAdm0 = adm0_comb_interpExtrap
# inDataAdm1 = adm1_ratioAdm1Adm0_interp

source('functions/f_gini_data2raster.R')

varNames <- c('gini_disp', 'gini_WID')  #c('gini_mkt', 'gini_disp')

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_gini_data2raster(inYears = timesteps, 
                                     IndexName = varNames[iVar], 
                                     inDataAdm0 = adm0_comb_interpExtrap, 
                                     inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}

# 
# ### there are some areas where no adm1 data (Krimi, areas between China and India, ec). 
# ## let's use adm0 data for those
# 
# r_gini_adm0 <- rast('results/rast_adm0_gini_disp_1990_2021.tif')
# r_gini_adm1 <- rast('results/rast_gini_disp_1990_2021.tif')
# 
# r_gini_adm1[is.na(r_gini_adm1)] = r_gini_adm0
# 
# terra::writeRaster(r_gini_adm1,paste0('results/rast_gini_disp_1990_2021.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


### 6. simplify polygon layer ----

# if (file.exists('data_gis/gini_comb_Adm0Adm1_simpl.gpkg')){
# load it
adm0adm1_polyg_simp <- st_read('data_gis/gini_comb_Adm0Adm1_simpl.gpkg') 
adm0_polyg_simp <- st_read('data_gis/gini_Adm0_simpl.gpkg') %>% 
  mutate(admID = cntry_code)
# } else { 
#   # create it
#   
#   # adm0adm1_polyg_simp <- as.polygons(adm0adm1_raster_5arcmin) 
#   # writeVector(adm0adm1_polyg_simp, 'data_gis/gini_comb_Adm0Adm1_siml.gpkg', overwrite=T)
#   # 
#   adm0_polyg_simp <- as.polygons(adm0_raster_5arcmin) 
#   writeVector(adm0_polyg_simp, 'data_gis/adm0_polyg_simp.gpkg', overwrite=T)
#   
#   # adm0adm1_polyg_simp <- st_read('data_gis/gini_comb_Adm0Adm1_siml.gpkg') %>% 
#   #   rename(admID = layer)
#   adm0_polyg_simp <- st_read('data_gis/adm0_polyg_simp.gpkg') %>% 
#     rename(admID = layer)
#   
#   
# }



#### 7. put data to gpkg (and slope to raster) -----
# 
# inYears = 1990:2021
# IndexName = 'gini_mkt'
# inDataAdm0 = adm0_comb_interpExtrap
# inDataAdm1 = adm1_ratioAdm1Adm0_interp

source('functions/f_gini_data2gpkg.R')

varNames <- c('gini_disp', 'gini_WID') #c('gini_mkt', 'gini_disp')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_gini_data2gpkg(inYears = timesteps, 
                                   IndexName = varNames[iVar], 
                                   inDataAdm0 = adm0_comb_interpExtrap, 
                                   inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}





### 8. adm0 data to gkpg ------

source('functions/f_gini_adm0data2gpkg.R')


varNames <- c('gini_disp', 'gini_WID')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_gini_adm0data2gpkg(inYears = timesteps, 
                                       IndexName = varNames[iVar], 
                                       inDataAdm0 = adm0_comb_interpExtrap) 
  
}



#### 9. calculate % change for gini and gnic-----

slope_gini <- rast("results/rast_slope_gini_disp_1990_2023.tif")
# please change folder path to ../subnatGNI/
slope_gnic <- rast("../subnatGNI/results/rast_slope_log10gnic_1990_2023.tif")

r_gini <- rast("results/rast_gini_disp_1990_2023.tif")
# please change folder path to ../subnatGNI/
r_gnic <- rast("../subnatGNI/results/rast_gnic_1990_2023.tif")

perChange_gini <- (slope_gini / subset(r_gini,34)) * 34
perChange_gnic <- (slope_gnic / log10(subset(r_gnic,34))) * 34

plot(perChange_gini)
plot(perChange_gnic)

terra::writeRaster(perChange_gini,paste0('results/rast_slope_perc_gini.tif'), 
                   gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(perChange_gnic,paste0('results/rast_slope_adm1_perc_gnic.tif'), 
                   gdal="COMPRESS=LZW",overwrite=TRUE)
