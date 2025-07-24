

### interpolating and extrapolating national data - for WID data

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
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  
  distinct(cntry_code, iso3, .keep_all = T)

cntryID_reg <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso3, RegionID)


cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data))

GIS_layers_gini <- unique(cntry_metaData_gini$GIS_data)



##### 2 prepare for extrapolation ----

years_in = 1990:2023

# centroid of adm0

if (file.exists('data_GIS/p_adm0_centroids.gpkg')) {
  p_adm0_centroids <- vect('data_GIS/p_adm0_centroids.gpkg')
  
} else { # create it
  
  v_cntryGIS <- terra::simplifyGeom(vect('results/swiidDataFilled_gis.gpkg'))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg')
}



indicDataFilled_org <- read_csv("results/WID_nationalData.csv") %>% 
  drop_na() %>% 
  distinct(iso3, year, .keep_all = T) %>% 
  rename(gini_WID = gini)

indicDataFilled <- indicDataFilled_org %>%
  mutate(year = as.integer(year)) %>%  # ensure year is numeric
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    run_id = data.table::rleid(gini_WID),
    is_last_in_run = row_number() == max(row_number()),
    gini_WID = if_else(row_number() != max(row_number()) & run_id == lead(run_id), NA_real_, gini_WID)
  ) %>%
  ungroup() %>%
  select(-run_id, -is_last_in_run)  %>% # optional: clean up helpers
  drop_na() 




### 3. national data extrapolation ----




source('functions/f_interpExtrap_gini.R')

interpExtrap_giniWID <- f_interpExtrap_gini('gini_WID')


write_csv(interpExtrap_giniWID, 'results/gini_adm0_WID_extrap.csv')




### 4 fill missing adm0 areas fron swiid, scaling with a neigbour country


adm0_comb_interpExtrap_SWIID <- read_csv('results/gini_adm0_DispMkt_extrap.csv')
adm0_comb_interpExtrap_WID <- read_csv('results/gini_adm0_WID_extrap.csv')

adm0_SWIID <- adm0_comb_interpExtrap_SWIID %>% 
  select(iso3) %>% 
  distinct() %>% 
  left_join(adm0_comb_interpExtrap_WID %>% 
              select(iso3) %>% distinct() %>%  mutate(existWID = 1)) %>% 
  filter(is.na(existWID))

# for these use data from France

FRA_DOM_iso3 <- c('GLP','MTQ','GUF', 'REU','MYT')
FRA_DOM_name <- c('Guadeloupe','Martinique','French Guiana', 'Réunion','Mayotte')
cntryID_DOM <- cntryID %>% 
  filter(iso3 %in% FRA_DOM_iso3)

DOM_WID_Data <- interpExtrap_giniWID %>% 
  filter(iso3 %in% FRA_DOM_iso3) 

FRA_WID_Data <- interpExtrap_giniWID %>% 
  filter(iso3 == 'FRA') 


# Create a function to generate the data for each DOM
create_dom_data <- function(country, iso3, original_data) {
  data.frame(
    Country = rep(country, nrow(original_data)),
    iso3 = rep(iso3, nrow(original_data)),
    year = original_data$year,
    gini_WID = original_data$gini_WID
  )
}

# Define the DOMs and their ISO3 codes
doms <- list(
  Guadeloupe = "GLP",
  Martinique = "MTQ",
  French_Guiana = "GUF",
  Reunion = "REU",
  Mayotte = "MYT"
)

# Generate the data for each DOM and combine into one data frame
dom_data_list <- lapply(names(doms), function(dom) {
  create_dom_data(dom, doms[[dom]], FRA_WID_Data)
})

combined_dom_data_FRA <- do.call(rbind, dom_data_list) %>% 
  as_tibble() 






# missing adm0
adm0_WID_missing <- adm0_SWIID %>% 
  filter(!iso3 %in% FRA_DOM_iso3) 

adm0_WID_missing <- adm0_WID_missing$iso3

adm0_SWIID_scaling <- c('MLI', 'MAR', 'ZAF', 'SRB')

remove('estGiniWID_comb')


for (iIso3 in 1:length(adm0_SWIID_scaling)) {
  
  iso3_inBoth = adm0_SWIID_scaling[iIso3]
  iso3_missing = adm0_WID_missing[iIso3] 
  scalingData = adm0_comb_interpExtrap_SWIID
  
  # estimate missing data in SWIID with the help of WID
  
  iso3_temp <- iso3_inBoth
  WID_temp <- adm0_comb_interpExtrap_WID %>% 
    filter(iso3 == iso3_temp) %>% 
    #select(iso3, year, gini_disp) %>% 
    #pivot_wider(names_from = 'year', values_from = 'gini_mkt') %>% 
    mutate(year = as.character(year)) %>% 
    
    select(-iso3) 
  
  # SWIID_temp$year <- as.character(SWIID_temp$year )
  
  
  SWIID_temp <- scalingData %>% 
    filter(iso3 == iso3_temp) %>% 
    select(- iso3, -gini_mkt) %>% 
    mutate(year = as.character(year)) %>% 
    #pivot_longer(everything())%>% 
    rename(giniSWIID = gini_disp)
  
  comb_temp <- WID_temp %>% 
    left_join(SWIID_temp) %>% 
    drop_na()
  
  # SWIID data to be used for predictions
  iso3_pred <- iso3_missing
  SWIID_pred <- scalingData %>% 
    filter(iso3 == iso3_pred) %>% 
    mutate(year = as.character(year)) %>% 
    select(- iso3, -gini_mkt) %>% 
    rename(giniSWIID_pred = gini_disp) %>% 
    filter(year %in% comb_temp$year)  %>% 
    drop_na() %>% 
    left_join(comb_temp) %>% 
    mutate(ratio = gini_WID / giniSWIID) %>% 
    mutate(giniWID_est = ratio * giniSWIID_pred) 
  
  estGiniWID <- SWIID_pred %>% 
    mutate(iso3 = !!iso3_missing) %>% 
    select(iso3, year, giniWID_est) 
  
  
  
  if (exists('estGiniWID_comb')) {
    estGiniWID_comb <- estGiniWID_comb %>% 
      bind_rows(estGiniWID)
  } else {
    estGiniWID_comb <- estGiniWID
  }
  
}



adm0_comb_interpExtrap_WID_filled <- adm0_comb_interpExtrap_WID %>% 
  filter(!iso3 %in% unique(estGiniWID_comb$iso3)) %>% 
  filter(!iso3 %in% unique(combined_dom_data_FRA$iso3)) %>% 
  bind_rows(estGiniWID_comb %>% rename(gini_WID = giniWID_est) %>% mutate(year = as.numeric(year))) %>% 
  bind_rows(combined_dom_data_FRA  %>% mutate(year = as.numeric(year)) %>% select(-Country))



write_csv(adm0_comb_interpExtrap_WID_filled, 'results/gini_adm0_WID_extrap_filled.csv')

