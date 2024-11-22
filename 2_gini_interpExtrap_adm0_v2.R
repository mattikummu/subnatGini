

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





### 3. national data extrapolation ----

source('functions/f_extrapol.R')

indicDataFilled <- read_csv("results/swiidDataFilled.csv")

nameIndic = 'gini_mkt'

# 
indicWider <- indicDataFilled %>% 
  select(iso3, year, !!as.name(nameIndic)) %>% 
  arrange(iso3, year) %>% 
  pivot_wider(names_from = 'year', values_from = 'gini_mkt') %>% 
  select(c(iso3, as.character(seq(1990,2021,1))))


# full timeseries
dataFull <- indicWider %>% 
  set_names(c('iso3', paste0('n',1990:2021))) %>% 
  drop_na()

# data where no data
data0 <- indicDataFilled %>% 
  select(iso3, year, !!nameIndic) %>% 
  rename(value := !!nameIndic) %>% 
  group_by(iso3) %>% 
  summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
  filter(value == 0)

# data where data for less than five years
dataLess5 <- indicDataFilled %>% 
  select(iso3, year, !!nameIndic) %>% 
  rename(value := !!nameIndic) %>% 
  group_by(iso3) %>% 
  summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
  filter(!value == 0) %>% 
  filter(value < 5)

# select data that is missing max three years from beginning and end of the time period (1990-2021)
dataNearlyFull <- indicWider %>% 
  set_names(c('iso3', paste0('n',1990:2021))) %>% 
  filter(!iso3 %in% dataFull$iso3) %>% 
  mutate(nearFullTimeser = ifelse(!is.na(n1993)&!is.na(n2018), 1, 0 )) %>% 
  filter(nearFullTimeser == 1) %>% 
  select(-nearFullTimeser)

# missing between dataLess5 and dataNearlyFull
dataNotFull <- indicWider %>% 
  filter(!iso3 %in% c(unique(dataFull$iso3), unique(dataLess5$iso3), unique(dataNearlyFull$iso3), unique(data0$iso3))) %>% 
  set_names(c('iso3', paste0('n',1990:2021))) 


# for countries with just few entries missing

if (exists('collectFilledData_nearlyFull')) {
  remove('collectFilledData_nearlyFull') # remove if exist
} else {
  # do nothing
}

if (nrow(dataNearlyFull) == 0) {
  dataFull_NearlyFull <- dataFull
} else{
  for (i in 1:nrow(dataNearlyFull)) { # 
    
    # extrapolate a country in question
    tempExtrapolCntry <- f_extrapol(dataAll = indicWider, 
                                       dataFull = dataFull, 
                                       dataToBeExtrap = dataNearlyFull, 
                                       iCntry = i)
    
    # collect to new data frame
    if (exists('collectFilledData_nearlyFull')) {
      collectFilledData_nearlyFull <- collectFilledData_nearlyFull %>% 
        bind_rows(tempExtrapolCntry)
    } else {
      collectFilledData_nearlyFull <- tempExtrapolCntry
    }
    
  }
  collectFilledData_nearlyFullWide <- collectFilledData_nearlyFull %>% 
    select(iso3, year, valueOrg_filled) %>% 
    pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
  
  dataFull_NearlyFull <- dataFull %>% 
    bind_rows(collectFilledData_nearlyFullWide )
}




# #### 4. for countries with less than 5 observations, we'll use trend from the closest country with full or nearly full data ----


if (exists('collectFilledData_less5')) {
  remove('collectFilledData_less5') # remove if exist
} else {
  # do nothing
}


if (nrow(dataLess5) == 0) {
  collectFilledData_less5Wide <- NULL
} else {
  for (i in 1:nrow(dataLess5)) {
    
    v_fullData <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% dataFull_NearlyFull$iso3)
    v_target <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% dataLess5[i,]$iso3)
    
    distTemp <- distance(v_target, v_fullData) %>% 
      as_tibble() 
    
    distTempCntry <- t(distTemp) %>% 
      bind_cols(as_tibble(v_fullData$GID_0))
    
    dataClosest <- distTempCntry[which(distTempCntry$...1 == min(distTempCntry$...1)),]
    
    refData <- dataFull_NearlyFull %>% 
      filter(iso3 == dataClosest$value) %>% 
      pivot_longer(-iso3, names_to = 'year', values_to = 'value') %>% 
      select(value)
    
    dataToBeExtrap <- indicWider  %>% 
      set_names(c('iso3', paste0('n',1990:2021))) %>% 
      filter(iso3 == dataLess5[i,]$iso3)
    
    filledData <- pivot_longer(dataToBeExtrap, -iso3, names_to = 'year', values_to = 'valueOrg') %>% 
      bind_cols(refData) %>% 
      # calculate ratio of the first and last non_NA value over modelled data
      #mutate(modDataSel = ifelse(is.na(gini_mkt), NA, value)) %>% 
      mutate(ratioFirst = first(na.omit(valueOrg / value))) %>% 
      mutate(ratioLast = last(na.omit(valueOrg / value))) %>% 
      #mutate(nrow = row_number()) %>% 
      # identify where first and last NAs are located
      mutate(valueTEMP = ifelse(is.na(valueOrg), 0, valueOrg )) %>% 
      mutate(isLeadingNA = cumsum(valueTEMP) == 0,
             isTrailingNA = rev(cumsum(rev(valueTEMP))) ==0 ) %>% 
      
      # mutate(#isTrailingNA_1 <- is.na(lead(valueOrg)) & is.na(valueOrg) & lag(is.na(lead(valueOrg)) & is.na(valueOrg)),
      #        isLeadingNA = is.na(lag(valueOrg, n=1:nrow)) & is.na(valueOrg),
      #        isTrailingNA = is.na(lead(valueOrg)) & is.na(valueOrg) & !isLeadingNA) %>%
      # fill NA values with the scaled value of the target data, based on the trend in modelled data
      mutate(valueOrg_filled = valueOrg, 
             valueOrg_filled = case_when(
               isLeadingNA ~ value * ratioFirst,
               isTrailingNA ~ value * ratioLast,
               TRUE ~ valueOrg_filled
             )) %>% 
      select(-c(ratioFirst, ratioLast, valueTEMP, isLeadingNA, isTrailingNA))
    
    
    
    # collect to new data frame
    if (exists('collectFilledData_less5')) {
      collectFilledData_less5 <- collectFilledData_less5 %>% 
        bind_rows(filledData)
    } else {
      collectFilledData_less5 <- filledData
    }
    
  }
  collectFilledData_less5Wide <- collectFilledData_less5 %>% 
    select(iso3, year, valueOrg_filled) %>% 
    pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
}


#### 5. then extrapolate the other countries, using the full and extrapolated nearlyFull data ----

if (exists('collectFilledData_rest')) {
  remove('collectFilledData_rest') # remove if exist
} else {
  # do nothing
}


for (i in 1:nrow(dataNotFull)) { # 
  
  # extrapolate a country in question
  tempExtrapolCntry <- f_extrapol(dataAll = indicWider, 
                                     dataFull = dataFull_NearlyFull, 
                                     dataToBeExtrap = dataNotFull, 
                                     iCntry = i)
  
  # collect to new data frame
  if (exists('collectFilledData_rest')) {
    collectFilledData_rest <- collectFilledData_rest %>% 
      bind_rows(tempExtrapolCntry)
  } else {
    collectFilledData_rest <- tempExtrapolCntry
  }
  
}
collectFilledData_restWide <- collectFilledData_rest %>% 
  select(iso3, year, valueOrg_filled) %>% 
  pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')



# put all together
dataFull_NearlyFull_Rest_Less5 <- dataFull_NearlyFull %>% 
  bind_rows(collectFilledData_restWide) %>% 
  bind_rows(collectFilledData_less5Wide) %>% 
  set_names('iso3', paste0(1990:2021)) %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = nameIndic)


write_csv(dataFull_NearlyFull_Rest_Less5, 'results/swiidAll_extrap_gini_mkt.csv')






#### 6. add to full dataset with also gini_disp ----

swiidAll_extrap <- read_csv('results/swiidAll_extrap_gini_mkt.csv') %>% 
  pivot_wider(names_from = 'year', values_from = 'gini_mkt') %>% 
  set_names(c('iso3', paste0(1990:2021))) %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gini_mkt') 

swiidDataFilled_mktExt <- indicDataFilled %>% 
  mutate(year = as.character(year)) %>% 
  filter(year > 1989) %>% 
  select(-c(Country,gini_mkt, gini_disp_se, gini_mkt_se)) %>% 
  pivot_wider(names_from = 'year', values_from = 'gini_disp') %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gini_disp') %>% 
  arrange(iso3, year) %>% 
  full_join(swiidAll_extrap) %>% 
  arrange(iso3, year)

# write to file
write_csv(swiidDataFilled_mktExt, 'results/swiidDataFilled_mktExt.csv')


#### 7. estimate gini_disp for missing values using gini_mkt -----

swiidDataFilled_mktExt <-  read_csv('results/swiidDataFilled_mktExt.csv')

# regression model to explain gini_disp with gini_mkt
lm_DispMkt <- swiidDataFilled_mktExt %>% 
  nest_by(iso3) %>%
  mutate(mod = list(lm(gini_disp ~ gini_mkt, data = data))) 

pm_DispMkt <- swiidDataFilled_mktExt %>% 
  nest_by(iso3) %>%
  mutate(mod = list(lm(gini_disp ~ gini_mkt, data = data))) 

lm_models_performance <- lm_DispMkt %>% 
  reframe(glance(mod))

lmModel_exist <- lm_models_performance %>% 
  drop_na()

lmModel_dont_exist <- lm_models_performance %>% 
  filter(!iso3 %in% lmModel_exist$iso3)


# test which degree of regression gives best result
# source: https://www.statology.org/polynomial-regression-r/
r.squar = matrix(data=NA,nrow=nrow(lmModel_exist),ncol=4)

for (j in 1:4){
  # j = 1
  lm_DispMkt <- swiidDataFilled_mktExt %>% 
    filter(iso3 %in% lmModel_exist$iso3) %>% 
    nest_by(iso3) %>%
    mutate(mod = list(lm(gini_disp ~ poly(gini_mkt,j), data = data))) 
  
  lm_models_performance <- lm_DispMkt %>% 
    reframe(glance(mod))
  
  r.squar[,j] = lm_models_performance$'r.squared'
}
r.squar <- r.squar %>% 
  as_tibble() %>% 
  bind_cols(lm_DispMkt$iso3)

# -> the third level polyn should be used; for many countries better than 1st or 2nd, but not much diff with 4th
# however, for some countries that did not produce reliable estimates... so 1st level was used

## to do

# 7.1. for those countries with only one entry, calculate ratio for that year and use it for all years

swiidOnlyOneEntry <- swiidDataFilled_mktExt %>% 
  filter(iso3 %in% lmModel_dont_exist$iso3) %>% 
  group_by(iso3) %>% 
  # same value for all years, as with gini_mkt
  mutate(gini_disp = mean(gini_disp, na.rm=T))




# 7.2. for other countries: 
# - decide which regression to be used 
# - collect all regressions
# - estimate disp based on those
# - extrapolate using the trend of the modelled timeseries

lm_DispMkt <- swiidDataFilled_mktExt %>% 
  filter(iso3 %in% lmModel_exist$iso3) %>% 
  nest_by(iso3) %>%
  mutate(mod = list(lm(gini_disp ~ poly(gini_mkt,1), data = data))) 


source('functions/f_Mkt2Disp.R')


# extrapolate gini_disp based on gini_mkt for those countries with more than one entry

swiidMoreThanOneEntry <- swiidDataFilled_mktExt %>% 
  filter(!iso3 %in% lmModel_dont_exist$iso3)

if (exists('collectFilledData_rest')) {
  remove('collectFilledData_rest') # remove if exist
} else {
  # do nothing
}



for (i in 1:length(unique(swiidMoreThanOneEntry$iso3))) { # 
  
  # extrapolate a country in question
  tempExtrapolCntry <- f_Mkt2Disp(dataFull = swiidMoreThanOneEntry, 
                                     iCntry = i)
  
  # collect to new data frame
  if (exists('collectFilledData_rest')) {
    collectFilledData_rest <- collectFilledData_rest %>% 
      bind_rows(tempExtrapolCntry)
  } else {
    collectFilledData_rest <- tempExtrapolCntry
  }
  
}

# then collect all data together
swiidDisp_extrap <- collectFilledData_rest %>% 
  select(-c(gini_disp, value)) %>% 
  rename(gini_disp = gini_disp_filled) %>% 
  select(iso3, year, gini_disp, gini_mkt) %>% 
  bind_rows(swiidOnlyOneEntry) %>% 
  arrange(iso3, year)

write_csv(swiidDisp_extrap, 'results/gini_adm0_DispMkt_extrap.csv')


