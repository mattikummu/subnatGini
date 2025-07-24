

### interpolating and extrapolating data
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
v_cntryGIS <- vect('results/swiidDataFilled_gis.gpkg')

##### 2. calculate sub-national - country ratio using reported national values  ----

### 2.1 LIS

cntryListLIS <- cntry_metaData_gini %>% 
  filter(SUBNAT_USED == "LIS") %>% 
  select(GID_0,ID_0,NAME_0,Notes) 

id_cntryList <- unique(cntryListLIS$GID_0)

national_data_LIS <- read.xlsx('data_in/LIS_ERF_years_data.xlsx', sheet = 'LIS_data') %>% 
  as_tibble() %>% 
  rename(Subnat = adm1) %>% 
  mutate(iso3 = toupper(iso3)) %>% 
  left_join(cntryID,by='iso3') %>% 
  dplyr::filter(iso3 %in% cntryListLIS$GID_0) %>% 
  filter(Subnat == 'Total') %>% 
  arrange(iso3) %>% 
  select(-c(Country,region_c,cntry_code, iso2, X41, X42)) 



### 2.2 ERF

cntryListERF <- cntry_metaData_gini %>% 
  filter(SUBNAT_USED == "ERF") %>% 
  select(GID_0,ID_0,NAME_0,Notes) 

id_cntryList <- unique(cntryListERF$GID_0)

national_data_ERF <- read.xlsx('data_in/LIS_ERF_years_data.xlsx', sheet = 'ERF_data') %>% 
  as_tibble() %>% 
  rename(Subnat = adm1) %>% 
  mutate(iso3 = toupper(iso3)) %>% 
  left_join(cntryID,by='iso3') %>% 
  dplyr::filter(iso3 %in% cntryListERF$GID_0) %>% 
  filter(Subnat == 'Total') %>% 
  arrange(iso3) %>% 
  select(-c(Country,region_c,cntry_code, iso2)) 




### 2.3GDL

cntryListGDL <- cntry_metaData_gini %>% 
  filter(GIS_data == GIS_layers_gini[1]) %>% 
  select(GID_0,ID_0,NAME_0,Notes) 

id_cntryList <- unique(cntryListGDL$GID_0)

national_data_GDL <- read.xlsx('data_raw/GDL-Gini-coefficient-wealth-inequality-data_mod.xlsx') %>% 
  as_tibble() %>% 
  rename(iso3 = ISO_Code) %>% 
  rename(Subnat = Region) %>% 
  left_join(cntryID,by='iso3') %>% 
  dplyr::filter(iso3 %in% cntryListGDL$GID_0) %>% 
  arrange(iso3) %>% 
  select(-c(Continent,Country.x,Level,Country.y,cntry_code, iso2)) %>% 
  filter(Subnat == "Total")

names(national_data_GDL) <- c('iso3','RegID','Subnat',as.character(1991:2021))


### 2.4 other
years <- as.character(seq(1985,2021,1))

cntryListOTHER <- cntry_metaData_gini %>% 
  filter(grepl('OTHER|OECD', SUBNAT_USED) ) %>% 
  select(GID_0,ID_0,NAME_0) %>% 
  tibble::add_column(!!!set_names(as.list(rep(NA, length(years))),nm=years)) %>% 
  mutate(across(where(is.logical), as.numeric))

id_cntryList <- unique(cntryListOTHER$GID_0)

# write.xlsx(cntryListOTHER,"data_out/national_reported_data_OTHER.xlsx")

national_data_OTHER <- read.xlsx('data_out/national_reported_data_OTHER_data.xlsx') %>% 
  as_tibble() %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_0) %>% 
  left_join(cntryID,by='iso3') %>% 
  dplyr::filter(iso3 %in% cntryListOTHER$GID_0) %>% 
  arrange(iso3) %>% 
  select(-c(Country, cntry_code, iso2)) 


### 2.5 GSAP

subnat_gsap <- read.csv('results/sf_gsap_inc_wide.csv')
gsap_subnat_gis_combined <- subnat_gis_combined %>% 
  filter(iso3 %in% unique(subnat_gsap$iso3))

gsap_subnat_data_combined <- subnat_data_combined %>% 
  filter(iso3 %in% unique(subnat_gsap$iso3))

r_popCount <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')

ext_in_x_pop <- exactextractr::exact_extract(x= r_popCount,
                              y=gsap_subnat_gis_combined, 
                              fun='sum') %>% 
  as_tibble()

names(ext_in_x_pop) <- paste0('pop_',1990:2022) 

sf_gsap_pop <- gsap_subnat_gis_combined %>% 
  st_drop_geometry() %>%
  cbind(ext_in_x_pop)

pop_w_gini <- as.matrix(ext_in_x_pop[,1:32]) * as.matrix(gsap_subnat_data_combined[,8:39]) %>% 
  as_tibble() 

gsap_subnat_data_combined_pop_gini <- gsap_subnat_data_combined %>% 
  select(iso3, GID_nmbr) %>% 
  bind_cols(pop_w_gini) %>% 
  group_by(iso3) %>% 
  summarise_at(vars('1990':'2021'), sum, na.rm = TRUE) 

gsap_subnat_data_combined_pop <- gsap_subnat_data_combined %>% 
  select(iso3, GID_nmbr) %>% 
  bind_cols(ext_in_x_pop) %>% 
  group_by(iso3) %>% 
  summarise_at(vars('pop_1990':'pop_2021'), sum, na.rm = TRUE) 


temp_national_gini <- as.matrix(gsap_subnat_data_combined_pop_gini[,2:33]) / 
  as.matrix(gsap_subnat_data_combined_pop[,2:33]) %>% 
  as_tibble() 


national_data_GSAP <- gsap_subnat_data_combined  %>% 
  distinct(iso3) %>% 
  bind_cols(temp_national_gini) %>% 
  mutate(across(starts_with("pop_"), na_if, 0)) %>% 
  set_names('iso3', 1990:2021) #%>% 
  #pivot_longer(-iso3, names_to = 'year', values_to = 'gini_adm0')
  



#### 3. combine national reported data ----

national_data_combined <- national_data_LIS %>% 
  bind_rows(national_data_ERF) %>% 
  bind_rows(national_data_GDL) %>% 
  bind_rows(national_data_OTHER) %>%
  filter(!iso3 %in% unique(national_data_GSAP$iso3)) %>% 
  bind_rows(national_data_GSAP) %>% 
  select(-c(RegID, ID_0, Subnat)) %>% 
  setNames(c('iso3', paste0('n',seq(1985,2021,1))))


#### 4. calculate ratio between subnat and national ----


subnat_data_combined_nat <- subnat_data_combined %>% 
  setNames( c(names(subnat_data_combined)[1:6], paste0('s',seq(1989,2021,1)))) %>% 
  arrange(iso3, GID_nmbr) %>% 
  left_join(national_data_combined, by = "iso3") #%>% 
#mutate(across(starts_with('s'), ~ ./get(str_replace(cur_column(), 
#                                                          's', 'n')), .names = '{.col}_new'))


subnat_data_combined_ratio <- subnat_data_combined_nat[,1:6] %>% 
  bind_cols(subnat_data_combined_nat[,7:39] / subnat_data_combined_nat[,44:76]) 

write_csv(subnat_data_combined_ratio, 'results/subnat_data_combined_ratio_reportedOnly.csv')


#### 5. interpolate ------

subnat_data_combined_ratio <- read.csv('results/subnat_data_combined_ratio_reportedOnly.csv')

subnat_data_combined_ratio_interp <- subnat_data_combined_ratio %>% 
  as_tibble() %>% 
  select(-c(s1989, Country, Subnat, GID_1)) %>% 
  set_names('iso3', 'cntry_code', 'GID_nmbr', paste0(1990:2021)) %>% 
  mutate('2022' = NA) %>% mutate('2023' = NA) %>% 
  pivot_longer(-c('iso3', 'cntry_code', 'GID_nmbr'), names_to = 'year', values_to = 'ratio_gini') %>% 
  group_by(GID_nmbr) %>% 
  mutate(ratio_gini = na.approx(ratio_gini, rule = 2)) %>% 
  ungroup() 



write_csv(subnat_data_combined_ratio_interp, 'results/subnat_data_combined_ratio_interp.csv')

#st_write(subnat_gis_combined_filtered, 'results/subnat_data_combined_ratio_interp.gpkg')


