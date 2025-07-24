### processing subnational gini data
library(sf)
library(terra)
library(Rfast)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. load general data -----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data))


#### 2. process adm 1 level Gini data ----

# general

GIS_layers_gini <- unique(cntry_metaData_gini$GIS_data)


### 2.1 GDL ----

cntryList <- cntry_metaData_gini %>% 
  filter(GIS_data == "GDL") %>% 
  select(GID_0,ID_0,NAME_0,Notes) 

id_cntryList <- unique(cntryList$GID_0)


# read data

subnat_data <- read.xlsx('data_raw/GDL-Gini-coefficient-wealth-inequality-data_modV2.xlsx') %>% 
  as_tibble() %>% 
  rename(iso3 = ISO_Code) %>% 
  rename(Subnat = Region) %>% 
  left_join(cntryID,by='iso3') %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  arrange(iso3) %>% 
  select(-c(Continent,Country.x,Level,Country.y,cntry_code, iso2))

names(subnat_data) <- c('iso3','RegID','Subnat',as.character(1991:2021))

id_subnat <- unique(subnat_data$iso3)

gis_data <- read_sf( 'data_gis/GDL_world.gpkg' ) %>% 
  # st_drop_geometry() %>% 
  #select(ISO,CNTRYNAMEE,DHSREGEN,REG_ID) %>% 
  rename(RegID = gdlcode) %>% 
  select(-continent) %>% 
  rename(iso3 = iso_code) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,RegID) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  arrange(iso3)


## crop GDL Morocco subnat based on Morocco on GADM
# i.e. remove Western Sahara part of the 'South' admin area of MAR

gis_data_MAR <- gis_data %>% 
  filter(iso3=='MAR') 

gis_nationalData_MAR <- st_read('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>% 
  rename(iso3 = GID_0) %>% 
  filter(iso3 == 'MAR')

gis_data_MAR_cropped <- st_intersection(gis_data_MAR, gis_nationalData_MAR) %>% 
  select(c(Country, iso3, cntry_code, RegID))

#plot(gis_data_MAR_cropped)

gis_data_corr <- gis_data %>% 
  filter(!iso3 == 'MAR') %>% 
  bind_rows(gis_data_MAR_cropped)


id_gis <- unique(gis_data_corr$iso3)



# diffobj::diffChr(id_subnat,id_gis)

# some small island countries missing from GDL shp; we need to ignore those


subnat_gis_data <- gis_data_corr %>% 
  left_join(subnat_data,by=c('RegID','iso3')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,Subnat) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_1 = paste0(iso3,".",rowNmbr,"_1")) %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr)%>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,as.character(1991:2021))

subnat_gis_data_GDL <- subnat_gis_data
subnat_gis_data_GDL_noGeom <- subnat_gis_data_GDL %>% 
  st_drop_geometry()
subnat_data_GDL <- subnat_data


#### 2.2 GADM ----

cntryList <- cntry_metaData_gini %>% 
  filter(GIS_data == "GADM") %>% 
  select(GID_0,ID_0,NAME_0,Notes) 


# read data 

subnat_data <- read.xlsx('data_in/gadm_level1_meta.xlsx', sheet='gadm_level1_meta', startRow = 1) %>% 
  as_tibble() %>% 
  rename(iso3=GID_0) %>% 
  # select(-GID_1) %>% 
  # rename(GID_1 = GID_1_combined) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  rename(Country = NAME_0) %>% 
  select('iso3','GID_1','Subnat',as.character(1988:2021)) %>% 
  arrange(iso3)

# all data columns to numeric
cols.num <- c(as.character(1988:2021))
subnat_data[cols.num] <- sapply(subnat_data[cols.num],as.numeric)

id_subnat <- unique(subnat_data$iso3)

gis_data <- read_sf( '/Users/mkummu/R/migration_data_bee/data_in/gadm_lev1.gpkg' ) %>% 
  #st_drop_geometry() %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,GID_1) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  arrange(iso3)

id_gis <- unique(gis_data$iso3)

# diffobj::diffChr(id_subnat,id_gis)


subnat_gis_data <- gis_data %>% 
  left_join(subnat_data,by=c('iso3','GID_1')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,as.character(1988:2021))

subnat_gis_data_GADM <- subnat_gis_data
subnat_data_GADM <- subnat_data



### 2.3 EUROSTAT NUTS1 ----


cntryList <- cntry_metaData_gini %>% 
  filter(GIS_data %in% "NUTS1")%>% 
  select(GID_0,ID_0,NAME_0,Notes) 

subnat_data <- read.xlsx('data_in/nuts1tableData.xlsx') %>% 
  as_tibble() %>% 
  rename(iso2=CNTR_CODE) %>% 
  # select(-GID_1) %>% 
  # rename(GID_1 = GID_1_combined) %>% 
  mutate(iso2 = ifelse(iso2 == "UK", "GB", iso2)) %>% 
  left_join(cntryID) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  select('tl3_id',as.character(1988:2021)) 

subnat_gis_data <-  read_sf("data_gis/nuts1_polyg.gpkg") %>% 
  #st_drop_geometry() %>% 
  rename(iso2 = CNTR_CODE) %>% 
  mutate(iso2 = ifelse(iso2 == "UK", "GB", iso2)) %>% 
  left_join(cntryID,by='iso2') %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  left_join(subnat_data, by=c("NUTS_ID" = "tl3_id")) %>% 
  rename(GID_1 = NUTS_ID) %>% 
  rename(Subnat = NAME_LATN) %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,everything()) %>% 
  select(-iso2)

subnat_gis_data_NUTS1 <- subnat_gis_data

subnat_gis_data_NUTS1_noGeom <- subnat_gis_data %>% 
  st_drop_geometry()
subnat_data_NUTS1 <- subnat_data






### 2.4 EUROSTAT NUTS2 ----


cntryList <- cntry_metaData_gini %>% 
  filter(GIS_data %in% "NUTS2")%>% 
  select(GID_0,ID_0,NAME_0,Notes) 

subnat_data <- read.xlsx('data_in/nuts2tableData.xlsx', sheet='nuts2table', startRow = 1) %>% 
  as_tibble() %>% 
  rename(iso2=CNTR_CODE) %>% 
  # select(-GID_1) %>% 
  # rename(GID_1 = GID_1_combined) %>% 
  
  left_join(cntryID) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  select('tl2_id',as.character(1988:2021)) 

subnat_gis_data <-  read_sf('data_gis/nuts2_corGeom.gpkg') %>% 
  #st_drop_geometry() %>% 
  select(NUTS0,NUTS, NAME) %>% 
  rename(iso2 = NUTS0) %>% 
  left_join(cntryID,by='iso2') %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  left_join(subnat_data, by=c("NUTS" = "tl2_id")) %>% 
  rename(GID_1 = NUTS) %>% 
  rename(Subnat = NAME) %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,everything()) %>% 
  select(-iso2)

subnat_gis_data_NUTS2 <- subnat_gis_data

subnat_gis_data_NUTS2_noGeom <- subnat_gis_data %>% 
  st_drop_geometry()
subnat_data_NUTS2 <- subnat_data




### 2.5 EUROSTAT NUTS3 ----


cntryList <- cntry_metaData_gini %>% 
  filter(GIS_data %in% "NUTS3")%>% 
  select(GID_0,ID_0,NAME_0,Notes) 

subnat_data <- read.xlsx('data_in/nuts3tableData.xlsx', sheet='nuts3table', startRow = 1) %>% 
  as_tibble() %>% 
  rename(iso2=CNTR_CODE) %>% 
  # select(-GID_1) %>% 
  # rename(GID_1 = GID_1_combined) %>% 
  
  left_join(cntryID) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  select('tl3_id',as.character(1988:2021)) 

subnat_gis_data <-  read_sf('data_gis/nuts3_polyg.gpkg') %>% 
  # st_drop_geometry() %>% 
  select(CNTR_CODE,tl3_id, NAME_LATN) %>% 
  rename(iso2 = CNTR_CODE) %>% 
  left_join(cntryID,by='iso2') %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  left_join(subnat_data, by=c("tl3_id" = "tl3_id")) %>% 
  rename(GID_1 = tl3_id) %>% 
  rename(Subnat = NAME_LATN) %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,everything()) %>% 
  select(-iso2)

subnat_gis_data_NUTS3 <- subnat_gis_data
subnat_data_NUTS3 <- subnat_data


### 2.6 OECD ----

# no data at the end from OECD
# 
# cntryList <- cntry_metaData_gini %>% 
#   filter(GIS_data == GIS_layers_gini[7]) %>% 
#   select(GID_0,ID_0,NAME_0,Notes) 
# 
# id_cntryList <- unique(cntryList$GID_0)
# 
# # read data
# 
# subnat_data <- readr::read_csv('data_raw/oecd_gini_regional.csv') %>% 
#   rename(Subnat = Regions) %>% 
#   filter(Indicator == "Gini before taxes and transfers") %>% 
#   filter(Measure == "Value") %>% 
#   select(c(REG_ID, Subnat, TIME, Value)) %>% 
#   mutate(iso2 = substr(REG_ID,1,2) ) %>% 
#   left_join(cntryID,by='iso2') %>% 
#   dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
#   arrange(TIME) %>% 
#   pivot_wider( names_from = TIME, values_from = Value) %>% 
#   select(-c(iso2)) %>% 
#   arrange(REG_ID) %>% 
#   rename(tl2_id = REG_ID) 
# 
# #names(subnat_data) <- c('tl2_id','iso3',as.character(2001:2019))
# 
# id_subnat <- unique(subnat_data$iso3)
# 
# gis_data <- read_sf( 'data_gis/oecd_tl2.gpkg' ) %>% 
#   # st_drop_geometry() %>% 
#   select(tl2_id,iso3,name_en) %>% 
#   rename(Subnat = name_en) %>% 
#   left_join(cntryID,by='iso3') %>% 
#   select(iso3,tl2_id) %>% 
#   dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
#   arrange(iso3)
# 
# id_gis <- unique(gis_data$iso3)
# 
# compare::compare(id_subnat,id_gis)
# 
# subnat_gis_data <- gis_data %>% 
#   left_join(subnat_data,by=c('tl2_id','iso3')) %>% 
#   #st_drop_geometry() %>% 
#   arrange(iso3,Subnat) %>% 
#   group_by(iso3) %>% 
#   mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
#   ungroup() %>% 
#   mutate(GID_1 = paste0(iso3,".",rowNmbr,"_1")) %>% 
#   mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
#   select(-rowNmbr)%>% 
#   select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,as.character(c(2010, 2011, 2014)))
# 
# subnat_gis_data_OECD <- subnat_gis_data
# subnat_data_OECD <- subnat_data


# write.xlsx(subnat_data_OECD,"data_raw/oecd_wide.xlsx")


### 2.7 individual countries ----

cntryList <- cntry_metaData_gini %>% 
  filter(GIS_data %in% "OTHER") %>% 
  select(GID_0,ID_0,NAME_0,Notes) 

# read data

subnat_data <- read.xlsx('data_in/otherGIS_data.xlsx', sheet="other_GIS", startRow = 1) %>% 
  as_tibble() %>% 
  rename(iso3=GID_0) %>% 
  #select(-GID_1)  %>% 
  #rename(GID_1 = GID_1_combined) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  rename(Country = COUNTRY)  %>% 
  select('iso3','GID_2','Subnat',as.character(1988:2021)) %>% 
  arrange(iso3,GID_2) %>% 
  rename(subnatID = GID_2)


temp_gis_CRI <- read_sf( 'data_gis/costa_rica_regions_diss.gpkg') %>% 
  select(GID_0,COUNTRY,NL_NAME_1, CC_2) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = CC_2) %>% 
  rename(Subnat = NL_NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID)

temp_gis_DOM <- read_sf( 'data_gis/dom_rep_regions.gpkg') %>% 
  select(GID_0,COUNTRY,NL_NAME_1, CC_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = CC_1)%>% 
  rename(Subnat = NL_NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID)

temp_gis_JPN <- read_sf( 'data_gis/jpn_regions.gpkg') %>% 
  select(GID_0,COUNTRY,NL_NAME_1, CC_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = CC_1)%>% 
  rename(Subnat = NL_NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID)

temp_gis_PER<- read_sf( 'data_gis/per_regions.gpkg') %>% 
  select(GID_0,COUNTRY,NL_NAME_1, CC_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = CC_1)%>% 
  rename(Subnat = NL_NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID)

temp_gis_TWN <- read_sf( 'data_gis/twn_gadm2.gpkg') %>% 
  select(GID_0,COUNTRY,NAME_2, GID_2) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = GID_2)%>% 
  rename(Subnat = NAME_2) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID)

temp_gis_VNM <- read_sf( 'data_gis/vnm_regions.gpkg') %>% 
  select(GID_0,COUNTRY,NL_NAME_1, CC_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = CC_1)%>% 
  rename(Subnat = NL_NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID)

temp_gis_SVN <- read_sf( 'data_gis/Slovenia_adm1_diss_v2.gpkg') %>% 
  select(GID_0,COUNTRY,NL_NAME_1, CC_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(subnatID = CC_1)%>% 
  rename(Subnat = NL_NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  mutate(subnatID = ifelse(Subnat == 'Eastern Slovenia', 'SVN.1', 'SVN.2')) %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1, subnatID) 
  


gis_data <- bind_rows(temp_gis_CRI,
                      temp_gis_DOM,
                      temp_gis_JPN,
                      temp_gis_PER,
                      temp_gis_SVN,
                      temp_gis_TWN,
                      temp_gis_VNM) %>% 
  select(Country,iso3,cntry_code,GID_1, subnatID) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  arrange(iso3,GID_1) #%>% 


temp <- gis_data %>% 
  st_drop_geometry()# %>% 

# identical(temp,temp_subnat)


subnat_gis_data <- gis_data %>% 
  left_join(subnat_data,by=c('iso3','subnatID')) %>% 
  # st_drop_geometry() %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr)%>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,as.character(1988:2021))

subnat_gis_data_individuals <- subnat_gis_data
subnat_data_individuals <- subnat_data



############# 3 Combine all gis data ------

# # Define CRS for GDL
# crsWgs84 <- crs(subnat_gis_data_GADM)
# st_crs(subnat_gis_data_GDL) = crsWgs84
# 
# subnat_gis_data_GDL_proj <- project(vect(subnat_gis_data_GDL),crs(vect(subnat_gis_data_GADM)))

# subnat_gis_combined <- merge(vect(subnat_gis_data_NUTS1),
#                              vect(subnat_gis_data_NUTS2),
#                              vect(subnat_gis_data_NUTS3),
#                              vect(subnat_gis_data_OECD),
#                              vect(subnat_gis_data_GADM),
#                              subnat_gis_data_GDL_proj,
#                              vect(subnat_gis_data_individuals))

subnat_gis_combined <- bind_rows(subnat_gis_data_NUTS1,
                                 subnat_gis_data_NUTS2,
                                 subnat_gis_data_NUTS3,
                                 #subnat_gis_data_OECD,
                                 subnat_gis_data_GADM,
                                 subnat_gis_data_GDL,
                                 subnat_gis_data_individuals) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,as.character(1989:2021))

testGIS <- subnat_gis_combined %>% 
  st_drop_geometry()


#### 4. filter out the countries with unreliable data ----

# the countries with value lower than given threshold (.15)

subnat_unreliableData <- testGIS %>% 
  #pivot_longer(-iso3, names_to = 'year', values_to = 'gini') %>% 
  select(iso3, paste0(1989:2021)) %>% 
  mutate(giniMean = rowMeans(select(., where(is.numeric)), na.rm = TRUE)) %>% 
  select(iso3, giniMean) %>% 
  #group_by(iso3) %>% 
  summarise(Min = min(giniMean, na.rm=T), Max = max(giniMean, na.rm=T), .by = 'iso3') %>% 
  mutate(ratio = Min/Max) %>% 
  filter(Min < 0.15 | ratio < 0.3)

# subnat_unreliableData <- national_data_combined %>% 
#   pivot_longer(-iso3, names_to = 'year', values_to = 'gini') %>% 
#   group_by(iso3) %>% 
#   summarise(Min = min(gini, na.rm=T), Max = max(gini, na.rm=T)) %>% 
#   filter(Min < 0.15)
#   
# 
# subnat_gis_combined_filtered <- testGIS %>% 
#   filter(!iso3 %in% subnat_unreliableData$iso3)
# 
# subnat_data_combined_filtered <-  subnat_gis_combined_filtered %>% 
#   st_drop_geometry()

# also, prefer GSAP over GDL for countries with only one entry

sf_adm0 <- subnat_gis_combined

subnat_data_iso3 <- subnat_gis_combined %>% 
  st_drop_geometry()  %>% 
  distinct(iso3)

# check for how many years there is subnational data for each country

sf_dataReported <- subnat_gis_combined %>% 
  st_drop_geometry() %>% 
  mutate(nmbrObs = rowSums(!is.na(.)) - 6) %>% 
  select(Country, iso3, GID_nmbr, nmbrObs) %>% 
  filter(iso3 %in% subnat_data_iso3$iso3) %>% 
  select(iso3, nmbrObs) %>% 
  distinct(.keep_all = T)

# then check the origin of the data

cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data)) %>% 
  rename(iso3 = GID_0) %>% 
  select(iso3, SUBNAT_USED) %>% 
  left_join(sf_dataReported)




####  5. add still GSAP subnational data -----

# sf_gini_disp_iso3 <- read_sf('results/vect_gini_disp_1990_2021.gpkg') %>% 
#   st_drop_geometry()  %>% 
#   filter(admID > 1000) %>% 
#   distinct(iso3)

# other subnat data after criteria (under 4_gini_adm1...)

# subnat_data_combined_ratio <- read_csv('results/subnat_data_combined_ratio_reportedOnly.csv') %>% 
#   distinct(iso3)

# exclude those where source is GDL and only 1 reported year
cntry_metaData_gini_exc <- cntry_metaData_gini %>% 
  filter(nmbrObs == 1 & SUBNAT_USED == "GDL") 

# remove the countries from list with unreliable data and only having one observation from GDL

subnat_data_combined_ratio_iso3 <- testGIS %>% 
  distinct(iso3) %>% 
  filter(!iso3 %in% cntry_metaData_gini_exc$iso3) %>% 
  filter(!iso3 %in% subnat_unreliableData$iso3)
  
## read in national data

adm0_data <- read_csv('results/gini_adm0_DispMkt_extrap.csv')


sf_gsap_org <- read_sf('data_gis/gsap-maps/GSAP2.shp') %>% 
  st_drop_geometry() %>% 
  rename(iso3 = code) %>%
  mutate(iso3 = ifelse(iso3 == 'XKX', 'XKO', iso3)) %>% 
  rename(year = baseyear) %>%
  select(iso3, geo_code2, sample, level, year, GSAP2_gini) %>% 
  filter(!iso3 %in% unique(subnat_data_combined_ratio_iso3$iso3)) #%>% 
  #filter(!iso3 %in% unique(cntry_metaData_gini_inc$iso3)) %>% 
  #filter(level == "national")

sf_gsap_all <- read_sf('data_gis/gsap-maps/GSAP2.shp') %>% 
  #st_drop_geometry() %>% 
  rename(iso3 = code) %>%
  mutate(iso3 = ifelse(iso3 == 'XKX', 'XKO', iso3)) %>% 
  rename(year = baseyear) %>%
  select(iso3, geo_code2, sample, level, year, GSAP2_gini) %>% 
  #filter(level == 'subnatid1') %>% 
  filter(!is.na(GSAP2_gini)) %>% 
  mutate(GSAP2_gini = as.numeric(GSAP2_gini))

st_write(sf_gsap_all, "data_gis/gsap-maps/GSAP2_gini.gpkg", delete_dsn = T)

national_average_sf_gsap <- sf_gsap_org %>% 
  mutate(GSAP2_gini = as.numeric(GSAP2_gini)) %>% 
  filter(!level == -1) %>% 
  filter(!level == "national") %>% 
  reframe(meanAdm0 = mean(GSAP2_gini, na.rm = TRUE), .by = c(iso3, year))

## fill missing values with national average
sf_gsap <- sf_gsap_org %>% 
  left_join(national_average_sf_gsap, by = "iso3") %>% 
  mutate(GSAP2_gini = ifelse(is.na(GSAP2_gini), meanAdm0, GSAP2_gini)) %>% 
  mutate(level = ifelse(!is.na(GSAP2_gini)&level == "-1", "subnatid",level  )) %>% 
  ## filter out areas without data
  mutate(year.x = ifelse(is.na(year.x), year.y, year.x)) %>% 
  rename(year = year.x) %>% 
  select(-year.y) %>% 
  filter(!level == -1) %>% 
  filter(!level == "national") %>% 
  #drop_na() %>% 
  arrange(iso3, level) 

gsapIso3 <- unique(sf_gsap$iso3)

gsapYears <- unique(sf_gsap$year) %>% 
  sort()

length(gsapIso3)

## create file


sf_gsap_inc <- sf_gsap %>% #read_sf('data_gis/gsap-maps/GSAP2.shp') %>% 
  #st_drop_geometry() %>% 
  # rename(iso3 = code) %>%
  rename(Subnat = sample) %>% 
  # mutate(iso3 = ifelse(iso3 == 'XKX', 'XKO', iso3)) %>% 
  # rename(year = baseyear) %>%
  select(iso3, Subnat, geo_code2, level, year, GSAP2_gini) %>% 
  filter(iso3 %in% gsapIso3) %>% 
  #drop_na() %>% 
  mutate(GSAP2_gini = as.numeric(GSAP2_gini)) %>% 
  filter(!is.na(GSAP2_gini)) %>% 
  arrange(year, iso3)

#UKR_-1_ADM1_3158

#temp <- sf_gsap_inc %>% st_drop_geometry()

sf_gsap_inc_wide <- sf_gsap_inc %>% 
  pivot_wider(names_from = 'year', values_from = 'GSAP2_gini') %>% 
  left_join(cntryID) %>% 
  group_by(iso3) %>% 
  mutate(rownmb = row_number()) %>% 
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100 + rownmb) %>% 
  select(-iso2, -rownmb) %>% 
  mutate(GID_1 = as.character(GID_nmbr)) %>% 
  select(Country, iso3, cntry_code, Subnat, geo_code2, GID_1, GID_nmbr, everything(), -level)

## add geometry

sf_gsap_geom <- read_sf('data_gis/gsap-maps/GSAP2.shp') %>% 
  select(geo_code2)

sf_gsap_inc_wide_geom <- sf_gsap_inc_wide %>% 
  left_join(sf_gsap_geom) %>% 
  select(-geo_code2) %>% 
  rename(geom = geometry)

tempGSAP <- sf_gsap_inc_wide %>% 
  select(-geo_code2) %>% 
  st_drop_geometry()

write_csv(tempGSAP,'results/sf_gsap_inc_wide.csv')



### compare GSAP with other data

sf_gsap_all_noGeom <- sf_gsap_all %>% 
  st_drop_geometry() %>% 
  filter(!level == -1) %>% 
  filter(!level == "national") %>% 
  distinct(iso3, year) %>% 
  left_join(subnat_gis_combined %>% 
              st_drop_geometry() %>% 
              select(iso3, '1990':'2021') %>% 
              pivot_longer(-iso3, names_to = 'year', values_to = 'giniSubnat') %>% 
              drop_na() %>% 
              mutate(subnat_data = 1) %>% 
              distinct(iso3, year, .keep_all = T)) %>% 
  filter(subnat_data == 1)


# sf_gsap_inc_wide %>% filter(iso3 == 'MMR')

## combine with others

subnat_gis_combined_GSAP <- subnat_gis_combined %>% 
  filter(iso3 %in% subnat_data_combined_ratio_iso3$iso3) %>% 
  filter(!iso3 %in% gsapIso3) %>% 
  bind_rows(sf_gsap_inc_wide_geom) %>% 
  arrange(iso3, GID_nmbr)


testGIS_GSAP <- subnat_gis_combined_GSAP %>% 
  st_drop_geometry() 

testGIS_GSAP_iso3 <- unique(testGIS_GSAP$iso3)


## update the country metadata

cntry_metaData_gini_upd <- cntry_metaData_gini %>% 
  mutate(SUBNAT_USED = ifelse(iso3 %in% gsapIso3, 'GSAP', SUBNAT_USED)) %>% 
  mutate(nmbrObs = ifelse(SUBNAT_USED == 'GSAP', 1, nmbrObs))


write_csv(cntry_metaData_gini_upd, "results/cntry_metaData_gini_upd.csv")

#### 6. save final ----


write_sf(subnat_gis_combined_GSAP, "results/gisData_gini_combined.gpkg",delete_dsn = TRUE)

#st_write(subnat_gis_data_GDL, "results/subnat_gis_data_GDL.gpkg",delete_dsn = TRUE)

write_csv(testGIS_GSAP, "results/subnat_gis_combined.csv")

#st_is_valid(subnat_gis_combined)

# test which subnat areas we do not have data but GSAP has
# https://datacatalog.worldbank.org/search/dataset/0042041/International-Poverty-Line---Subnational-Poverty




# test for which admin units no data

allSubNat <- read_csv("results/subnat_gis_combined.csv") 

allSubNat_noData <- allSubNat%>% 
  filter(!if_any('1989':'2021', ~ !is.na(.)))



#### remove sub-national data that is not reliable

# some of the GDL data has very small values; those countries are removed here






