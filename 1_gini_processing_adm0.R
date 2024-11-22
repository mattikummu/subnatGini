
### processing national gini data

## creator: matti.kummu@aalto.fi 


## load libraries

library(sf)
library(terra)
library(Rfast)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1 load data -----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data))

#### 2. load national data -----


#### 2.1. world bank national data 

WB_cntryData <- read_xls("data_raw/national_data/API_SI.POV.GINI_DS2_en_excel_v2_4770509.xls", sheet="Data", skip = 3) %>% 
  rename(Country = 'Country Name') %>% 
  rename(iso3 = 'Country Code') %>% 
  mutate(iso3 = ifelse(Country == "Kosovo", "XKO", iso3)) %>% 
  select(c(Country,iso3,paste0(as.character(seq(1985,2021,1))))) %>% 
  mutate(across(where(is.numeric), ~./100)) %>% 
  filter(iso3 %in% cntryID$iso3) %>% 
  mutate(giniMean = rowMeans(select(., where(is.numeric)), na.rm = TRUE)) %>% 
  select(Country, iso3, giniMean, everything())

WB_metaRegData <- read_xls("data_raw/national_data/API_SI.POV.GINI_DS2_en_excel_v2_4770509.xls", sheet="Data", skip = 3) %>% 
  rename(Country = 'Country Name') %>% 
  rename(iso3 = 'Country Code') %>% 
  mutate(iso3 = ifelse(Country == "Kosovo", "XKO", iso3)) %>% 
  select(c(Country,iso3,paste0(as.character(seq(1985,2021,1))))) %>% 
  mutate(across(where(is.numeric), ~./100)) %>% 
  filter(!iso3 %in% cntryID$iso3) %>% 
  mutate(giniMean = rowMeans(select(., where(is.numeric)), na.rm = TRUE)) 


if (dir.exists('results/')) {
  
} else {
  dir.create('results/')  
}

write.xlsx(WB_cntryData,"results/WB_national_data.xlsx")

WB_cntryData_missing <- WB_cntryData %>% 
  filter(is.na(giniMean))

WB_extent <- WB_cntryData %>% 
  filter(!is.na(giniMean)) %>% 
  pivot_longer(-c(Country, iso3, giniMean), names_to = 'year', values_to = 'gini') %>% 
  drop_na()



##### 2.2. GDL 

GDL_nationalMetaData <- read.xlsx('data_raw/GDL-Gini-coefficient-wealth-inequality-data_mod.xlsx') %>% 
  as_tibble() %>% 
  rename(iso3 = ISO_Code) %>% 
  rename(Subnat = Region) %>% 
  left_join(cntryID,by='iso3') %>% 
  arrange(iso3) %>% 
  select(-c(Continent,Country.x,Level,Country.y,cntry_code, iso2)) %>% 
  filter(Subnat == "Total")



#### 2.3 WID national data 

WID_nationalMetaData <- read_xlsx("data_raw/national_data/WID_Data_15022023-133037.xlsx", sheet="Metadata") %>% 
  rename(CountryWID = 'Country Name') %>% 
  rename(iso2 = 'Country Code') %>% 
  select(iso2, CountryWID) %>% 
  distinct() %>% 
  left_join(cntryID, by = "iso2") %>% 
  distinct() %>% 
  filter(!is.na(iso3)) %>% 
  select(-Country) %>% 
  rename(Country = CountryWID)

WID_nationalData <- read_xlsx("data_raw/national_data/WID_Data_15022023-133037.xlsx", sheet="Data") %>% 
  select(c(seq(1,39,1))) %>% 
  select(-c(Percentile))  %>% 
  left_join(WID_nationalMetaData)  %>% 
  select(Country, iso3, iso2, everything()) %>% 
  filter(!is.na(iso3)) %>% 
  select(-c(iso2, cntry_code))


names(WID_nationalData) <- c("Country", "iso3", paste0(as.character(seq(1985,2021,1)))) 


WID_extent <- WID_nationalData %>% 
  mutate(giniMean = rowMeans(select(., where(is.numeric)), na.rm = TRUE)) %>% 
  filter(!is.na(giniMean)) %>% 
  pivot_longer(-c(Country, iso3, giniMean), names_to = 'year', values_to = 'gini') %>% 
  drop_na()


#### 2.4 SWIID 


# Load the SWIID
load("data_raw/national_data/swiid9_4/swiid9_4.rda")

swiidMeta <- read_xlsx("data_raw/national_data/swiid9_4/swiid9_4_summary.xlsx", sheet="metadata")

swiidData <-  swiid_summary %>% 
  rename(Country = country) %>% 
  left_join(swiidMeta) %>% 
  filter(year >= 1985) %>% 
  filter(!is.na(iso3)) %>% 
  mutate(year = as.character(year)) %>% 
  select(Country, iso3, year, gini_disp, gini_disp_se, gini_mkt, gini_mkt_se) 



#### 2.5 compare SWIID and WID datasets

iso3SWIID <-  unique(swiidData$iso3) %>% 
  sort()

length(iso3SWIID)

isoWID <- unique(WID_nationalData$iso3) %>% 
  sort()

length(isoWID)

iso3WB <- unique(WB_extent$iso3) %>% 
  sort()

length(iso3WB)

diffNat <- diffobj::diffChr(isoWID,iso3SWIID)

# missing from SWIID but in WID
WIDnotSWIID <- c('BRN', 'CUB', 'ERI', 'MAC', 'PRK', 'STP')

# countries that could be used to scale values from WID to SWIID (exist in both)
WIDnotSWIID_neighbours <- c('MYS','HTI', 'DJI', 'HKG', 'KOR', 'GAB')


### 2.6 compare SWIID and GDL datasets

iso3SWIID <-  unique(swiidData$iso3) %>% 
  sort()

isoGDL <- GDL_nationalMetaData$iso3 %>% 
  sort()

diffNatSWIID_GDL <- diffobj::diffChr(isoGDL,iso3SWIID)

### 3. harmonise those that are missing from SWIID but in GDL ----

GDLnotSWIID <- c('CIV', 'CUB', 'ERI', 'STP')

# Western Sahara part of Marocco in GDL; let's use that one for the West Sahara


# morocco in GDL
MAR_subnat_data <- read.xlsx('data_raw/GDL-Gini-coefficient-wealth-inequality-data_mod.xlsx') %>% 
  as_tibble() %>% 
  rename(iso3 = ISO_Code) %>% 
  rename(Subnat = Region) %>% 
  left_join(cntryID,by='iso3') %>% 
  filter(iso3=='MAR') %>% 
  select(-c(Continent,Country.x,Level,Country.y,cntry_code, iso2)) 


names(MAR_subnat_data) <- c('iso3','RegID','Subnat',as.character(1991:2021))

# marocco in SWIID

MARyears <- MAR_subnat_data %>% 
  select(-c(iso3, Subnat)) %>% 
  pivot_longer(-RegID) %>% 
  rename(year = name)

MAR_swiidData <- swiidData %>% 
  filter(iso3=='MAR') %>% 
  filter(year %in% MARyears$year) %>% 
  select(c(year, gini_disp, gini_disp_se, gini_mkt, gini_mkt_se))


# calculate ratio for south part of Morocco, and use that to scale SWIID to it
WestSahara <- MAR_subnat_data %>% 
  select(-c(iso3,Subnat )) %>% 
  filter(RegID %in% c('MARt', 'MARr107')) %>% 
  pivot_longer(-RegID) %>% 
  pivot_wider(names_from = RegID) %>% 
  mutate(ratio = MARr107 / MARt) %>% 
  rename(year = name) %>% 
  left_join(MAR_swiidData) %>% 
  mutate(gini_disp = ratio * gini_disp / 100) %>% 
  mutate(gini_disp_se = ratio * gini_disp_se / 100) %>% 
  mutate(gini_mkt = ratio * gini_mkt  / 100) %>% 
  mutate(gini_mkt_se = ratio * gini_mkt_se /100) %>% 
  mutate(iso3 = 'ESH') %>% 
  mutate(Country = "Western Sahara") %>% 
  select(Country, iso3, year, gini_disp, gini_disp_se, gini_mkt, gini_mkt_se)



#### CIV from GDL to SWIID 

CIV_national <- GDL_nationalMetaData %>% 
  filter(iso3 == 'CIV')



# estimate Gini disp

source('functions/f_estimateMissingGini.R')

if (exists('estGiniDisp')) {
  remove('estGiniDisp')
}


for (iIso3 in 1:length(WIDnotSWIID_neighbours)) {
  temp_estGiniDisp <- f_estimateMissingGini(iso3_inBoth = WIDnotSWIID_neighbours[iIso3], 
                                              iso3_missing = WIDnotSWIID[iIso3], 
                                              giniVar = 'gini_disp', 
                                              scalingData = WID_nationalData, 
                                              nRand = 100, 
                                              nPred = 100)
  
  if (exists('estGiniDisp')) {
    estGiniDisp <- estGiniDisp %>% 
      bind_rows(temp_estGiniDisp)
  } else {
    estGiniDisp <- temp_estGiniDisp
  }
  
}


# estimate Gini mkt

if (exists('estGiniMkt')) {
  remove('estGiniMkt')
}


for (iIso3 in 1:length(WIDnotSWIID_neighbours)) {
  temp_estGiniMkt <- f_estimateMissingGini(iso3_inBoth = WIDnotSWIID_neighbours[iIso3], 
                                             iso3_missing = WIDnotSWIID[iIso3], 
                                             giniVar = 'gini_mkt', 
                                             scalingData = WID_nationalData, 
                                             nRand = 100, 
                                             nPred = 100)
  
  if (exists('estGiniMkt')) {
    estGiniMkt <- estGiniMkt %>% 
      bind_rows(temp_estGiniMkt)
  } else {
    estGiniMkt <- temp_estGiniMkt
  }
  
}


temp_GDL_nationalMetaData <- GDL_nationalMetaData %>% 
  rename(Country = GDLCODE) %>% 
  select(-Subnat)



CIV_estGiniDisp <- f_estimateMissingGini(iso3_inBoth = 'GHA', 
                                          iso3_missing = 'CIV', 
                                          giniVar = 'gini_disp', 
                                          scalingData = temp_GDL_nationalMetaData, 
                                          nRand = 100, 
                                          nPred = 100)

estGiniDisp <- estGiniDisp %>% 
  bind_rows(CIV_estGiniDisp)

CIV_estGiniMkt <- f_estimateMissingGini(iso3_inBoth = 'GHA', 
                                          iso3_missing = 'CIV', 
                                          giniVar = 'gini_mkt', 
                                          scalingData = temp_GDL_nationalMetaData, 
                                          nRand = 100, 
                                          nPred = 100)

estGiniMkt <- estGiniMkt %>% 
  bind_rows(CIV_estGiniMkt)

# create result table
years <- as.character(seq(1985,2021,1))
missingResults <- c(WIDnotSWIID, 'CIV') %>% 
  as_tibble() %>% 
  tibble::add_column(!!!set_names(as.list(rep(NA, length(years))),nm=years)) %>% 
  pivot_longer(cols = all_of(years), names_to = 'year', values_to = 'gini') %>% 
  rename(iso3 = value) %>% 
  left_join(cntryID[c('iso3', 'Country')]) %>% 
  select(Country, everything())

# store results
missingResults <- missingResults %>% 
  select(-gini) %>% 
  left_join(estGiniDisp) %>% 
  left_join(estGiniMkt) %>% 
  drop_na()

unique(missingResults$iso3)


# combine with SWIID data

swiidDataFilled <- swiidData %>% 
  mutate(across(where(is.numeric), ~./100)) %>% 
  bind_rows(missingResults) %>% 
  bind_rows(WestSahara) %>% 
  arrange(iso3, year)




write_csv(swiidDataFilled, "results/swiidDataFilled.csv")

### check number of subnational areas

adm0Gnic <- swiidDataFilled %>% 
  select(iso3, year, gini_disp) %>% 
  drop_na() %>% 
  summarise(nNat = n_distinct(iso3))




#### 4 add to gis -----

swiidDataFilled <- read_csv("results/swiidDataFilled.csv") %>% 
  select(iso3, year, gini_disp) %>% 
  pivot_wider( names_from = year, values_from = gini_disp) %>% 
  select(iso3, as.character(seq(1985,2021,1))) %>% 
  mutate(mean = rowMeans(across(9:37), na.rm=T))

gadm_adm0 <- st_read('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>% 
  rename(iso3 = GID_0) %>% 
  filter(iso3 != "ALA") %>% 
  left_join(cntryID[,c(1,3)])


swiidDataFilled_gis <- gadm_adm0 %>% 
  left_join(swiidDataFilled) #%>% 
#rmapshaper::ms_simplify(input = as(.,'Spatial'), keep = 0.1, keep_shapes = T) 
#   st_as_sf() 

writeVector(vect(swiidDataFilled_gis), 'results/swiidDataFilled_gis.gpkg', overwrite = T)






