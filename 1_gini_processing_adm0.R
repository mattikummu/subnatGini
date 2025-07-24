
### processing national gini data

## creator: matti.kummu@aalto.fi 


## load libraries

library(sf)
library(terra)
library(Rfast)
# library(qgisprocess)

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


WID_nationalMetaData <- read_xlsx("data_raw/national_data/WID_Data_10062025-072528.xlsx", sheet="Metadata") %>% 
  rename(CountryWID = 'Country Name') %>% 
  rename(iso2 = 'Country Code') %>% 
  select(iso2, CountryWID) %>% 
  distinct() %>% 
  left_join(cntryID, by = "iso2") %>% 
  distinct() %>% 
  filter(!is.na(iso3)) %>% 
  select(-Country) %>% 
  rename(Country = CountryWID) %>% 
  distinct()


WID_nationalData <- read_xlsx("data_raw/national_data/WID_Data_10062025-072528.xlsx", sheet="Data", col_names = F) %>% 
  set_names(c("Country", "Var", "Var2", "year", "gini")) %>% 
  select(-Var, -Var2) %>% 
  left_join(WID_nationalMetaData)  %>% 
  select(Country, iso3, iso2, everything()) %>% 
  filter(!is.na(iso3)) %>% 
  select(-c(iso2, cntry_code)) %>% 
  distinct(iso3, year, .keep_all = T) %>% 
  pivot_wider(names_from = 'year', values_from = "gini") %>% 
  pivot_longer(-c('Country', 'iso3'), names_to = 'year', values_to = 'gini')


write_csv(WID_nationalData, "results/WID_nationalData.csv")

# WID_nationalMetaData <- read_xlsx("data_raw/national_data/WID_Data_15022023-133037.xlsx", sheet="Metadata") %>% 
#   rename(CountryWID = 'Country Name') %>% 
#   rename(iso2 = 'Country Code') %>% 
#   select(iso2, CountryWID) %>% 
#   distinct() %>% 
#   left_join(cntryID, by = "iso2") %>% 
#   distinct() %>% 
#   filter(!is.na(iso3)) %>% 
#   select(-Country) %>% 
#   rename(Country = CountryWID)
# 
# 
# WID_nationalData <- read_xlsx("data_raw/national_data/WID_Data_15022023-133037.xlsx", sheet="Data") %>% 
#   select(c(seq(1,39,1))) %>% 
#   select(-c(Percentile))  %>% 
#   left_join(WID_nationalMetaData)  %>% 
#   select(Country, iso3, iso2, everything()) %>% 
#   filter(!is.na(iso3)) %>% 
#   select(-c(iso2, cntry_code))

# 
# names(WID_nationalData) <- c("Country", "iso3", paste0(as.character(seq(1985,2023,1)))) 
# 
# 
WID_extent <- WID_nationalData #%>%
#   mutate(giniMean = rowMeans(select(., where(is.numeric)), na.rm = TRUE)) %>%
#   filter(!is.na(giniMean)) %>%
#   pivot_longer(-c(Country, iso3, giniMean), names_to = 'year', values_to = 'gini') %>%
#   drop_na()


#### 2.4 SWIID 


# Load the SWIID
load("data_raw/national_data/swiid9_8/swiid9_8.rda")

swiidMeta <- read_xlsx("data_raw/national_data/swiid9_4/swiid9_4_summary.xlsx", sheet="metadata") %>% 
  mutate(Country = ifelse(iso3 == "STP", "São Tomé and Príncipe", Country)) %>% 
  mutate(Country = ifelse(iso3 == "CIV", "Côte d'Ivoire", Country)) %>% 
  bind_rows(tibble(Country = "Marshall Islands", iso3 = "MHL"))


swiidData <-  swiid_summary %>% 
  rename(Country = country) %>% 
  left_join(swiidMeta) %>% 
  filter(year >= 1985) %>% 
  filter(!is.na(iso3)) %>% 
  mutate(year = as.character(year)) %>% 
  select(Country, iso3, year, gini_disp, gini_disp_se, gini_mkt, gini_mkt_se) 

swiidData %>% distinct(iso3)

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
WIDnotSWIID <- c('BRN', 'CUB', 'ERI', 'MAC', 'PRK', 'MCO')

# countries that could be used to scale values from WID to SWIID (exist in both)
WIDnotSWIID_neighbours <- c('MYS','HTI', 'DJI', 'HKG', 'KOR', 'FRA')


### 2.6 compare SWIID and GDL datasets

iso3SWIID <-  unique(swiidData$iso3) %>% 
  sort()

isoGDL <- GDL_nationalMetaData$iso3 %>% 
  sort()

diffNatSWIID_GDL <- diffobj::diffChr(isoGDL,iso3SWIID)

### 3. harmonise those that are missing from SWIID but in GDL ----



# Western Sahara part of Marocco in GDL; let's use that one for the West Sahara


# morocco in GDL
MAR_subnat_data <- read.xlsx('data_raw/GDL-Gini-coefficient-wealth-inequality-data_2025.xlsx') %>% 
  as_tibble() %>% 
  rename(iso3 = ISO_Code) %>% 
  rename(Subnat = Region) %>% 
  left_join(cntryID,by='iso3') %>% 
  filter(iso3=='MAR') %>% 
  select(-c(Continent,Country.x,Level,Country.y,cntry_code, iso2)) 


names(MAR_subnat_data) <- c('iso3','RegID','Subnat',as.character(1992:2023))

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
  # add extra uncertainty due to scaling
  mutate(gini_disp_se = gini_disp_se * 1.5) %>% 
  mutate(gini_mkt_se = gini_mkt_se * 1.5) %>% 
  mutate(iso3 = 'ESH') %>% 
  mutate(Country = "Western Sahara") %>% 
  select(Country, iso3, year, gini_disp, gini_disp_se, gini_mkt, gini_mkt_se)


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
  select(-Subnat) %>% 
  pivot_longer(-c(iso3, Country), names_to = 'year', values_to = 'gini')


# 
# CIV_estGiniDisp <- f_estimateMissingGini(iso3_inBoth = 'GHA', 
#                                          iso3_missing = 'CIV', 
#                                          giniVar = 'gini_disp', 
#                                          scalingData = temp_GDL_nationalMetaData, 
#                                          nRand = 100, 
#                                          nPred = 100)
# 
# estGiniDisp <- estGiniDisp %>% 
#   bind_rows(CIV_estGiniDisp)
# 
# CIV_estGiniMkt <- f_estimateMissingGini(iso3_inBoth = 'GHA', 
#                                         iso3_missing = 'CIV', 
#                                         giniVar = 'gini_mkt', 
#                                         scalingData = temp_GDL_nationalMetaData, 
#                                         nRand = 100, 
#                                         nPred = 100)
# 
# estGiniMkt <- estGiniMkt %>% 
#   bind_rows(CIV_estGiniMkt)

# create result table
years <- as.character(seq(1985,2023,1))
missingResults <- c(WIDnotSWIID) %>% 
  as_tibble() %>% 
  tibble::add_column(!!!set_names(as.list(rep(NA, length(years))),nm=years)) %>% 
  pivot_longer(cols = all_of(years), names_to = 'year', values_to = 'gini') %>% 
  rename(iso3 = value) %>% 
  left_join(cntryID[c('iso3', 'Country')]) %>% 
  select(Country, everything()) %>% 
  distinct(iso3, year, .keep_all = T)

# store results
missingResults <- missingResults %>% 
  select(-gini) %>% 
  left_join(estGiniDisp %>%  drop_na()) %>% 
  left_join(estGiniMkt %>%  drop_na()) %>% 
  # add extra uncertainty due to scaling
  mutate(gini_disp_se = gini_disp_se * 1.5) %>% 
  mutate(gini_mkt_se = gini_mkt_se * 1.5) %>% 
  drop_na()

unique(missingResults$iso3)


### French data to its Overseas Departments

# Guadeloupe - GLP
# Martinique - MTQ
# French Guiana - GUF
# Réunion - REU
# Mayotte - MYT

FRA_DOM_iso3 <- c('GLP','MTQ','GUF', 'REU','MYT')
FRA_DOM_name <- c('Guadeloupe','Martinique','French Guiana', 'Réunion','Mayotte')
cntryID_DOM <- cntryID %>% 
  filter(iso3 %in% FRA_DOM_iso3)

DOM_wiidData <- swiidData %>% 
  filter(iso3 %in% FRA_DOM_iso3) 

FRA_swiidData <- swiidData %>% 
  filter(iso3 == 'FRA') 


# Create a function to generate the data for each DOM
create_dom_data <- function(country, iso3, original_data) {
  data.frame(
    Country = rep(country, nrow(original_data)),
    iso3 = rep(iso3, nrow(original_data)),
    year = original_data$year,
    gini_disp = original_data$gini_disp,
    gini_disp_se = original_data$gini_disp_se,
    gini_mkt = original_data$gini_mkt,
    gini_mkt_se = original_data$gini_mkt_se
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
  create_dom_data(dom, doms[[dom]], FRA_swiidData)
})

combined_dom_data <- do.call(rbind, dom_data_list) %>% 
  as_tibble() %>% 
  mutate(across(starts_with("gini_"), ~ .x / 100))




# combine with SWIID data

swiidDataFilled <- swiidData %>% 
  mutate(across(where(is.numeric), ~./100)) %>% 
  bind_rows(missingResults) %>% 
  bind_rows(WestSahara) %>% 
  bind_rows(combined_dom_data) %>% 
  arrange(iso3, year)

write_csv(swiidDataFilled, "results/swiidDataFilled.csv")




### check number of adm0 areas

adm0Gnic <- swiidDataFilled %>% 
  select(iso3, year, gini_disp) %>% 
  drop_na() %>% 
  summarise(nNat = n_distinct(iso3))




#### 4 add to gis -----

swiidDataFilled <- read_csv("results/swiidDataFilled.csv") %>% 
  select(iso3, year, gini_disp) %>% 
  distinct(iso3, year, .keep_all = T) %>% 
  pivot_wider( names_from = year, values_from = gini_disp) %>% 
  select(iso3, as.character(seq(1985,2023,1))) %>% 
  mutate(mean = rowMeans(across(9:37), na.rm=T))


WID_DataFilled <- read_csv("results/WID_nationalData.csv") %>% 
  select(iso3, year, gini) %>% 
  distinct(iso3, year, .keep_all = T) %>% 
  pivot_wider( names_from = year, values_from = gini) %>% 
  select(iso3, as.character(seq(1985,2023,1))) %>% 
  mutate(mean = rowMeans(across(9:37), na.rm=T))

gadm_adm0 <- st_read('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>%
  rename(iso3 = GID_0) %>%
  filter(iso3 != "ALA") %>%
  filter(iso3 != "ATA") %>%
  left_join(cntryID[,c(1,3)])


# 
# swiidDataFilled_gis <- gadm_adm0_simpl %>% 
#   left_join(swiidDataFilled) #%>% 
# #rmapshaper::ms_simplify(input = as(.,'Spatial'), keep = 0.1, keep_shapes = T) 
# #   st_as_sf() 
# 
swiidDataFilled_gis <- gadm_adm0 %>%
  left_join(swiidDataFilled)

writeVector(terra::vect(swiidDataFilled_gis), 'results/swiidDataFilled_gis.gpkg', overwrite = T)






