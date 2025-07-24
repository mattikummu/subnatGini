
# comparison of adm0 datasets


# Load necessary libraries

library(sf)
library(terra)
library(Rfast)

library(tmap)
library(scico)
library(rnaturalearth)
library(rmapshaper)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 
library(data.table)


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### 1 read data in ----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

cntry_metaData_gini <- read.xlsx("data_in/cntry_metaDataGINI.xlsx",sheet = 'cntry_meta_gini') %>% 
  as_tibble() %>% 
  mutate(SUBNAT_USED = ifelse(SUBNAT_USED == '-', NA, SUBNAT_USED)) %>% 
  mutate(GIS_data = ifelse(GIS_data == '-', NA, GIS_data))



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


WB_cntryData_missing <- WB_cntryData %>% 
  filter(is.na(giniMean))

WB_extent <- WB_cntryData %>% 
  filter(!is.na(giniMean)) %>% 
  pivot_longer(-c(Country, iso3, giniMean), names_to = 'year', values_to = 'gini') %>% 
  drop_na() %>% 
  filter(year > 1989)


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


WID_extent <- WID_nationalData %>% 
  rename(gini_WID = gini) %>% 
  select(iso3, year, gini_WID)

# remove repeated values for years without data

WID_extent_cleaned <- WID_extent %>%
  mutate(year = as.integer(year)) %>%  # ensure year is numeric
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    run_id = rleid(gini_WID),
    is_last_in_run = row_number() == max(row_number()),
    gini_WID = if_else(row_number() != max(row_number()) & run_id == lead(run_id), NA_real_, gini_WID)
  ) %>%
  ungroup() %>%
  select(-run_id, -is_last_in_run)  %>% # optional: clean up helpers
  drop_na() 


# Load the SWIID

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

swiid_extent <- swiidData %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year > 1989) %>% 
  select(iso3, year, gini_disp) %>% 
  mutate(gini_disp = gini_disp/100)



##### 2 compare -----

df_comparison <- swiid_extent %>% 
  left_join(WID_extent_cleaned) %>% 
  drop_na()

cor_rmse_by_country <- df_comparison %>%
  group_by(iso3) %>%
  mutate(
    norm_disp = gini_disp / mean(gini_disp, na.rm = TRUE),
    norm_WID  = gini_WID  / mean(gini_WID, na.rm = TRUE)
  ) %>%
  summarise(
    n_obs = sum(!is.na(norm_disp) & !is.na(norm_WID)),
    pattern_corr = if (n_obs >= 2) cor(norm_disp, norm_WID, use = "complete.obs") else NA_real_,
    pattern_rmse = if (n_obs >= 2) sqrt(mean((norm_disp - norm_WID)^2, na.rm = TRUE)) else NA_real_
  ) %>%
  ungroup() %>% 
  mutate(harmonic_score = 2 * ( (1 - pattern_corr) * pattern_rmse ) / ((1 - pattern_corr) + pattern_rmse)) %>% 
  filter(n_obs >= 10) %>% 
  arrange((harmonic_score))



### 3 plot -----


source('functions/f_Plot_sf_abs.R')


sf_adm0 <- read_sf("data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')


sf_gini_disp_adm0 <- read_sf('results/vect_adm0_gini_disp_1990_2021.gpkg') %>% 
  mutate(slopeGiniDisp = 32*slope) %>% 
  rename(giniDisp2021 = '2021')  %>% 
  rename(giniDisp1990 = '1990') %>% 
  filter(!iso3 == 'ATA')

sf_gini_disp_adm0_cor <- sf_gini_disp_adm0 %>% 
  left_join(cor_rmse_by_country)


minGini <- quantile( cor_rmse_by_country$harmonic_score, .05, na.rm=T)
maxGini <- quantile( cor_rmse_by_country$harmonic_score, .95, na.rm=T) 

giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                  by= 0.01) 


# minSlope <- quantile(sf_gini_disp$slopeGiniDisp, .05, na.rm = T)
# maxSlope <- quantile(sf_gini_disp$slopeGiniDisp, .95, na.rm = T)
# 
# slopeRange <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ), 
#                    plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
#                    by= 0.01) 


scico_palette_names()

giniPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "glasgow")

p_giniAdm0_harmScore <- f_Plot_sf_abs(sf_gini_disp_adm0_cor,'harmonic_score',giniRange, colPal = giniPal )
p_giniAdm0_harmScore


# RMSE

minRMSE <- quantile( cor_rmse_by_country$pattern_rmse, .05, na.rm=T)
maxRMSE <- quantile( cor_rmse_by_country$pattern_rmse, .95, na.rm=T) 

rmseRange <- seq( plyr::round_any( minRMSE,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxRMSE,accuracy=0.05,f=ceiling ) ,
                  by= 0.005) 

scico_palette_names()

giniPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "tokyo")

p_giniAdm0_rmse <- f_Plot_sf_abs(sf_gini_disp_adm0_cor,'pattern_rmse',giniRange, colPal = giniPal )
p_giniAdm0_rmse




source('functions/f_Plot_sf_trend.R')

minCorr <- quantile( cor_rmse_by_country$pattern_corr, .05, na.rm=T)
maxCorr <- quantile( cor_rmse_by_country$pattern_corr, .95, na.rm=T) 

corrRange <- seq( plyr::round_any( minCorr,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxCorr,accuracy=0.05,f=ceiling ) ,
                  by= 0.1) 


# minSlope <- quantile(sf_gini_disp$slopeGiniDisp, .05, na.rm = T)
# maxSlope <- quantile(sf_gini_disp$slopeGiniDisp, .95, na.rm = T)
# 
# slopeRange <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ), 
#                    plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
#                    by= 0.01) 




corrPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "broc")

p_giniAdm0_corr <- f_Plot_sf_trend(sf_gini_disp_adm0_cor,'pattern_corr',corrRange, pal = corrPal )
p_giniAdm0_corr




if (dir.exists('figures/figAdm0Corr/')) {
  
} else {
  dir.create('figures/figAdm0Corr/')  
}

layers <- list(p_giniAdm0_corr, p_giniAdm0_harmScore, p_giniAdm0_rmse)

nameLayers <- c('p_giniAdm0_corr', 'p_giniAdm0_harmScore', 'p_giniAdm0_rmse')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  p_fig_leg <- layers[[i]]
  
  tmap_save(p_fig,filename = paste0('figures/figAdm0Corr/fig_',nameLayers[i],'.png'),width = 80, height = 35.51, units='mm', dpi = 450)
  tmap_save(p_fig_leg,filename = paste0('figures/figAdm0Corr/fig_legend_',nameLayers[i],'.png'),width = 80, height = 35.51, units='mm', dpi = 450)
  
}





