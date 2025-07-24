### summary of the datasets


library(terra)
library(sf)

library(dplyr)
library(tidyr)

# Your dataset


# set working directory the path that this script is located in
setwd('/Users/mkummu/R/subnatGini/')

### read data in -----

adm0_data_swiid <- read.csv('results/swiidDataFilled.csv') %>% 
  select(-gini_mkt, -gini_mkt_se, -Country) %>% 
  as_tibble() %>% 
  distinct(iso3, year, .keep_all = T)
#filter(iso3 == 'IND')

adm0_data_WID <- read.csv('results/WID_nationalData.csv') %>% 
  as_tibble() %>% 
  distinct(iso3, year, .keep_all = T)


adm0_data_full_swiid <- read.csv('results/gini_adm0_DispMkt_extrap.csv') %>% 
  select(-gini_mkt) %>% 
  as_tibble()

adm0_data_full_WID <- read.csv('results/gini_adm0_WID_extrap_filled.csv') %>% 
  as_tibble()

v_adm1 <- vect("results_final/polyg_adm1_gini_disp_1990_2023.gpkg")


## read in pop
r_pop <- subset(rast("data_gis/r_pop_GHS_1985_2025_5arcmin.tif"), 6:39)


# population for each admin unit
v_pop_adm1 <- terra::extract(r_pop, v_adm1, fun = sum, na.rm=T)


# adm1_data <- read.csv('results/subnat_data_combined_ratio_reportedOnly.csv') %>% 
#   as_tibble() %>% 
#   select(iso3, cntry_code, GID_nmbr, s1989:s2021) %>% 
#   # if these do not exist
#   mutate(s2022 = NA) %>% mutate(s2023 = NA) %>% 
#   setNames(c('iso3', 'cntry_code', 'GID_nmbr', paste0(1989:2023))) %>% 
#   pivot_longer(-c('iso3', 'cntry_code', 'GID_nmbr'), values_to = 'ratio_gini', names_to = 'year' )

adm1_data_org <- read.csv('results/subnat_data_combined_ratio_reportedOnly.csv') %>%
  as_tibble() 


adm1_data_full <- read.csv('results/subnat_data_combined_ratio_interp.csv') %>% 
  as_tibble() %>% 
  select(-cntry_code) %>% 
  drop_na() 



adm1_data_full_SWIID <- read_sf("results/vect_gini_disp_1990_2023.gpkg") %>% 
  st_drop_geometry()

adm1_data_full_WID <- read_sf("results/vect_gini_WID_1990_2023.gpkg") %>% 
  st_drop_geometry()


### adm0 ---

adm0_data_swiid %>% 
  distinct(iso3)

adm0_data_full_swiid_unique <- adm0_data_full_swiid %>% 
  distinct(iso3)

adm0_data_full_WID_unique <- adm0_data_full_WID %>% 
  distinct(iso3)

### adm1 ---


df_adm1_pop <- as_tibble(v_adm1) %>% 
  select(iso3, admID) %>% 
  bind_cols(v_pop_adm1 %>% as_tibble() %>% select(-ID)) %>% 
  set_names(c('iso3', 'admID', paste0(1990:2023))) %>% 
  pivot_longer(-c(iso3, admID), values_to = 'pop', names_to = 'year') %>% 
  mutate(year = as.integer(year)) %>% 
  distinct(admID, year, .keep_all=T)

adm1_data_full_pop <- adm1_data_full %>% 
  rename(admID = GID_nmbr) %>% 
  distinct(admID, year, .keep_all=T) %>% 
  left_join(df_adm1_pop) %>% 
  drop_na()
  
adm1_data_full_wider <- adm1_data_full %>% 
  pivot_wider(values_from = 'ratio_gini', names_from = 'year')
 

adm1_pop_2023 <- adm1_data_full_pop %>% 
  filter(year == 2023) %>% 
  summarise(sumPop = sum(pop, na.rm = T))

total_pop_2023 <- global(subset(r_pop,34), fun = "sum", na.rm = TRUE)

adm1_pop_2023 <- adm1_data_full_pop %>% 
  filter(year == 2023) %>% 
  summarise(sumPop = sum(pop, na.rm = T)) %>% 
  mutate(percOfGloba = sumPop / total_pop_2023)

adm0_of_adm1 <- adm1_data_full %>% 
  distinct(iso3)

adm1_entries <- HDI_data %>% 
  filter(!level == 'National') %>% 
  drop_na()




