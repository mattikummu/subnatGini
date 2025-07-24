

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
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T)

load(file = "data_out/uncertainty_results_v4_n500.RData")

sf_gini_disp_adm1 <- read_sf('results/vect_gini_disp_1990_2023.gpkg') %>% 
  filter(!iso3 == 'ATA')

sf_adm0 <- read_sf("data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

### 3 plot -----


source('functions/f_Plot_sf_abs.R')
source('functions/f_Plot_sf_abs_trendAgr.R')


source('functions/f_Plot_sf_trend.R')

### slope uncertainty

nmbrRuns <- n_distinct(slopes_table_long$run_n)


sf_slope_uncert_adm1 <- sf_gini_disp_adm1 %>%
  select(admID, iso3) %>%
  distinct(admID, .keep_all = TRUE) %>% # 'T' is equivalent to TRUE
  left_join(adm1_withSlope_uncertainty, by = "admID") %>% # Added 'by' argument for clarity if not inferred
  left_join(sf_gini_disp_adm1 %>% st_drop_geometry() %>% select(admID, slope) ) %>% 
  rowwise() %>% # This tells dplyr to perform the following operations row by row
  mutate(mean = 34*mean) %>% 
  mutate(sd = 34*sd) %>% 
  mutate(p50 = ifelse(p50 ==0, NA, 34*p50)) %>% 
  mutate(
    # Step 1: Find the maximum value among the three count columns for the current row
    # na.rm = TRUE handles cases where there might be NA values in your count columns
    max_overall_count = max(c(count_negative, count_positive, count_zero), na.rm = TRUE),
    
    # Step 2: Determine the value for the new 'max_count_id' column based on conditions
    # case_when evaluates conditions sequentially and assigns the value from the first TRUE condition
    max_count_id = case_when(
      # Condition 1: If 'count_negative' is the highest value (or tied for highest).
      # If count_negative equals the overall maximum, it means it's among the highest.
      count_negative == max_overall_count ~ -1,
      
      # Condition 2: If 'count_positive' is the highest value (and not covered by the negative condition).
      # This condition will only be met if count_positive is the max AND count_negative was NOT the max.
      count_positive == max_overall_count ~ 1,
      
      # Condition 3: If 'count_zero' is the highest value (and not covered by previous conditions).
      count_zero == max_overall_count ~ 0,
      
      # Fallback: For any other cases (e.g., if all counts are NA for a row, or unexpected values)
      TRUE ~ NA_real_ # Assign NA (numeric type) if no conditions are met
    )
  ) %>%
  ungroup() %>% # It's crucial to ungroup after using rowwise() to return to a normal data frame structure
  mutate(
    # Existing 'chosen_count' calculation based on max_count_id
    chosen_count = case_when(
      max_count_id < 0 ~ count_negative * (-1) / nmbrRuns, # Assuming 'nmbrRuns' is available in your data
      max_count_id > 0 ~ count_positive / nmbrRuns,
      max_count_id == 0 ~ count_zero * 0,
      TRUE ~ NA_integer_ # Handles any other cases, e.g., NA values in p50
    ),
    
    # --- NEW COLUMN ADDITION START ---
    # New column: 'chosen_count_by_mean_slope'
    # It chooses the count based on the sign of the 'mean' slope
    chosen_count_by_mean_slope = case_when(
      mean < 0 ~ count_negative * (-1) / nmbrRuns,   # If mean slope is negative, choose count_negative
      mean > 0 ~ count_positive / nmbrRuns,   # If mean slope is positive, choose count_positive
      mean == 0 ~ count_zero * 0,      # If mean slope is zero, choose count_zero
      TRUE ~ NA_integer_           # Handles any other cases, like NA values in 'mean'
    ) ,
    # It chooses the count based on the sign of the 'mean' slope
    chosen_count_by_median_slope_neg = case_when(
      slope < 0 ~ count_negative * (-1) / nmbrRuns,   # If mean slope is negative, choose count_negative
      slope > 0 ~ NA,   # If mean slope is positive, choose count_positive
      slope == 0 ~ NA,      # If mean slope is zero, choose count_zero
      TRUE ~ NA_integer_           # Handles any other cases, like NA values in 'mean'
    ),
    chosen_count_by_median_slope_pos = case_when(
      slope < 0 ~ NA,   # If mean slope is negative, choose count_negative
      slope > 0 ~ count_positive / nmbrRuns,   # If mean slope is positive, choose count_positive
      slope == 0 ~ NA,      # If mean slope is zero, choose count_zero
      TRUE ~ NA_integer_           # Handles any other cases, like NA values in 'mean'
    ),
    chosen_count_by_median_slope_zero = case_when(
      slope < 0 ~ NA,   # If mean slope is negative, choose count_negative
      slope > 0 ~ NA,   # If mean slope is positive, choose count_positive
      slope == 0 ~ count_zero / nmbrRuns,      # If mean slope is zero, choose count_zero
      TRUE ~ NA_integer_           # Handles any other cases, like NA values in 'mean'
    ),
    
    # --- NEW COLUMN ADDITION END ---
  ) %>%
  # Optional: Remove the temporary 'max_overall_count' column if you don't need it
  select(-max_overall_count)

# View the updated data frame (optional)
 print(sf_slope_uncert_adm1)

# minGini <- quantile( sf_slope_uncert_adm1$chosen_count, .05, na.rm=T)
# maxGini <- quantile( sf_slope_uncert_adm1$chosen_count, .95, na.rm=T) 
# 
# giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
#                   plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
#                   by= 0.1) 

 
 
 sf_gini_uncert_adm1 <- sf_gini_disp_adm1 %>%
   select(admID, iso3) %>%
   mutate(GID_nmbr = admID) %>%
   distinct(admID, .keep_all = TRUE) %>% # Ensures unique admID rows on the left side
   left_join(
     stats_table_comb %>%
       mutate(GID_nmbr = ifelse(is.na(GID_nmbr), cntry_code, GID_nmbr)) %>% 
       mutate(mean_gini_disp_adm1 = ifelse(is.na(mean_gini_disp_adm1), mean_gini_disp_adm0, mean_gini_disp_adm1)) %>% 
       select(GID_nmbr, year, mean_gini_disp_adm1) %>%
       distinct(GID_nmbr, year, .keep_all = TRUE) %>% # Ensures unique GID_nmbr-year combinations for pivot_wider
       pivot_wider(names_from = 'year', values_from = 'mean_gini_disp_adm1'), # FIX: Removed -GID_nmbr
     by = "GID_nmbr" # Explicitly state the join key for clarity
   )
 
 sf_sd_uncert_adm1 <- sf_gini_disp_adm1 %>%
   select(admID, iso3) %>%
   mutate(GID_nmbr = admID) %>%
   distinct(admID, .keep_all = TRUE) %>% # Ensures unique admID rows on the left side
   left_join(
     stats_table_comb %>%
       mutate(GID_nmbr = ifelse(is.na(GID_nmbr), cntry_code, GID_nmbr)) %>% 
       mutate(sd_gini_disp_adm1 = ifelse(is.na(sd_gini_disp_adm1), sd_gini_disp_adm0, sd_gini_disp_adm1)) %>% 
       select(GID_nmbr, year, sd_gini_disp_adm1) %>%
       distinct(GID_nmbr, year, .keep_all = TRUE) %>% # Ensures unique GID_nmbr-year combinations for pivot_wider
       pivot_wider(names_from = 'year', values_from = 'sd_gini_disp_adm1'), # FIX: Removed -GID_nmbr
     by = "GID_nmbr" # Explicitly state the join key for clarity
   )
 
 #### plot gini and sd ------
 
 minGini <- quantile( sf_gini_uncert_adm1[['2023']], .05, na.rm=T)
 maxGini <- quantile( sf_gini_uncert_adm1[['2023']], .95, na.rm=T) 
 
 giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                   plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                   by= 0.02) 
 
 
 # minSlope <- quantile(sf_gini_disp$slopeGiniDisp, .05, na.rm = T)
 # maxSlope <- quantile(sf_gini_disp$slopeGiniDisp, .95, na.rm = T)
 # 
 # slopeRange <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ), 
 #                    plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
 #                    by= 0.01) 
 
 
 giniPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "glasgow")
 
 p_gini_mean_1990 <- f_Plot_sf_abs(sf_gini_uncert_adm1,'1990',giniRange, colPal = giniPal )
 p_gini_mean_2021 <- f_Plot_sf_abs(sf_gini_uncert_adm1,'2023',giniRange, colPal = giniPal )
 
 
 
 
 minSD_gini <- quantile( c(sf_sd_uncert_adm1[['2023']],sf_sd_uncert_adm1[['1990']]), .05, na.rm=T)
 maxSD_gini <- quantile( c(sf_sd_uncert_adm1[['2023']],sf_sd_uncert_adm1[['1990']]), .95, na.rm=T) 
 
 sdRange_gini <- seq( plyr::round_any( minSD_gini,accuracy=0.05,f=floor ), 
                   plyr::round_any( maxSD_gini,accuracy=0.05,f=ceiling ) ,
                   by= 0.01) 
 
 
 
 sdPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "imola")
 
 p_gini_sd_1990 <- f_Plot_sf_abs(sf_sd_uncert_adm1,'1990',sdRange_gini, colPal = sdPal )
 p_gini_sd_2021 <- f_Plot_sf_abs(sf_sd_uncert_adm1,'2023',sdRange_gini, colPal = sdPal )
 
 
 
 
 
 #### plot slope -----
# minSlope_Agr <- quantile(sf_slope_uncert_adm1$chosen_count_by_median_slope, .05, na.rm = T)
# maxSlope_Agr <- quantile(sf_slope_uncert_adm1$chosen_count_by_median_slope, .95, na.rm = T)
# 
# slopeRange_Agr <- seq( plyr::round_any( minSlope_Agr,accuracy=0.01,f=floor ),
#                    plyr::round_any( maxSlope_Agr,accuracy=0.01,f=ceiling ) ,
#                    by= 0.1)

slopeRange_Agr <- seq(-1,1,.1)

slopePal_neg <- scico(9, begin = 0.1, end = 0.5,direction = -1, palette = "vik")
slopePal_pos <- scico(9, begin = 0.5, end = 0.9,direction = 1, palette = "vik")
# slopePal_neg <- scico(9, begin = 0.5, end = 0.9,direction = 1, palette = "bam")
# slopePal_pos <- scico(9, begin = 0.1, end = 0.5,direction = -1, palette = "bam")
slopePal_zero <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "grayC")

p_giniAdm1_agreedSlope <- f_Plot_sf_abs_trendAgr(sf_slope_uncert_adm1,'chosen_count_by_median_slope_neg',seq(-1,0,.1), colPal = slopePal_neg ) +
  f_Plot_sf_abs_trendAgr(sf_slope_uncert_adm1,'chosen_count_by_median_slope_pos',seq(0,1,.1), colPal = slopePal_pos ) +
  f_Plot_sf_abs_trendAgr(sf_slope_uncert_adm1,'chosen_count_by_median_slope_zero',seq(0,1,.1), colPal = slopePal_zero )





minSlope <- quantile( sf_slope_uncert_adm1$mean, .05, na.rm=T)
maxSlope <- quantile( sf_slope_uncert_adm1$mean, .95, na.rm=T) 

meanSlopeRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                  by= 0.01) 

meanSlopeRange <- seq(-0.12, 0.12, .0125)

slopePal <- scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "vik")

p_slope_mean <- f_Plot_sf_abs(sf_slope_uncert_adm1,'mean',meanSlopeRange, colPal = slopePal )


# median slope


minSlope <- quantile( sf_slope_uncert_adm1$p50, .05, na.rm=T)
maxSlope <- quantile( sf_slope_uncert_adm1$p50, .95, na.rm=T) 

meanSlopeRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                       plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                       by= 0.01) 

meanSlopeRange <- seq(-0.12, 0.12, .0125)

slopePal <- scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "vik")

p_slope_median <- f_Plot_sf_abs(sf_slope_uncert_adm1,'p50',meanSlopeRange, colPal = slopePal )




minSD <- quantile( sf_slope_uncert_adm1[['sd']], .05, na.rm=T)
maxSD <- quantile( sf_slope_uncert_adm1[['sd']], .95, na.rm=T) 

sdRange <- seq( plyr::round_any( minSD,accuracy=0.01,f=floor ), 
                plyr::round_any( maxSD,accuracy=0.01,f=ceiling ) ,
                by= 0.01) 



sdPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "imola")

p_slope_SD <- f_Plot_sf_abs(sf_slope_uncert_adm1,'sd',sdRange, colPal = sdPal )


#### save figures -----

pathDir <- 'figures/figUncert/'


if (dir.exists(pathDir)) {
  
} else {
  dir.create(pathDir)  
}



layers <- list(p_slope_SD,
               p_slope_mean,
               p_slope_median,
               p_giniAdm1_agreedSlope,
               p_gini_sd_2021,
               p_gini_mean_2021,
               p_gini_sd_1990,
               p_gini_mean_1990
               )

nameLayers <- c('p_slope_SD',
                'p_slope_mean',
                'p_slope_median',
                'p_giniAdm1_agreedSlope',
                'p_gini_sd_2021',
                'p_gini_mean_2021',
                'p_gini_sd_1990',
                'p_gini_mean_1990')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  p_fig_leg <- layers[[i]]
  
  tmap_save(p_fig,filename = paste0(pathDir,'fig_',nameLayers[i],'.png'),width = 80, height = 35.51, units='mm', dpi = 600)
  tmap_save(p_fig_leg,filename = paste0(pathDir,'fig_legend_',nameLayers[i],'.png'),width = 80, height = 35.51, units='mm', dpi = 600)
  
}


