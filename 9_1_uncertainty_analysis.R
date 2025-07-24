# Load necessary libraries


library(trend)
library(foreach)   # For parallel execution
library(doParallel) # For parallel backend

library(terra)
library(sf)

library(trend)
library(ggplot2)
library(MASS)
library(purrr)
library(dplyr)
library(tidyr)

# Your dataset


# set working directory the path that this script is located in
setwd('/Users/mkummu/R/subnatGini/')

### read data in -----

adm0_data <- read.csv('results/swiidDataFilled.csv') %>% 
  select(-gini_mkt, -gini_mkt_se, -Country) %>% 
  as_tibble() %>% 
  distinct(iso3, year, .keep_all = T)
#filter(iso3 == 'IND')

adm0_data_full <- read.csv('results/gini_adm0_DispMkt_extrap.csv') %>% 
  select(-gini_mkt) %>% 
  as_tibble()

adm0_withSlope <- read.csv("results_final/tabulated_adm0_gini_disp.csv") %>% 
  as_tibble()

# adm1_data <- read.csv('results/subnat_data_combined_ratio_reportedOnly.csv') %>% 
#   as_tibble() %>% 
#   select(iso3, cntry_code, GID_nmbr, s1989:s2021) %>% 
#   # if these do not exist
#   mutate(s2022 = NA) %>% mutate(s2023 = NA) %>% 
#   setNames(c('iso3', 'cntry_code', 'GID_nmbr', paste0(1989:2023))) %>% 
#   pivot_longer(-c('iso3', 'cntry_code', 'GID_nmbr'), values_to = 'ratio_gini', names_to = 'year' )

adm1_data_org <- read.csv('results/subnat_data_combined_ratio_reportedOnly.csv') %>%
  as_tibble()

# Function to conditionally add columns
add_missing_cols <- function(df, cols_to_add) {
  for (col_name in cols_to_add) {
    if (!col_name %in% names(df)) {
      df <- df %>% mutate(!!sym(col_name) := NA)
    }
  }
  return(df)
}

# Define the columns you want to ensure exist
years_to_check <- paste0("s", 2022:2023)

adm1_data <- adm1_data_org %>%
  # Conditionally add s2022 and s2023 if they don't exist
  add_missing_cols(years_to_check) %>%
  # Now that we are sure s2022 and s2023 exist (or were added as NA),
  # we can select the desired range of columns.
  # Using `all_of()` for precise column selection.
  select(iso3, cntry_code, GID_nmbr, all_of(paste0("s", 1989:2023))) %>%
  # Rename columns (assuming the order is now fixed due to `select`)
  setNames(c('iso3', 'cntry_code', 'GID_nmbr', paste0(1989:2023))) %>%
  # Pivot longer
  pivot_longer(-c('iso3', 'cntry_code', 'GID_nmbr'), values_to = 'ratio_gini', names_to = 'year')



adm1_data_full <- read.csv('results/subnat_data_combined_ratio_interp.csv') %>% 
  as_tibble() %>% 
  select(-cntry_code)

adm1_data_full_SWIID <- read_sf("results/vect_gini_disp_1990_2023.gpkg") %>% 
  st_drop_geometry()
  
adm1_data_full_WID <- read_sf("results/vect_gini_WID_1990_2023.gpkg") %>% 
  st_drop_geometry()



adm1_withSlope <- read.csv("results_final/tabulated_adm1_gini_disp.csv") %>% 
  as_tibble()

v_adm1 <- vect("results_final/polyg_adm1_gini_disp_1990_2023.gpkg")


### prepare data ---

adm0_data_allYears <- adm0_data %>% 
  select(-gini_disp_se) %>% 
  pivot_wider(values_from = 'gini_disp', names_from = 'year') %>% 
  select(iso3, paste0(1990:2023)) %>% 
  pivot_longer(-iso3, values_to = 'gini_disp', names_to = 'year') %>% 
  left_join(adm0_data %>% select(-gini_disp) %>% mutate(year = as.character(year) ))

adm0_mean_se <- adm0_data %>% 
  group_by(iso3) %>% 
  summarise(meanAdm0_se = mean(gini_disp_se, na.rm=T))

# define the increase of standard error with the distance from closest observation
# the function is created in script 8_sensitivityAna*.R



##### summarise MC -----

load('data_out/uncertainty_tables_v4_n500.RData')

## read in pop
r_pop <- subset(rast("data_gis/r_pop_GHS_1985_2025_5arcmin.tif"), 6:39)


# population for each admin unit
v_pop_adm1 <- terra::extract(r_pop, v_adm1, fun = sum, na.rm=T)


# combine adm 1 and adm 0

stats_table_comb <- stats_table_adm1 %>% 
  bind_rows(stats_table_adm0 %>% filter(!iso3 %in% slopes_table_adm1$iso3)) %>% 
  left_join(adm0_withSlope %>% select(iso3, cntry_code) %>% distinct(iso3, cntry_code, .keep_all = T))



df_adm1_pop <- as_tibble(v_adm1) %>% 
  select(iso3, admID) %>% 
  bind_cols(v_pop_adm1 %>% as_tibble() %>% select(-ID)) #%>% 
#set_names('iso3', 'admID', paste0(1990:2021)) 

slopes_table_comb <- slopes_table_adm1 %>% 
  bind_rows(slopes_table_adm0 %>% filter(!iso3 %in% slopes_table_adm1$iso3))


slopes_table_byAdmin <- slopes_table_comb %>%
  rowwise() %>%
  mutate(
    mean = mean(c_across(starts_with("V")), na.rm=T),
    sd  = sd(c_across(starts_with("V")), na.rm=T),
    p25 = quantile(c_across(starts_with("V")), 0.25, na.rm=T),
    p50 = quantile(c_across(starts_with("V")), 0.5, na.rm=T),
    p75 = quantile(c_across(starts_with("V")), 0.75, na.rm=T),
  ) %>%
  select(iso3,  GID_nmbr,mean, sd, p25, p50, p75 )


adm1_data_full_SWIID_long <- adm1_data_full_SWIID %>% 
  select(admID, iso3, slope) %>% 
  #rename(GID_nmbr = admID) %>% 
  pivot_longer(-c('iso3', 'admID'), names_to = 'run_n', values_to = 'slope_SWIID') %>% 
  distinct(admID, .keep_all = T)

adm1_data_full_WID_long <- adm1_data_full_WID %>% 
  select(admID, iso3, slope) %>% 
  #rename(GID_nmbr = admID) %>% 
  pivot_longer(-c('iso3', 'admID'), names_to = 'run_n', values_to = 'slope_WID') %>% 
  distinct(admID, .keep_all = T)




slopes_table_byAdmin_pop <- slopes_table_byAdmin %>%
  mutate(admID = GID_nmbr) %>%
  left_join(df_adm1_pop %>% select(admID, pop2023) %>% distinct(admID, pop2023, .keep_all = TRUE)) %>%
  rename(tpop2023 = pop2023)

summary_df <- slopes_table_byAdmin_pop %>%
  #pivot_longer(cols = starts_with("p"), names_to = "quantile", values_to = "trend") %>%
  mutate(
    trend_class = case_when(
      mean < 0 ~ "neg",
      mean == 0 ~ "zero",
      mean > 0 ~ "pos"
    )
  ) %>%
  #group_by(quantile, trend_class) %>%
  group_by(trend_class) %>%
  summarise(pop_sum = sum(tpop2023, na.rm = TRUE), .groups = "drop") %>%
  # group_by(quantile) %>%
  mutate(
    total_pop = sum(pop_sum),
    perc = pop_sum / total_pop
  ) 

summary_df_SWIID <- adm1_data_full_SWIID_long %>%
  left_join(df_adm1_pop %>% select(admID, pop2023) %>% distinct(admID, pop2023, .keep_all = TRUE)) %>%
  rename(tpop2023 = pop2023) %>% 
  #pivot_longer(cols = starts_with("p"), names_to = "quantile", values_to = "trend") %>%
  mutate(
    trend_class = case_when(
      slope_SWIID < 0 ~ "neg",
      slope_SWIID == 0 ~ "zero",
      slope_SWIID > 0 ~ "pos"
    )
  ) %>%
  #group_by(quantile, trend_class) %>%
  group_by(trend_class) %>%
  summarise(pop_sum = sum(tpop2023, na.rm = TRUE), .groups = "drop") %>%
  # group_by(quantile) %>%
  mutate(
    total_pop = sum(pop_sum),
    perc = pop_sum / total_pop
  ) 


summary_df_WID <- adm1_data_full_WID_long %>%
  left_join(df_adm1_pop %>% select(admID, pop2023) %>% distinct(admID, pop2023, .keep_all = TRUE)) %>%
  rename(tpop2023 = pop2023) %>% 
  #pivot_longer(cols = starts_with("p"), names_to = "quantile", values_to = "trend") %>%
  mutate(
    trend_class = case_when(
      slope_WID < 0 ~ "neg",
      slope_WID == 0 ~ "zero",
      slope_WID > 0 ~ "pos"
    )
  ) %>%
  #group_by(quantile, trend_class) %>%
  group_by(trend_class) %>%
  summarise(pop_sum = sum(tpop2023, na.rm = TRUE), .groups = "drop") %>%
  # group_by(quantile) %>%
  mutate(
    total_pop = sum(pop_sum),
    perc = pop_sum / total_pop
  ) 



neg_slopes_table <- slopes_table_comb %>%
  rowwise() %>%
  mutate(
    count_negative = sum(c_across(starts_with("V")) < 0, na.rm = TRUE),
    count_positive = sum(c_across(starts_with("V")) > 0, na.rm = TRUE),
    count_zero = sum(c_across(starts_with("V")) == 0, na.rm = TRUE)
  ) %>%
  select(iso3,  GID_nmbr,count_negative, count_positive, count_zero )

adm1_withSlope_uncertainty <- adm1_withSlope %>% 
  mutate(slope = ifelse(is.na(slope), 0, slope)) %>% 
  select(iso3, cntry_code, admID) %>% 
  distinct(iso3, cntry_code, admID, .keep_all = TRUE) %>% 
  filter(iso3 %in% unique(neg_slopes_table$iso3) ) %>% 
  mutate(GID_nmbr = admID) %>% 
  left_join(slopes_table_byAdmin %>% select(iso3, GID_nmbr, mean, sd, p25,p50,p75)) %>% 
  distinct(iso3, cntry_code, admID, .keep_all = TRUE) %>%
  left_join(neg_slopes_table)



slopes_table_long <- slopes_table_comb %>% 
  pivot_longer(-c('iso3', 'GID_nmbr'), names_to = 'run_n', values_to = 'slope') %>% 
  mutate(admID = GID_nmbr) %>% 
  left_join(df_adm1_pop %>% select(admID, pop2023) %>% distinct(admID, pop2023, .keep_all = TRUE)) %>% 
  mutate(neg_pop = ifelse(slope < 0, pop2023, 0)) %>% 
  mutate(pos_pop = ifelse(slope > 0, pop2023, 0)) %>% 
  mutate(zero_pop = ifelse(slope == 0, pop2023, 0))


slopes_table_long_summary <- slopes_table_long %>% 
  reframe(
    total = sum(neg_pop + pos_pop + zero_pop, na.rm=T),
    perc_neg = sum(neg_pop, na.rm=T) / total,
    perc_pos = sum(pos_pop, na.rm=T) / total,
    perc_zero = sum(zero_pop, na.rm=T) / total,
    .by = run_n
  ) %>% 
  reframe(across(starts_with("perc_"), list(quant = ~quantile(.x, probs = c(0.1, 0.5, 0.9), na.rm=T))))

save(stats_table_comb, 
     slopes_table_long_summary, slopes_table_long,adm1_withSlope_uncertainty,
     slopes_table_byAdmin_pop, summary_df,
     file = "data_out/uncertainty_results_v4_n500.RData")






