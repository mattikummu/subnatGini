


### sensitivity of extrapolation to the length of the missing years

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
library(patchwork)


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
  distinct(iso3, cntry_code, .keep_all = T)


dataFull_NearlyFull <- read_csv("data_out/dataFull_NearlyFull.csv")




##### 2 prepare for extrapolation ----


# centroid of adm0

if (file.exists('data_GIS/p_adm0_centroids.gpkg')) {
  p_adm0_centroids <- vect('data_GIS/p_adm0_centroids.gpkg')
  
} else { # create it
  # 
  # v_cntryGIS <- terra::simplifyGeom(vect('results/swiidDataFilled_gis.gpkg'))
  # v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  # terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  # p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  # 
  # terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg')
  
  
  gadm_adm0 <- st_read('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>%
    rename(iso3 = GID_0) %>%
    filter(iso3 != "ALA") %>%
    filter(iso3 != "ATA") %>%
    left_join(cntryID[,c(1,3)])
  
  v_cntryGIS <- terra::simplifyGeom(vect(gadm_adm0))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  #terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg', overwrite = T)
}


# 
indicWider <- dataFull_NearlyFull 

# Create an empty results data frame
sens.results <- data.frame(
  iso3 = character(),
  case = character(),
  RMSE = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)

##### uncertainty/sensitivity analysis


source('functions/f_extrapol.R')

YrMissing <- seq(1,25,3)

for (iYrMissing in 1:length(YrMissing)) {
  
  print(paste0('Doing YrMissing = ', YrMissing[iYrMissing]))
  
  missingYr <- YrMissing[iYrMissing]
  
  columns_to_na <- names(dataFull_NearlyFull)[2:(2+missingYr)] 
  
  data_to_extrapol <- dataFull_NearlyFull %>% 
    #filter(iso3 == iso3_sel) %>% 
    mutate(across(all_of(columns_to_na), ~ NA))
  
  data_to_extrapol_long <- dataFull_NearlyFull %>% 
    #filter(iso3 == iso3_sel) %>% 
    pivot_longer(values_to = 'valueRep', names_to = 'year', -iso3)
  
  for (i in 1:nrow(dataFull_NearlyFull)) { # 
    
    # iso3 for country to be restricted
    iso3_sel <- as.character(dataFull_NearlyFull[i,1])
    
    
    
    # put data to zero for that iso3 for missingYr starting from end
    dataFull_NearlyFull_sel <- dataFull_NearlyFull %>% 
      filter(!iso3 == iso3_sel)
    
    
    # extrapolate a country in question
    tempExtrapolCntry <- f_extrapol(dataAll = indicWider, 
                                    dataFull = dataFull_NearlyFull_sel, 
                                    dataToBeExtrap = data_to_extrapol, 
                                    iCntry = i) %>% 
      left_join(data_to_extrapol_long %>% filter(iso3 == iso3_sel))
    
    
    # RMSE
    rmse <- sqrt(mean((tempExtrapolCntry$valueOrg_filled - tempExtrapolCntry$valueRep)^2))
    
    
    # R-squared
    r2 <- cor(tempExtrapolCntry$valueOrg_filled, tempExtrapolCntry$valueRep)^2
    
    # starndard error
    
    # Calculate residuals
    residuals <- tempExtrapolCntry$valueOrg_filled - tempExtrapolCntry$valueRep
    
    # Standard error of the residuals
    standard_error <- sd(residuals) / sqrt(length(residuals))
    
    sens.results <- rbind(sens.results, data.frame(
      iso3 = iso3_sel,
      case = missingYr,
      RMSE = rmse,
      R2 = r2,
      SE = standard_error,
      stringsAsFactors = FALSE
    ))
    
    
  }
  
  
}


sens.results %>% group_by(case) %>% summarise(rmse_median = median(RMSE))
sens.results %>% group_by(case) %>% summarise(R2_median = median(R2))
sens.results %>% group_by(case) %>% summarise(se_median = median(SE))


## plot

sens.results$case <- as.factor(sens.results$case)

p1 <- ggplot(sens.results, aes(x = case, y = RMSE)) +
  geom_boxplot(fill = "#69b3a2", color = "black") +
  labs(title = "RMSE by Case",
       x = "Number of years with missing data",
       y = "RMSE") +
  theme_minimal()  +
  ylim(0, 0.3)  # Setting the y-axis limits from 0 to 0.3

p2 <- ggplot(sens.results, aes(x = case, y = R2)) +
  geom_boxplot(fill = "#404080", color = "black", width = 0.5) +
  labs(title = "R² by Case",
       x = "Number of years with missing data",
       y = expression(R^2)) +
  theme_minimal()

# Create combined plot
combined_plot <- p1 + p2

# Save it
ggsave("figures/combined_plot.pdf", combined_plot, width = 150, height = 150, unit = "mm")
ggsave("figures/combined_plot.png", combined_plot, width = 150, height = 150, unit = "mm", dpi = 600)





### add still case for 0
# 
# sens.results_0 <- sens.results %>% 
#   filter(case ==1) %>% 
#   mutate(case = 0) %>% 
#   mutate(RMSE = 0) %>% 
#    mutate(R2 = 1) %>% 
#   mutate(SE = 0) 

# sens.results_filled <- sens.results %>% 
#   bind_rows(sens.results_0) %>% 
#   arrange(case, iso3)

fit_loglinear_rmse_curves <- function(rmse_df, n_fits = 1000) {
  fits <- map(1:n_fits, function(i) {
    
    # Stratified bootstrap: one sample per distance group
    sampled_data <- rmse_df %>%
      group_by(distance) %>%
      slice_sample(n = 1, replace = TRUE) %>%
      ungroup()
    
    # Fit log-linear model with fallback handling
    tryCatch({
      fit <- lm(log(rmse) ~ distance, data = sampled_data)
      return(fit)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Filter out failed fits
  fits[!sapply(fits, is.null)]
}


rmse_df <- sens.results %>% 
  rename(distance = case) %>% 
  rename(rmse = RMSE) %>% 
  as_tibble()

# Step 1: Fit ensemble of RMSE curves
rmse_fit_list <- fit_loglinear_rmse_curves(rmse_df, n_fits = 1000)

save(rmse_fit_list, file = "data_out/rmse_curves.RData")



# Extract coefficients from fitted models and generate predicted RMSE values
pred_data <- map_df(seq_along(rmse_fit_list), function(i) {
  fit <- rmse_fit_list[[i]]
  if (!is.null(fit)) {
    # Extract coefficients
    coefficients <- coef(fit)
    intercept <- coefficients[1]
    slope <- coefficients[2]
    
    # Generate predicted distances
    distances <- unique(rmse_df$distance)
    
    # Calculate predicted RMSE values
    predicted_rmse <- exp(intercept + slope * distances)
    
    return(tibble(model_id = i, distance = distances, predicted_rmse = predicted_rmse))
  }
})

# Plot using ggplot
p_predLines <- ggplot(pred_data, aes(x = distance, y = predicted_rmse)) +
  geom_line(aes(group = model_id), alpha = 0.1, color = "blue") +
  labs(x = "Number of years with missing data", y = "Predicted RMSE", title = "Ensemble of Log-Linear RMSE Curves") +
  theme_minimal()



# 
# # Save it
ggsave("figures/p_predLines.pdf", p_predLines, width = 100, height = 150, unit = "mm")


#### reported years -----

indicDataFilled <- read_csv("results/swiidDataFilled.csv")

nameIndic = 'gini_disp'

# 
indicWider <- indicDataFilled %>% 
  select(iso3, year, !!as.name(nameIndic)) %>% 
  arrange(iso3, year) %>% 
  distinct(iso3, year, .keep_all = T) %>% 
  pivot_wider(names_from = 'year', values_from = !!as.name(nameIndic)) %>% 
  select(c(iso3, as.character(seq(1990,2023,1))))

# Convert to long format
df_long <- indicWider %>%
  pivot_longer(
    cols = -iso3,
    names_to = "year",
    values_to = "value"
  ) %>% 
  arrange(iso3, year)

max_head_tail_na <- df_long %>%
  group_by(iso3) %>%
  summarise(
    tail_na = {
      vals <- is.na(value)
      ifelse(vals[1], rle(vals)$lengths[1], 0)
    },
    head_na = {
      rev_vals <- rev(is.na(value))
      ifelse(rev_vals[1], rle(rev_vals)$lengths[1], 0)
    },
    max_head_tail_na = pmax(tail_na, head_na),
    .groups = "drop"
  )

# Calculate percentiles
p50 <- quantile(max_head_tail_na$max_head_tail_na, 0.50, na.rm = TRUE)
p75 <- quantile(max_head_tail_na$max_head_tail_na, 0.75, na.rm = TRUE)
p95 <- quantile(max_head_tail_na$max_head_tail_na, 0.95, na.rm = TRUE)


# Plot
p_hist <- ggplot(max_head_tail_na, aes(x = max_head_tail_na)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
  
  # Add percentile lines
  geom_vline(xintercept = p50, color = "grey20", linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = p75, color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = p95, color = "red", linetype = "dashed", linewidth = 0.5) +
  
  # Add labels
  annotate("text", x = p50, y = Inf, label = "50th", vjust = -0.5, hjust = 1.5, color = "black", angle = 90, size = 3.5) +
  annotate("text", x = p75, y = Inf, label = "75th", vjust = -0.5, hjust = 1.5,color = "blue", angle = 90, size = 3.5) +
  annotate("text", x = p95, y = Inf, label = "95th", vjust = -0.5, hjust = 1.5,color = "red", angle = 90, size = 3.5) +
  
  labs(title = "Distribution of years without data per country",
       x = "Number of years with missing data",
       y = paste0("Number of Countries (n = ", nrow(indicWider),")")) +
  theme_minimal()
p_hist

ggsave(plot = p_hist, filename = "figures/hist_NA_years_adm0.pdf", width = 120, height = 120, units = "mm")
ggsave(plot = p_hist, filename = "figures/hist_NA_years_adm0.png", width = 120, height = 120, units = "mm", dpi = 300)

