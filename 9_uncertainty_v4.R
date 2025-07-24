# Load necessary libraries
library(trend)
library(foreach)    # For parallel execution
library(doParallel) # For parallel backend

library(terra)
library(sf)

library(ggplot2)
library(MASS)
library(purrr)
library(dplyr)
library(tidyr)
library(truncnorm) # For truncated normal distribution

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

adm1_withSlope <- read.csv("results_final/tabulated_adm1_gini_disp.csv") %>%
  as_tibble()

v_adm1 <- vect("results_final/polyg_adm1_gini_disp_1990_2023.gpkg")

### prepare data ---

adm0_data_allYears <- adm0_data %>%
  select(-gini_disp_se) %>%
  pivot_wider(values_from = 'gini_disp', names_from = 'year') %>%
  select(iso3, paste0(1990:2023)) %>%
  pivot_longer(-iso3, values_to = 'gini_disp', names_to = 'year') %>%
  left_join(adm0_data %>% select(-gini_disp) %>% mutate(year = as.character(year)))

adm0_mean_se <- adm0_data %>%
  group_by(iso3) %>%
  summarise(meanAdm0_se = mean(gini_disp_se, na.rm=T))

# define the increase of standard error with the distance from closest observation
# the function is created in script 8_sensitivityAna*.R

load("data_out/rmse_curves.RData")

# 2. Predict RMSE across a range of distances for each fit
distance_seq <- seq(0, 32, by = 1)

rmse_curve_df <- map_dfr(seq_along(rmse_fit_list), function(i) {
  model <- rmse_fit_list[[i]]
  tibble(
    curve_id = i,
    distance = distance_seq,
    rmse = exp(predict(model, newdata = tibble(distance = distance_seq))) %>%
      replace(1, 0)  # enforce RMSE = 0 at distance 0
  )
})

# 3. Calculate median RMSE per distance
rmse_median_curve <- rmse_curve_df %>%
  group_by(distance) %>%
  summarise(rmse_median = median(rmse), .groups = "drop")

# 4. Plot the median curve
ggplot(rmse_median_curve, aes(x = distance, y = rmse_median)) +
  geom_line(color = "darkred", size = 1.2) +
  labs(
    title = "Median RMSE Curve from 1000 Log-Linear Fits",
    x = "Distance (years)",
    y = "Median RMSE"
  ) +
  theme_minimal()

# Convert to wide matrix: rows = distance (0–30), cols = curve_id
rmse_matrix <- rmse_curve_df %>%
  pivot_wider(names_from = curve_id, values_from = rmse) %>%
  select(-distance) %>%
  as.matrix()

# Store distances separately
distance_vector <- distance_seq

#### monte carlo analysis - preparation ------


# (compute_sen_slope function remains the same)
compute_sen_slope <- function(y, time) {
  # If y has all NA values or all identical values, return 0 or NA as appropriate
  if (all(is.na(y)) || length(unique(na.omit(y))) <= 1) {
    return(0) # Or return NA if you prefer to distinguish
  }
  
  df <- data.frame(y = y, time = time)
  
  # Check for sufficient non-NA data points for mblm
  if (sum(!is.na(df$y)) < 2) { # mblm needs at least two non-NA points
    return(0)
  }
  
  # Fit model within a tryCatch block to gracefully handle errors
  model_fit <- tryCatch({
    mblm::mblm(y ~ time, data = df)
  }, error = function(e) {
    # If mblm fails, return a dummy object that indicates no slope
    return(NULL)
  })
  
  if (is.null(model_fit)) {
    return(0)
  }
  
  tidy_res <- broom::tidy(model_fit)
  slope_row <- tidy_res %>% filter(term == "time")
  
  # Check if "time" term exists and p-value condition
  if (nrow(slope_row) == 0 || is.na(slope_row$p.value) || slope_row$p.value > 0.1) {
    return(0)
  } else {
    return(slope_row$estimate)
  }
}



monte_carlo_simulation <- function(iso, n_simulations, adm0_data_allYears, adm0_data_full, adm1_data, adm1_data_full, adm0_mean_se, rmse_matrix, distance_vector) {
  # Filter data for the current iso3
  adm0_data_iso <- adm0_data_allYears %>% filter(iso3 == iso)
  adm0_data_full_iso <- adm0_data_full %>% filter(iso3 == iso)
  adm1_data_iso <- adm1_data %>% filter(iso3 == iso)
  adm1_data_full_iso <- adm1_data_full %>% filter(iso3 == iso)
  
  # Pre-calculate common values for ADM0
  n_years_adm0 <- nrow(adm0_data_iso)
  time_index_adm0 <- 0:(n_years_adm0 - 1)
  rho <- 0.9 # Autocorrelation
  cov_matrix_adm0 <- rho^abs(outer(time_index_adm0, time_index_adm0, "-"))
  
  # Initialize lists to store results for this country
  simulated_adm0_list <- list()
  simulated_adm1_list <- list()
  
  # Lists to store slopes for each simulation run
  adm0_slopes_sim_list <- list()
  adm1_slopes_sim_list <- list() # Initialize even if not used for some countries
  
  # Flag to check if ADM1 data exists for this country
  has_adm1_data <- nrow(adm1_data_iso) > 0
  
  
  if (has_adm1_data) {
    # Pre-calculate common values for ADM1 only if data exists
    n_rows_adm1 <- nrow(adm1_data_full_iso)
    time_index_adm1 <- 0:(n_rows_adm1 - 1)
    cov_matrix_adm1 <- rho^abs(outer(time_index_adm1, time_index_adm1, "-"))
  }
  
  
  for (i in 1:n_simulations) {
    # --- Step 1: Simulate ADM0 data within Monte Carlo loop ---
    
    # Sample a rmse curve for this MC round
    curve_id_sample <- sample(1:ncol(rmse_matrix), 1)
    sampled_rmse_curve <- rmse_matrix[, curve_id_sample]
    
    # Calculate adm0_data_allYears_extSE for current simulation
    adm0_data_allYears_extSE_MC <- adm0_data_iso %>%
      mutate(year = as.numeric(year)) %>%
      group_by(iso3) %>%
      group_modify(~ {
        reported_indices <- which(!is.na(.x$gini_disp_se))
        .x %>%
          mutate(
            distance_to_reported = sapply(year, function(y) {
              distances <- abs(y - year[reported_indices])
              if (length(distances) > 0) {
                return(min(distances))
              } else {
                return(NA_integer_)
              }
            }),
            nearest_reported_index = sapply(year, function(y) {
              distances <- abs(y - year[reported_indices])
              if (length(distances) > 0) {
                nearest_index <- which.min(distances)
                return(reported_indices[nearest_index])
              } else {
                return(NA_integer_)
              }
            })
          )
      }) %>%
      ungroup() %>%
      # Join with sampled RMSE curve
      mutate(rmse_from_curve = sampled_rmse_curve[pmin(distance_to_reported + 1, length(sampled_rmse_curve))]) %>% # +1 because R index from 1
      group_by(iso3) %>%
      group_modify(~ {
        .x %>%
          mutate(
            adjusted_gini_disp_se = if_else(
              is.na(gini_disp_se),
              .x$gini_disp_se[nearest_reported_index] + rmse_from_curve,
              gini_disp_se
            )
          )
      }) %>%
      ungroup() %>%
      select(-rmse_from_curve)
    
    
    # Prepare adm0 data for simulation (similar to original adm0_data_full_se)
    adm0_data_full_se_MC <- adm0_data_full_iso %>%
      left_join(adm0_data_allYears_extSE_MC %>% select(iso3, year, nearest_reported_index, distance_to_reported, adjusted_gini_disp_se)) %>%
      rename(gini_disp_adm0 = gini_disp) %>%
      rename(adjusted_gini_disp_se_adm0 = adjusted_gini_disp_se) %>%
      mutate(gini_disp_se_adm0 = ifelse(distance_to_reported == 0, adjusted_gini_disp_se_adm0, NA)) # This column is used for reported SEs
    
    # Precompute lookup index for ADM0
    lookup_index_adm0 <- as.integer(adm0_data_full_se_MC$distance_to_reported) + 1
    lookup_index_adm0[is.na(lookup_index_adm0)] <- 1
    
    # Precompute fallback SEs for ADM0
    nearest_se_adm0 <- adm0_data_full_se_MC$gini_disp_se_adm0[adm0_data_full_se_MC$nearest_reported_index]
    
    # Calculate SEs for ADM0 simulation
    simulated_se_adm0 <- ifelse(
      is.na(adm0_data_full_se_MC$gini_disp_se_adm0),
      nearest_se_adm0 + sampled_rmse_curve[pmin(lookup_index_adm0, length(sampled_rmse_curve))],
      adm0_data_full_se_MC$adjusted_gini_disp_se_adm0
    )
    
    # Simulate correlated noise for ADM0
    error_vector_adm0 <- MASS::mvrnorm(1, mu = rep(0, n_years_adm0), Sigma = cov_matrix_adm0) * simulated_se_adm0
    
    # Add noise to base values for ADM0
    simulated_gini_adm0 <- adm0_data_full_se_MC$gini_disp_adm0 + error_vector_adm0
    simulated_adm0_current_sim <- tibble(
      iso3 = iso,
      year = adm0_data_full_se_MC$year,
      simulated_gini_disp_adm0 = simulated_gini_adm0
    )
    simulated_adm0_list[[i]] <- simulated_adm0_current_sim
    
    # Calculate Sen's slope for the current ADM0 simulation
    adm0_slope_current_sim <- simulated_adm0_current_sim %>%
      mutate(time_for_slope = year - min(year)) %>% # Adjust years to be relative to start
      group_by(iso3) %>%
      summarise(
        slope = compute_sen_slope(simulated_gini_disp_adm0, time_for_slope),
        .groups = "drop"
      ) %>%
      mutate(simulation_run = i) # Add simulation identifier
    adm0_slopes_sim_list[[i]] <- adm0_slope_current_sim
    
    
    # --- Step 2: Simulate ADM1 data within Monte Carlo loop (Conditional) ---
    if (has_adm1_data) {
      # Sample se_increase for each adm1 area in this MC round
      # Truncated normal distribution between 0.001 and 0.003
      adm1_gids <- adm1_data_iso %>% distinct(GID_nmbr) %>% pull(GID_nmbr)
      se_increase_sample <- rtruncnorm(
        n = length(adm1_gids),
        a = 0.001, b = 0.003, mean = 0.002, sd = 0.0005 # You can adjust mean and sd as needed
      )
      names(se_increase_sample) <- as.character(adm1_gids) # Ensure names are characters for lookup
      
      # Prepare simulated ADM0 SEs for joining to ADM1 data
      current_adm0_adjusted_se <- adm0_data_full_se_MC %>%
        select(iso3, year, adjusted_gini_disp_se_adm0) # Select only what's needed for join
      
      # Calculate adm1_data_SE for current simulation
      adm1_data_SE_MC <- adm1_data_iso %>%
        mutate(year = as.numeric(year)) %>%
        filter(!year == 1989) %>%
        select(-cntry_code) %>%
        left_join(current_adm0_adjusted_se, by = c("iso3", "year")) %>% # JOIN HERE
        mutate(adm1_se_base = ifelse(!is.na(ratio_gini), adjusted_gini_disp_se_adm0, NA)) %>% # Use adjusted_gini_disp_se_adm0
        group_by(GID_nmbr) %>%
        group_modify(~ {
          current_gid_nmbr <- .y$GID_nmbr[1] # Get the current GID_nmbr
          current_se_increase <- se_increase_sample[as.character(current_gid_nmbr)]
          
          reported_indices <- which(!is.na(.x$adm1_se_base)) # Use adm1_se_base for reported checks
          .x %>%
            mutate(
              distance_to_reported = sapply(year, function(y) {
                distances <- abs(y - year[reported_indices])
                if(length(distances) > 0) {
                  return(min(distances))
                } else {
                  return(NA_integer_)
                }
              }),
              nearest_reported_index = sapply(year, function(y) {
                distances <- abs(y - year[reported_indices])
                if(length(distances) > 0) {
                  nearest_index <- which.min(distances)
                  return(reported_indices[nearest_index])
                } else {
                  return(NA_integer_)
                }
              }),
              adjusted_gini_disp_se = if_else(
                is.na(adm1_se_base), # If adm1_se_base is NA (meaning no direct reported Gini for ADM1 for that year)
                .x$adm1_se_base[nearest_reported_index] + current_se_increase + current_se_increase * distance_to_reported, # Add base and distance effect
                adm1_se_base + current_se_increase # If reported (adm1_se_base is NOT NA), just add base current_se_increase
              )
            )
        }) %>%
        ungroup() %>%
        select(-nearest_reported_index, -adm1_se_base) # Remove helper columns
      
      
      # Combine with full ADM1 data and simulated ADM0 gini for this run
      # Crucially, join with the *current simulation's* ADM0 Gini values
      adm1_data_full_se_MC <- adm1_data_full_iso %>%
        left_join(simulated_adm0_current_sim %>% rename(gini_disp_adm0 = simulated_gini_disp_adm0),
                  by = c("iso3", "year")) %>%
        # Need to bring in the adjusted SE from ADM0 as well for ratio calculation
        left_join(adm0_data_full_se_MC %>% select(iso3, year, adjusted_gini_disp_se_adm0),
                  by = c("iso3", "year")) %>%
        mutate(gini_disp_adm1_base = ratio_gini * gini_disp_adm0) %>% # Base Gini for ADM1 before adding noise
        left_join(adm1_data_SE_MC %>% select(GID_nmbr, year, adjusted_gini_disp_se) %>% rename(adjusted_gini_disp_se_adm1 = adjusted_gini_disp_se)) %>%
        mutate(sigma_ratio = ratio_gini * sqrt((adjusted_gini_disp_se_adm1 / gini_disp_adm1_base)^2 + (adjusted_gini_disp_se_adm0 / gini_disp_adm0)^2))
      
      
      # Prepare data for ADM1 simulation
      data_in_adm1 <- adm1_data_full_se_MC %>%
        mutate(
          G_adm1 = ratio_gini * gini_disp_adm0
        )
      
      # ADM1-level standard errors for this simulation
      sigma_adm1 <- sqrt((data_in_adm1$ratio_gini * data_in_adm1$adjusted_gini_disp_se_adm0)^2 + # This uses ADM0's SE
                           (data_in_adm1$gini_disp_adm0 * data_in_adm1$sigma_ratio)^2) # This is based on ratio SE
      
      # AR(1) error generation for ADM1
      error_vector_adm1 <- MASS::mvrnorm(1, mu = rep(0, n_rows_adm1), Sigma = cov_matrix_adm1) * sigma_adm1
      
      # Add noise to ADM1
      simulated_gini_adm1 <- data_in_adm1$G_adm1 + error_vector_adm1
      simulated_adm1_current_sim <- tibble(
        iso3 = iso,
        GID_nmbr = data_in_adm1$GID_nmbr,
        year = data_in_adm1$year,
        simulated_gini_disp_adm1 = simulated_gini_adm1
      )
      simulated_adm1_list[[i]] <- simulated_adm1_current_sim
      
      # Calculate Sen's slope for each ADM1 unit in the current simulation
      adm1_slope_current_sim <- simulated_adm1_current_sim %>%
        mutate(time_for_slope = year - min(year)) %>% # Adjust years to be relative to start
        group_by(iso3, GID_nmbr) %>%
        summarise(
          slope = compute_sen_slope(simulated_gini_disp_adm1, time_for_slope),
          .groups = "drop"
        ) %>%
        mutate(simulation_run = i) # Add simulation identifier
      adm1_slopes_sim_list[[i]] <- adm1_slope_current_sim
    }
  }
  
  # --- Aggregate and Process Results for ADM0 ---
  # Combine all simulated ADM0 data across simulations
  all_simulated_adm0_data <- bind_rows(simulated_adm0_list)
  
  simulated_adm0_stats <- all_simulated_adm0_data %>%
    group_by(iso3, year) %>%
    summarise(
      mean_gini_disp_adm0 = mean(simulated_gini_disp_adm0, na.rm = TRUE),
      sd_gini_disp_adm0 = sd(simulated_gini_disp_adm0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # **MODIFICATION HERE**: No longer summarise mean/sd slopes, just bind rows of individual slopes
  simulated_adm0_slopes_per_run <- bind_rows(adm0_slopes_sim_list)
  
  
  # --- Aggregate and Process Results for ADM1 (Conditional) ---
  if (has_adm1_data) {
    # Combine all simulated ADM1 data across simulations
    all_simulated_adm1_data <- bind_rows(simulated_adm1_list)
    
    simulated_adm1_stats <- all_simulated_adm1_data %>%
      group_by(iso3, GID_nmbr, year) %>%
      summarise(
        mean_gini_disp_adm1 = mean(simulated_gini_disp_adm1, na.rm = TRUE),
        sd_gini_disp_adm1 = sd(simulated_gini_disp_adm1, na.rm = TRUE),
        .groups = "drop"
      )
    
    # **MODIFICATION HERE**: No longer summarise mean/sd slopes, just bind rows of individual slopes
    simulated_adm1_slopes_per_run <- bind_rows(adm1_slopes_sim_list)
  } else {
    # If no ADM1 data, return empty tibbles for ADM1 results,
    # ENSURING GID_nmbr IS OF TYPE INTEGER
    simulated_adm1_stats <- tibble(
      iso3 = character(),
      GID_nmbr = integer(),
      year = numeric(),
      mean_gini_disp_adm1 = numeric(),
      sd_gini_disp_adm1 = numeric()
    )
    simulated_adm1_slopes_per_run <- tibble( # Renamed variable to reflect content
      iso3 = character(),
      GID_nmbr = integer(),
      slope = numeric(),
      simulation_run = integer()
    )
  }
  
  
  list(
    adm0_stats = simulated_adm0_stats,
    adm0_slopes = simulated_adm0_slopes_per_run, # Return per-run slopes
    adm1_stats = simulated_adm1_stats,
    adm1_slopes = simulated_adm1_slopes_per_run  # Return per-run slopes
  )
}

### run the functions ----
n_mc = 500
set.seed(42)

# Define iso3_list (assuming both ADM0 and ADM1 analysis will use the same set of countries)
iso3_list <- unique(c(adm0_data_full$iso3, adm1_data_full$iso3))
# iso3_list <- c("AGO", "FIN") # For testing a subset

# Initialize results containers

all_stats_adm0 <- list()
all_slopes_adm0 <- list() # This will collect per-run slopes
all_stats_adm1 <- list()
all_slopes_adm1 <- list() # This will collect per-run slopes

runNmb = 1 # Not used for actual simulation runs, but perhaps for tracking progress if you want

for (iso in iso3_list) {
  cat("Running simulation for", iso, "\n")
  result <- monte_carlo_simulation(
    iso = iso,
    n_simulations = n_mc, # Make sure n_mc is defined (e.g., n_mc = 10)
    adm0_data_allYears = adm0_data_allYears,
    adm0_data_full = adm0_data_full,
    adm1_data = adm1_data,
    adm1_data_full = adm1_data_full,
    adm0_mean_se = adm0_mean_se, # This is no longer used for ADM1 SE calculations within MC, but needs to be passed
    rmse_matrix = rmse_matrix,
    distance_vector = distance_vector
  )
  
  # Store results
  all_stats_adm0[[iso]] <- result$adm0_stats
  all_slopes_adm0[[iso]] <- result$adm0_slopes
  all_stats_adm1[[iso]] <- result$adm1_stats
  all_slopes_adm1[[iso]] <- result$adm1_slopes
}

# Combine all results into dataframes
stats_table_adm0 <- bind_rows(all_stats_adm0)

# -slopes ---
slopes_table_adm0 <- bind_rows(all_slopes_adm0) %>%
  pivot_wider(
    names_from = simulation_run,
    values_from = slope,
    names_prefix = "V" # This will create V1, V2, V3...
  ) %>%
  # You might need to adjust GID_nmbr joining based on your actual adm0_withSlope structure
  # If adm0_withSlope has 'cntry_code' that directly maps to the row, it might be an issue.
  # Assuming 'cntry_code' in adm0_withSlope is equivalent to the GID_nmbr for ADM0
  left_join(adm0_withSlope %>% select(iso3, GID_nmbr = cntry_code), by = "iso3")


stats_table_adm1 <- bind_rows(all_stats_adm1)

# slopes ---
slopes_table_adm1 <- bind_rows(all_slopes_adm1) %>%
  pivot_wider(
    names_from = simulation_run,
    values_from = slope,
    names_prefix = "V"
  )

# Now, slopes_table_adm0 and slopes_table_adm1 should be in the wide format you desired.

save(slopes_table_adm1, 
     stats_table_adm1, 
     slopes_table_adm0,
     stats_table_adm0,
     file = "data_out/uncertainty_tables_v4_n500.RData")
