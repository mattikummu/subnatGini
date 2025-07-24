f_simulate_rmse_lognormal <- function(distance, n_sim = 1000) {
  
  # ---- Function to Predict Mean RMSE ----
  f_predict_rmse_mean <- function(distance) {
    exp(predict(log_lm, newdata = data.frame(distance = distance)))
  }
  
  mu <- f_predict_rmse_mean(distance)
  sigma <- predict(sd_model, newdata = data.frame(distance = distance))
  
  # Convert mean/sd to lognormal parameters
  meanlog <- log(mu^2 / sqrt(mu^2 + sigma^2))
  sdlog <- sqrt(log(1 + (sigma^2 / mu^2)))
  
  rlnorm(n_sim, meanlog = meanlog, sdlog = sdlog)
}