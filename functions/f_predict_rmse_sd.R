
f_predict_rmse_sd <- function(distance) {
  predict(sd_model, newdata = data.frame(distance = distance))
}