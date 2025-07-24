f_predict_rmse_mean <- function(distance) {
  exp(predict(log_lm, newdata = data.frame(distance = distance)))
}