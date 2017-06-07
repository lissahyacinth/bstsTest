#' @title MAPE
#' @description Mean Absolute Percentage Error of Forecast against Response
#' @param actual vector or list of reported/response values
#' @param predicted vector or list of predicted values

MAPE = function(actual, predicted){
  if(is.list(actual)){actual <- as.numeric(unlist(actual))}
  if(is.list(predicted)){predicted <- as.numeric(unlist(predicted))}
  mean(abs((actual - predicted) / actual))
}