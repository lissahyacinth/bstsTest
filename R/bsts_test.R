#' Test a BSTS_Model - Highest Function level from bstsTest package
#' @export
#' 
#' @param df Dataframe
#' @param validate_n N Rows of the df will be used for cross valdiation
#' @param response Response variable name - will not accept lazy
#' @param date_variable Name of date variable column - will not accept lazy
#' @param na_fill - Fill for NA rows - defaults to 0 
#' @param nseasons - Seasonality of data - defaults to 7 - also takes "auto" to detect seasons automatically
#' @param trend_type - If using auto for nseasons, specify what type of seasonality exists [multiplicative/additive]
#' @param data_frequency - If using auto for nseasons, specify the frequency of the data. 
#' @param niter - Iterations to run BSTS 
#' @param target_variable - Name of target variable column 
#' @param group_variable - Name of column to group by 
#' @param model.options - Any extra BSTS Options to add in using BstsOptions()
#' @param rebag_vars - T/F - Sum all variables into a new predictor.
#' @param rebag_mean_vars - T/F Take the mean of all variables into a new prediction.
#' @param inclusion_probability Floor for including variables in output - doesn't affect model right now.
#' @importFrom stats predict decompose ts
#' @importFrom Metrics rmse
#' @importFrom data.table dcast
#' @importFrom bsts BstsOptions
#' @importFrom forecast findfrequency

bsts_test <- function(df = df,
                      validate_n = 20, 
                      date_variable,
                      response,
                      na_fill = 0,
                      nseasons = 7,
                      trend_type,
                      data_frequency, 
                      niter = 100,
                      target_variable,
                      group_variable,
                      model.options = BstsOptions(),
                      rebag_vars = FALSE,
                      rebag_mean_vars = FALSE,
                      inclusion_probability = 0.1){
  
  df = data.table::as.data.table(df)
  formatted_response = gsub(response, pattern = "`", replacement = "")
  
  #Ensure DF matches requirements
  if(class(df[,paste0(date_variable), with = FALSE][[1]]) != "Date"){
    tryCatch({
      df[,date_variable, with = FALSE] = as.Date(df[,paste0(date_variable), with = FALSE][[1]])
    }, 
    error = function(e){
      stop(sprintf("Failed to convert Date column (%s) to a date.", date_variable))
    })
  }
  if(class(df[,paste0(target_variable), with = FALSE][[1]]) != "numeric"){
    tryCatch({
      df[[target_variable]] = as.numeric(df[,paste0(target_variable), with = FALSE][[1]])
    }, 
    error = function(e){
      stop(sprintf("Failed to convert Target Variable column (%s) to a numeric.", target_variable))
    }
    )
  }
  
  #Generate Initial, Test, and Validation Datasets
  eval(parse(
    text=paste0(
      "init_df = df[!",
      date_variable, " %in% df[, .N, by = ", date_variable, "][,
      tail(",date_variable, ", validate_n)]]"))
    )
  
  eval(parse(
    text=paste0(
      "new_df = data.table::as.data.table(df)[", group_variable ," != \"", formatted_response, "\" & ",   
      date_variable, " %in% df[, .N, by = ", date_variable, "][,
      tail(",date_variable, ", validate_n)]]"))
    )
  
  eval(parse(
    text=paste0(
      "val_df = data.table::as.data.table(df)[", group_variable ," == \"", formatted_response, "\" & ",   
      date_variable, " %in% df[, .N, by = ", date_variable, "][order(", date_variable,")][,
      tail(",date_variable, ", validate_n)]]"))
    )  

  # Generate seasonality of data if auto is specified
  if(tolower(nseasons) == "auto") {
    if(missing(trend_type)){
      stop("Require a trend type [multiplicative,additive] for using auto seasons.")
    }
    if(missing(data_frequency)){
      stop("Requires a frequency - try 12 for monthly data, 52 for weekly, and 365 for daily")
    }
    # Detrend the data 
    all_data = ts(as.numeric(as.matrix(init_df)[as.matrix(init_df)[, 2] == formatted_response, ][, 3]), frequency = data_frequency)
    decomposed_data = decompose(all_data, type = trend_type)
    detrend_data = all_data-decomposed_data$trend
    nseasons = findfrequency(detrend_data)
      if(nseasons == 1){warning("Seasonality of 1 found, likely to be no seasonality")}
  }

  #Generate BSTS Model
  model.list <- bsts_create(df = init_df,
                            date_variable = date_variable, 
                            group_variable = group_variable, 
                            nseasons = nseasons, 
                            niter = niter,
                            response = response,
                            inclusion_probability = inclusion_probability,
                            target_variable = target_variable, 
                            model.options = model.options, 
                            rebag_vars = rebag_vars, 
                            rebag_mean_vars = rebag_mean_vars)
  
  
  # Reshape New Data for input into the model
  eval(parse(text=(paste0("new_df = new_df[,
                          .(\"t_var\" = `",target_variable,"`, 
                          \"g_var\" = `",group_variable,"`, 
                          \"d_var\" = `",date_variable,"`)]"))))
  
  # Also Reshape the Validation DF while we're here
  val_df = eval(parse(text=(paste0("val_df = val_df[,
                          .(\"t_var\" = `",target_variable,"`, 
                          \"g_var\" = `",group_variable,"`, 
                          \"d_var\" = `",date_variable,"`)]"))))[,
                                                                 .(t_var = sum(t_var)), 
                                                                 by = c("d_var", "g_var")][order(d_var)]
  
  # Add in Missing Predictors, because apparently that happens. 
  missing_preds = dimnames(structure(model.list$bsts.model$coefficients))[!gsub(dimnames(structure(model.list$bsts.model$coefficients)), pattern = "`", replacement = "") %in% unique(new_df$g_var)][2][[1]]
  for(X in 2:length(missing_preds)){
    init.col = missing_preds[X]
    new_row = data.frame(t_var = na_fill, 
                         g_var = gsub(init.col, pattern = "`", replacement = ""), stringsAsFactors = F,
                         d_var = unique(new_df$d_var)[1])
    new_df = as.data.table(rbind(new_row, as.data.frame(new_df)))
  }
  
  cast_new_df = dcast(new_df[,
                             .(t_var = sum(t_var)), 
                             by = c("d_var", "g_var")],
                      formula = d_var ~ g_var, fun.aggregate = sum, value.var = "t_var", fill = na_fill)
  
  # Add Rebag for New Data
  if(rebag_vars == TRUE){
    cast_new_df$Rebag = rowSums(cast_new_df[, -c(1,unlist(as.list(1:ncol(cast_new_df))[colnames(cast_new_df) == response])),with = FALSE])
  }
  if(rebag_mean_vars == TRUE){
    cast_new_df$Rebag_Mean = rowMeans(cast_new_df[, -c(1,unlist(as.list(1:ncol(cast_new_df))[colnames(cast_new_df) == response])),with = FALSE])
  }
  preds <- predict(model.list$bsts.model, newdata = cast_new_df)
  predictions = cbind(val_df, "Prediction" = preds$mean)
  
  return(list(predictions = predictions,
              raw_preds = preds,
              bsts.model = model.list$bsts.model, 
              predictors = model.list$predictors, 
              MAPE = MAPE(actual = predictions[["t_var"]], 
                          predicted = predictions[["Prediction"]]), 
              RMSE = rmse(actual = predictions[["t_var"]], 
                          predicted = predictions[["Prediction"]])))
}