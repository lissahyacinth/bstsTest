#' Test a BSTS_Model - Highest Function level from bstsTest package
#' @export
#' 
#' @param df Dataframe
#' @param validate_n N Rows of the df will be used for cross valdiation
#' @param response Response variable name - will not accept lazy
#' @param date_variable Name of date variable column - will not accept lazy
#' @param na_fill - Fill for NA rows - defaults to 0 
#' @param nseasons - Seasonality of data - defaults to 7
#' @param niter - Iterations to run BSTS 
#' @param target_variable - Name of target variable column 
#' @param group_variable - Name of column to group by 
#' @param model.options - Any extra BSTS Options to add in using BstsOptions()
#' @param rebag_vars - T/F - Add up all the other variables into an aggregate
#' @param inclusion_probability Floor for including variable in model 
#' @importFrom stats predict
#' @importFrom Metrics rmse
#' @importFrom data.table dcast
#' @importFrom bsts BstsOptions

bsts_test <- function(df = df,
                      validate_n = 20, 
                      date_variable,
                      response,
                      na_fill = 0,
                      nseasons = 7,
                      niter = 100,
                      target_variable,
                      group_variable,
                      model.options = BstsOptions(),
                      rebag_vars = FALSE,
                      inclusion_probability = 0.1){
  
  df = data.table::as.data.table(df)
  formatted_response = gsub(response, pattern = "`", replacement = "")
  
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
  #colnames(val_df)[3] = paste0("New_", colnames(val_df)[3])  
  
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
                            rebag_vars = rebag_vars)
  
  
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
                         d_var = unique(new_df$d_var)[1], 
                         g_var = gsub(init.col, pattern = "`", replacement = ""))
    new_df = rbind(new_row, new_df)
  }
  
  cast_new_df = dcast(new_df[,
                             .(t_var = sum(t_var)), 
                             by = c("d_var", "g_var")],
                      formula = d_var ~ g_var, fun.aggregate = sum, value.var = "t_var", fill = na_fill)
  
  # Add Rebag for New Data
  if(rebag_vars == TRUE){
    cast_new_df$Rebag = rowSums(cast_new_df[, -c(1,unlist(as.list(1:ncol(cast_new_df))[colnames(cast_new_df) == response])),with = FALSE])
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