#' Create a bsts_model
#' 
#' @export
#' @param df Dataframe
#' @param date_variable Column name containing date 
#' @param response Group that is being forecast
#' @param nseasons Seasonality of data - will be assumed if not provided
#' @param niter Number of MCMC iterations to attempt 
#' @param target_variable Column name containing metric to be forecast
#' @param group_variable Column name indicating group names - will build into regressors
#' @param model.options Additional options for BSTS
#' @param rebag_vars Create pseudo aggregate variable of other regressors
#' @param rebag_mean_vars Create psuedo mean variable of other regressors
#' @param inclusion_probability Minimum probability of inclusion in final model to show in returned predictors
#' @return list object - bsts.model, data.frame of predictors
#' @importFrom stats var sd median
#' @importFrom bsts AddLocalLevel BstsOptions bsts 
#' @importFrom zoo zoo

bsts_create = function(df,
                       date_variable,
                       response,
                       nseasons,
                       niter = 100,
                       target_variable,
                       group_variable,
                       model.options = BstsOptions(),
                       rebag_vars = FALSE,
                       rebag_mean_vars = FALSE,
                       inclusion_probability = 0.01){
  
  eval(parse(text=(paste0("df_init = data.table::as.data.table(df)[,
                          .(\"t_var\" = `",target_variable,"`, 
                          \"g_var\" = `",group_variable,"`, 
                          \"d_var\" = `",date_variable,"`)]"))))
  cast_df = data.table::dcast(data.table::as.data.table(df_init)[,
                          .(t_var = sum(t_var)), 
                          by = c("d_var", "g_var")],
                  formula = d_var ~ g_var, fun.aggregate = sum, value.var = "t_var")
  

  ### Remove NZV Variables ####
  column_variance = apply(cast_df[,2:ncol(cast_df)], function(x){
    if(length(x[x!=0 & !is.na(x)])/length(x) < 0.60 | any(is.infinite(x))){return(0)}
    # Removing outliers from the data to prevent variance being skewed during rescale. 
    x = x[(x <= median(x, na.rm=T) + 3*sd(x[!is.infinite(x)], na.rm = T)) & (x >= median(x, na.rm=T) - 3*sd(x[!is.infinite(x)], na.rm = T))]
    var((x[!is.infinite(x)] - min(x[x!=0 & !is.infinite(x)], na.rm =T))/max(x[!is.infinite(x)]-min(x[x!=0 & !is.infinite(x)], na.rm = T), na.rm =T), na.rm =T)
    }, MARGIN = 2)
  if(response %in% names(column_variance[column_variance <= 0.01])){
    stop("Response cannot have 0 variance.")
  }
  cast_df = select_multi(cast_df, cols = colnames(cast_df)[!colnames(cast_df) %in% names(column_variance[column_variance <= 0.01 | is.na(column_variance)])])
  
  
  # Add Rebag for Existing ####
  if(rebag_vars == TRUE){
    cast_df$Rebag = rowSums(cast_df[, -c(1,unlist(as.list(1:ncol(cast_df))[colnames(cast_df) == response])),with = FALSE])
  }
  
  if(rebag_mean_vars == TRUE){
    cast_df$Rebag_Mean = rowMeans(cast_df[, -c(1,unlist(as.list(1:ncol(cast_df))[colnames(cast_df) == response])),with = FALSE])
  }
  z_cast_df = eval(parse(text=paste0("zoo(cast_df$`", response, "`, cast_df$d_var)")))
  
  ### STATE SPECIFICATION ####
  eval(parse(text=paste0("ss = AddLocalLevel(list(), cast_df$`", response, "`)")))
  ss <- bsts::AddLocalLinearTrend(ss, y = z_cast_df)
  # Remove seasonality of 1 from ss, as it causes an issue with BLAS. 
  nseasons = nseasons[nseasons != 1]
  if(length(nseasons) > 0){
    for(seasons in 1:length(nseasons)){
      ss <- bsts::AddSeasonal(ss, y = z_cast_df, nseasons = nseasons[seasons])          
    }
  }else if(length(nseasons) == 1L){
    ss <- bsts::AddSeasonal(ss, y = z_cast_df, nseasons = nseasons)
  }
  ss <- bsts::AddAutoAr(ss, y = z_cast_df, lags = 50)

  ### PARAMETERS #####
  bsts.model = eval(parse(text=paste0("bsts(data = cast_df, formula = `", 
                                      response,
                                      "` ~ ",
                                      paste0("`", colnames(cast_df)[2:ncol(cast_df)][colnames(cast_df)[2:ncol(cast_df)] != response],
                                             "`", collapse = " + "),
                                      ",ss", ",niter = ", niter,", model.options = model.options)")))
  ### SELECT INITIAL PREDICTORS #####
  predictors = data.frame(summary(bsts.model)$coefficients)[data.frame(summary(bsts.model)$coefficients)$inc.prob > inclusion_probability,]
  
  return(list(bsts.model = bsts.model, 
              predictors = predictors))
}