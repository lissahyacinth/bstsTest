#' Rapid Function to select without worrying about data.table/frame or anything else
#' @param df Dataframe
#' @param cols Character vector of columns to select

select_multi <- function(df, cols){
  if("data.table" %in% class(df)){
    return(df[,c(1:ncol(df))[colnames(df) %in% cols], with = FALSE])
  }
  df[,c(1:ncol(df))[colnames(df) %in% cols]]
}