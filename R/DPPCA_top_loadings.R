#' Top n influential loading by time point
#'
#' @param n  number of top n loadings to be returned
#' @param PC number of the principal component to consider
#' @param W  chain of rotated loadings
#' @param cred_level cred_level percentage quantile-based credible interval
#'
#' @export


DPPCA_top_loadings = function(n, PC, W, cred_level = 0.95){
  M = length(W[[1]])
  all_m = sapply(seq(1,M,1),sig_loadings,PC = PC,W, cred_level, simplify = FALSE)
  #order the data frames and store top n
  top_loads = list();
  for(i in 1:M){
    df_time = all_m[[i]]
    df_sort <- df_time[order(-abs(df_time$estimate)),]
    top_n = df_sort[1:n,]
    top_n[,1] = gsub("X", "", as.character(top_n[,1]))
    top_loads[[i]] <- top_n
  }
  class(top_loads) <- "top_loadings"
  return(top_loads)
}
