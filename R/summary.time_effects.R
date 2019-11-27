#' Summary of top metabolites time effects
#'
#' @param object object of class time_effects
#' @param ... additional arguments
#'
#' @export
#'

summary.time_effects= function(object,...){
  sig_met = object
  k = length(sig_met)
  #transform to matrix
  to_matrix = function(i,sig_met){
    met = sig_met[[i]]
    result = matrix(unlist(met), nrow = 1)
    result = c(result,rep(NA,4 - length(result)))
    return(result)
  }

  effects = sapply(seq(1,k,1), to_matrix, sig_met)
  effects = t(effects)
  rownames(effects) = names(sig_met)
  colnames(effects) = c("intercept", "linear_effect", "quadratic_effect", "cubic_effect")
  print(effects)

}
