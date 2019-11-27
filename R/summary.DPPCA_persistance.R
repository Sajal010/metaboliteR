#' Summary of DPPCA persistance parameters
#'
#' @importFrom stats quantile
#'
#' @param object object of class DPPCA_persistance
#' @param cred_level cred_level percentage quantile-based credible interval
#' @param ... additional parameters for summary
#'
#' @export
#'

summary.DPPCA_persistance = function(object, cred_level = 0.95, ...){

  persistance_params <- object
  alpha = 1 - cred_level

  phi_chain = persistance_params[["phi_chain"]]
  Phi_chain = persistance_params[["Phi_chain"]]

  k = length(Phi_chain[[1]])

  cat(" Persistance of latent scores:", "\n")
  Phi = list();
  for(i in 1:k){
    Phi[[paste0("Phi",i)]] = unlist(lapply(Phi_chain, "[[", i))
    cat(paste0("(PC",i,")"), "Mean:", round(mean(Phi[[i]]),4),"|",paste0(100*(1-alpha),"% credible interval: "),  round(quantile(Phi[[i]], c(alpha/2,1 - alpha/2 )),4), "\n")
  }
  cat(" Persistance of errors:", "\n",
      "Mean :", round(mean(phi_chain),4),"|",paste0(100*(1-alpha),"% credible interval: "), round(quantile(phi_chain, c(alpha/2,1 - alpha/2 )),4), "\n")

}

