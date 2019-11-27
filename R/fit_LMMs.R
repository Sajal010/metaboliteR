
#' Fit Bayesian Linear Mixed Models to your top metabolites data
#'
#' @param top object of class top_loadings
#' @param alpha (1-alpha) percentage credible interval for parameter significant assessment
#' @param data the original data set in list format
#'
#' @export
#'

fit_LMMs = function(top, alpha, data){

  if(class(top) != "top_loadings"){
    print("Function expects an object of class top_loadings.")
  } else {

    lmm_data = LMMs_data(top, data) #this function is to get the original data

    K = length(lmm_data$bins)
    fit_lmm_k = function(k, lmm_data, alpha){

      data_met = lmm_data$series[[k]]

      fit3 = fit_cubic(data_met, alpha)
      sig_cubic = fit3$significant

      if(sig_cubic==1){
        beta_cube = fit3$beta_cube
        beta_square = fit3$beta_quad
        beta_linear = fit3$beta_lin
        beta0 =fit3$beta0
      } else{

        fit2 = fit_squared(data_met, alpha)
        sig_square = fit2$significant

        if(sig_square==1){
          beta_square = fit2$beta_quad
          beta_linear = fit2$beta_lin
          beta0 =fit2$beta0
        } else {

          fit1 = fit_linear(data_met, alpha)
          sig_linear = fit1$significant

          if(sig_linear==1){
            beta_linear = fit1$beta_lin
            beta0 =fit1$beta0
          }
        }
      }

      output = list();

      if(exists("beta0")){
        output[["intercept"]]=beta0
      }
      if(exists("beta_linear")){
        output[["linear_effect"]]=beta_linear
      }
      if(exists("beta_square")){
        output[["quadratic_effect"]]=beta_square
      }
      if(exists("beta_cube")){
        output[["cube_effect"]]=beta_cube
      }

      return(output)
    }
  }

  all_metabolites = sapply(seq(1,K,1), fit_lmm_k, lmm_data, alpha)
  significant = unlist(lapply(all_metabolites, length))
  sig_metabolites = all_metabolites[significant>0]
  names(sig_metabolites) = lmm_data$bins[significant>0]

  class(sig_metabolites) = "time_effects"
  return(sig_metabolites)

}
