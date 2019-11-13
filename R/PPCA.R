#' Title
#'
#' @param data Metabolomic data
#' @param q_min Minimum number of principal component
#' @param q_max Maximum number of principal component
#' @param eps Epsilon criterion (decision threshold) for convergence
#' @param max_it Maximum iteration runs
#' @param B Number of Bootstrap runs
#' @param conf_level Confidence Level
#' @param covariates_data Covariates from the sample attributes (e.g: weights, gender, regions, etc)
#' @param choose_q Choose Optimal Q value as model output
#'
#' @return PPCA objects
#' @export
#'
#' @examples
#' PPCA(urine_data, q_max=2)
PPCA <- function(data, covariates_data, q_min = 1, q_max = 10, eps = 0.01, max_it = 1000, B, conf_level=0.95, choose_q = TRUE){

  p <- ncol(data) # total number of spectral bins
  n <- nrow(data)

  if(q_min == q_max){ # For same q case
    if(missing(covariates_data)) {
      all_results = PPCA_one_q(data, q = q_min, eps = eps, max_it= max_it)
    } else {
      all_results = PPCA_one_q(data, covariates_data = covariates_data, q = q_min, eps = eps, max_it= max_it)
    }
    q = q_min
    bic_values <- all_results$bic
    PoV_values <- all_results$PoV
  } else { # For different q
    if(missing(covariates_data)) {
      all_results = PPCA_multi_q(data, q_min = q_min, q_max = q_max,eps = eps, max_it= max_it)
    } else {
      all_results = PPCA_multi_q(data, covariates_data = covariates_data, q_min = q_min, q_max = q_max,eps = eps, max_it= max_it)
    }
  }

    # Obtain BIC values
    if (q_min != q_max){
    X <- q_min:q_max
    bic_values <- lapply(X, function(x){
      bic <- all_results[[paste0('Q',x)]]$bic
    })
    bic_values <- unlist(bic_values)
    } else {
    bic_values <- all_results$bic
    }

    # Obtain proportion of variance
    if (q_min != q_max){
    PoV_values <- lapply(X, function(x){
      PoV <- all_results[[paste0('Q',x)]]$PoV
    })
    PoV_values <- unlist(PoV_values)
    } else {
      PoV_values <- all_results$PoV
    }

    optimal_q = which(bic_values==max(bic_values)) # Obtain optimal q
    if(choose_q==TRUE) {
      q = optimal_q
    } else {
      q = q_max
    }


  #### Outputs ####
  if(q_min == q_max){ # For same q case
    output <- all_results
  } else {
    output <- all_results[[paste0('Q',q)]]
  }

  if(missing(B)==FALSE) { #if B exists
    if(missing(covariates_data)) {
      loadings_sd <- loadings_std(data, q=q, B=B,initial_guesses = list(sigma2= output$sigma2,
                                                                  loadings = output$loadings)) # standard deviation of loadings
      alpha_sd = NULL
    }
   else if (!missing(covariates_data) ){
      boot = loadings_alpha_std(data, covariates_data, q=q, B=B, initial_guesses = list(alpha = output$alpha,
                                                                                        sigma2= output$sigma2,
                                                                                        loadings = output$loadings))
      loadings_sd = boot$sd_loads
      alpha_sd = boot$sd_alpha
    }
  } else {
      loadings_sd <- NULL
      alpha_sd <- NULL
  }

  if (!missing(B)){
    lower_CI <- output$loadings - qnorm(1-(1-conf_level)/2)*loadings_sd/sqrt(n)
    upper_CI <- output$loadings + qnorm(1-(1-conf_level)/2)*loadings_sd/sqrt(n)

    #For each component this function creates the table with the name of significant variable,
    #loading estimate and loading confidence interval.
    #index: number of the component
    get_table = function(data,est,lower,upper, index){
      significant = sign(lower[,index]) == sign(upper[,index])
      sig_names = matrix(colnames(data)[significant == TRUE], ncol =1)
      result = cbind(sig_names, est[significant == TRUE, index], lower[significant == TRUE,index], upper[significant==TRUE,index])
      colnames(result) = c("variable", "loading", "lower_bound", "upper_bound")
      return(result)
    }

    all = sapply(X = seq(1,q,1), FUN = get_table, data = data, est = output$loadings, lower = lower_CI, upper = upper_CI)

    significant_x = lapply(all, "[", i =, j = 1)
    significant_x = lapply(significant_x, as.vector)

    names(significant_x) <- c(paste0("PC", 1:q)) # Rename col names
    class(significant_x) <- 'PPCA_significant'

    #confidence interval for influence

    if(!missing(covariates_data)){
    lower_CI_alpha <- output$alpha - (qnorm(conf_level+(1-conf_level)/2)*alpha_sd)/sqrt(n)
    upper_CI_alpha <- output$alpha + (qnorm(conf_level+(1-conf_level)/2)*alpha_sd)/sqrt(n)

    #Significance of covariate in each PC
    sig_cov = function(index,alpha, lower, upper){
      alpha = alpha[index,]; lower = lower[index,]; upper = upper[index,]
      result = cbind(alpha, lower, upper)
      colnames(result)= c("estimate", "lower", "upper")
      significant = sign(result[,2]) == sign(result[,3])
      result = cbind(result, significant)
      return(result)
    }

    #q = nrow(output$alpha)
    output_covs = sapply(seq(1,q,1), sig_cov, simplify = FALSE, alpha = output$alpha,
                         lower = lower_CI_alpha, upper = upper_CI_alpha)
    class(output_covs) <- "influence_report"
    }

  }

  if(q_min != q_max) {
    output$optimal_q <- optimal_q
  }

  output[['alpha_sd']] <- alpha_sd
  output[['loadings_sd']] <- loadings_sd
  output$diagnostic$max_ll_values <- output$max_ll_results
  output$diagnostic$BIC_values <- bic_values
  output$diagnostic$PoV_values <- PoV_values
  output$max_ll <- output$max_ll_results[length(output$max_ll_results)]
  output$max_ll_results <- NULL

  if(exists('output_covs')==TRUE){
    output[['influence_report']] = output_covs
  }

  if(exists('significant_x')==TRUE){
    output[['significant_x']] = significant_x
  }

  class(output$diagnostic) <- 'PPCA_diagnostic'
  class(output) <- 'PPCA'
  return(output)
}
