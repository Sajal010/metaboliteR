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
#'
#' @return PPCA objects
#' @export
#'
#' @examples
#' PPCA(urine_data, q_max=2)
PPCA <- function(data, covariates_data, q_min = 1, q_max = 10, eps = 0.01, max_it = 1000, B, conf_level=0.95){

  p <- ncol(data) # total number of spectral bins
  n <- nrow(data)

  if(q_min == q_max){ # For same q case
    if(missing(covariates_data)) {
      all_results = PPCA_one_q(data, q = q_min, eps = eps, max_it= max_it)
    }
    else {
      all_results = PPCA_one_q(data, covariates_data = covariates_data, q = q_min, eps = eps, max_it= max_it)
    }
    q = q_min
  } else { # For different q
    if(missing(covariates_data)) {
      all_results = PPCA_multi_q(data, q_min = q_min, q_max = q_max,eps = eps, max_it= max_it)
    }
    else {
      all_results = PPCA_multi_q(data, covariates_data = covariates_data, q_min = q_min, q_max = q_max,eps = eps, max_it= max_it)
    }

    X <- q_min:q_max
    # Obtain BIC values
    bic_values <- lapply(X, function(x){
      bic <- all_results[[paste0('Q',x)]]$bic
    })
    bic_values <- unlist(bic_values)

    # Obtain proportion of variance
    PoV_values <- lapply(X, function(x){
      PoV <- all_results[[paste0('Q',x)]]$PoV
    })
    PoV_values <- unlist(PoV_values)

    q = which(bic_values==max(bic_values)) # Obtain optimal q
  }

  #### Outputs ####
  if(q_min == q_max){ # For same q case
    output <- all_results
  } else {
    output <- all_results[[paste0('Q',q)]]
  }

  if(missing(B)==FALSE){ #if B exists
    if(missing(covariates_data)) {
      loadings_sd <- loadings_std(data, q=q, B=B) # standard deviation of loadings
    }
    else {
      boot = loadings_alpha_std(data, covariates_data, q=q, B=B)
      loadings_sd = boot$sd_loads
      alpha_sd = boot$sd_alpha
    }


    lower_CI <- output$loadings - qnorm(1-(1-conf_level)/2)*loadings_sd/sqrt(n)
    upper_CI <- output$loadings + qnorm(1-(1-conf_level)/2)*loadings_sd/sqrt(n)

    #For each component this function creates the table with the name of significant variable,
    #loading estimate and loading confidence interval.
    #index: number of the component
    get_table = function(data,est,lower,upper, index){
      significant = sign(lower[,index]) != sign(upper[,index])
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
  }

  else {
    loadings_sd <- NULL
    if(!missing(covariates_data)) {
      alpha_sd <- NULL
    }
  }

  output[['loadings_sd']] <- loadings_sd
  output$optimal_Q <- q
  output$diagnostic$max_ll_values <- output$max_ll_results
  output$diagnostic$BIC_values <- bic_values
  output$diagnostic$PoV_values <- PoV_values
  output$max_ll <- output$max_ll_results[length(output$max_ll_results)]
  output$max_ll_results <- NULL
  if(exists('significant_x')==TRUE){
    output[['significant_x']] = significant_x
  }

  class(output$diagnostic) <- 'PPCA_diagnostic'
  class(output) <- 'PPCA'
  return(output)
}
