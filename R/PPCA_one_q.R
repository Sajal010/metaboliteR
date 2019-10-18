#' Performs PPCA(PPCCA) in the data using a single value of \code{q}.
#'
#' #' @importFrom stats prcomp qnorm sd glm gaussian
#'
#' @param data  data to perform MPPCA on
#' @param covariates_data covariate data if using PPCCA
#' @param q number of components
#' @param B number of bootstrap replicas to estimate standard devition of loadings
#' @param eps tolerance for EM algorithm convergence
#' @param max_it maximum number of EM algorithm iterations
#' @param est.sd Logical. If TRUE, standard deviation of loadings and covariate effects will be estimated via bootstrap. Default is FALSE
#' @param w.sd Logical. If TRUE, standard deviation of loadings will be estimated via bootstrap. Default is FALSE
#' @param w.sd Logical.
#'
#' @return list with PPCA (PPCCA) outputs
#' @export
#'
#' @examples
PPCA_one_q <- function(data, covariates_data, q, B = 100, eps = 0.01, max_it = 1000, est.sd = FALSE, w.sd = FALSE){

  #B is the number of bootstrap replicas to estimate loadings sd

  # Initialise variables
  n <- nrow(data) # total number of spectral profiles
  p <- ncol(data) # total number of spectral bins

  data.pca <- prcomp(data, center = TRUE) # Perform PCA on data
  W <- data.pca$rotation[,1:q]            # Initial guess for Loading Matrix
  W <- matrix(W, ncol=q)                  # Making sure W is in matrix form
  sigma2 <- 1                             # Initial guess for sigma2

  x_minus_mu <- t(scale(data, scale = FALSE)) #Scaling the data

  if(missing(covariates_data)){
    number_of_free_param <- (p * q) - (0.5 * q * (q - 1)) + 1

  }
  else {
    covariates_data <- as.data.frame(covariates_data)
    covariates = manipulate_covariates(covariates_data)
    L <- ncol(covariates)-1 # total number of covariates after data manipulation
    number_of_free_param <- (p * q) - (0.5 * q * (q - 1)) + (q * (L + 1)) + 1

    M_initial <- t(W)%*%W + sigma2*diag(q)
    delta_initial <- t(W)%*%x_minus_mu  # Formula from package, don't know why
    u.initial <- solve(M_initial)%*%t(W)%*%x_minus_mu + sigma2*solve(M_initial)%*%delta_initial # delta equal expected of u
    alpha_initial <- t(sapply(seq(1,q), getAlpha, u.initial, covariates_data))  # alpha is coefficient based on linear model

  }

  complete_data_ll_results <- c()
  max_ll_results <- c()
  iter <- 0
  conv <- FALSE
  while (conv == FALSE & iter <= max_it) {
    if(!exists('sigma2.new')) {
      sigma2 <- sigma2
    }
    else {
      sigma2 <- sigma2.new
    }
    if(!exists('W.new')) {
      W <- W
    }
    else {
      W <- W.new
    }
    if(!missing(covariates_data)){

      if(!exists('alpha.new')) {
        alpha <- alpha_initial
      } else {
        alpha <- alpha.new
      }
    }

    M <- t(W)%*%W + sigma2*diag(q)

    if(missing(covariates_data)) {
      ### E-step ###
      u.new <- solve(M)%*%t(W)%*%x_minus_mu                       # Expected value of u
      uu.new = sapply(seq(1,n,1), uu_exp, W, sigma2, x_minus_mu);  # Expected value of uu

      ### M-step ###
      W.new <- W_hat(x_minus_mu,u = u.new, uu = uu.new)         # New estimate of W
      sigma2.new = sigma2_hat(x_minus_mu, W, sigma2, u = u.new, uu = uu.new)  # New estimate of sigma2

      max_ll_new <- max_log_likelihood(data, W.new, sigma2.new)

    }
    else {
      ### E-step ###
      u.new <- solve(M)%*%t(W)%*%x_minus_mu + sigma2*solve(M)%*%(alpha%*%t(covariates))  # Expected value of u
      uu.new <- sapply(seq(1,n,1), uu_exp, W, sigma2, x_minus_mu);  # Expected value of uu

      ### M-step ###
      covariates = as.matrix(covariates)

      alpha.new<-(u.new%*%covariates)%*%solve(t(covariates)%*%covariates)      # New estimate of alpha
      W.new <- W_hat(x_minus_mu,u = u.new, uu = uu.new)                        # New estimate of W
      sigma2.new <- sigma2_hat(x_minus_mu, W, sigma2, u = u.new, uu = uu.new)  # New estimate of sigma2
      max_ll_new <- max_log_likelihood(data, W.new, sigma2.new, alpha.new%*%t(covariates))

    }

    # complete_ll = complete_loglik(sigma2, W, u = u.new, uu = uu.new, x_minus_mu)

    max_ll_results <- c(max_ll_results, max_ll_new)

    #Checking convergence criterion
    conv = ifelse((length(max_ll_results) <= 1),FALSE,
                  abs(max_ll_results[length(max_ll_results)] - max_ll_results[length(max_ll_results)-1]) < eps )

    iter <- iter + 1
  }

  bic_results <- bic_value(max_ll_new, number_of_free_param, n)
  aic_results <- aic_value(max_ll_new, number_of_free_param)

  communality <- t(W.new)%*%W.new
  PoV <- communality[q,q]/(tr(communality)+(sigma2.new*p)) # Proportion of variance

  if (w.sd == TRUE){
    loadings_sd = loadings_std(data, q, B=B)
  } else {
    loadings_sd = NULL
  }

  if (est.sd == TRUE){
    boot = loadings_alpha_std(data, covariates_data, q, B)
    loadings_sd = boot$sd_loads
    alpha_sd = boot$sd_alpha
  } else {
    loadings_sd = NULL
    alpha_sd = NULL
  }

  score_var <- sigma2.new * solve((t(W.new) %*% W.new) +
                                    (sigma2.new * diag(ncol(W.new))))
  rownames(score_var) <- c(paste0("PC", 1:q))
  colnames(score_var) <- c(paste0("PC", 1:q))

  score.new <- u.new
  rownames(score.new) <- c(paste0("PC", 1:q)) # Rename row names
  colnames(score.new) <- rownames(data) # Rename col names

  score <- list(score = score.new, score_var = score_var)

  loadings.new <- W.new
  colnames(loadings.new) <- c(paste0("PC", 1:q)) # Rename col names

  class(max_ll_results) <- c('PPCA_max_ll')
  class(loadings.new) <- 'PPCA_loadings'
  class(score) <- 'PPCA_score'

  if(missing(covariates_data)){
    #### Outputs ##
    output <- list(sigma2 = sigma2.new,
                   loadings = loadings.new,
                   loadings_sd = loadings_sd,
                   score = score,
                   max_ll_results = max_ll_results,
                   bic = bic_results,
                   aic = aic_results,
                   PoV = PoV)
  }
  else {
    influence.new <- alpha.new
    rownames(influence.new) <- c(paste0("PC", 1:q)) # Rename row names
    #### Outputs ##
    output <- list(sigma2 = sigma2.new,
                   alpha = alpha.new,
                   alpha_sd = alpha_sd,
                   influence = influence.new, # alpha is influence
                   loadings = loadings.new,
                   loadings_sd = loadings_sd,
                   score = score,
                   max_ll_results = max_ll_results,
                   bic = bic_results,
                   aic = aic_results,
                   PoV = PoV)
  }

  class(output) <- 'PPCA'
  return(output)

}

