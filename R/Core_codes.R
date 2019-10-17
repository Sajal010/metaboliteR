
# PPCA Method -------------------------------------------------------------

#' @importFrom stats prcomp qnorm sd glm gaussian
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom furrr future_map
PPCA_one_q <- function(data, covariates_data, q, B, eps = 0.01, max_it = 1000, est.sd = FALSE, w.sd = FALSE){
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


PPCA_multi_q = function(data, covariates_data, q_min, q_max, eps = 0.01, max_it = 1000){
  ori_plan <- plan()
  plan(multisession)

  X <- q_min:q_max

  if(missing(covariates_data)) {
    output <- future_lapply(X, function(x) {
      EM <- PPCA_one_q(data, q = x,eps = 0.01, w.sd = FALSE)
    })
  }
  else {
    output <- future_lapply(X, function(x) {
      EM <- PPCA_one_q(data, covariates_data = covariates_data, q = x, eps = 0.01, w.sd = FALSE)
    })
  }

  names(output) <- c(paste0("Q", X))
  on.exit(plan(ori_plan), add = TRUE)
  return(output)
}

# Core Calculation --------------------------------------------------------

uu_exp = function(index, W, sigma2,x_minus_mu){
  n = ncol(x_minus_mu); q = dim(W)[2]
  M <- t(W)%*%W + sigma2*diag(q)
  u <- solve(M)%*%t(W)%*%x_minus_mu
  uu_single <- sigma2*solve(M) + u[,index]%*%t(u[,index])
  out = list(); out[[1]] = uu_single
  return(out)
}

trace_ind = function(index, W, sigma2, uu){
  value <- tr(t(W)%*%(W)%*%uu[[index]])
  return(value)
}


sigma2_hat = function(x_minus_mu, W, sigma2, u, uu){
  n = ncol(x_minus_mu); p = dim(W)[1]
  S <- x_minus_mu%*%t(x_minus_mu)/(n)
  trWtWEuu = sapply(seq(1,n,1),trace_ind, W, sigma2,uu);
  sigma2.new <- 1/(n*p)*( (n*tr(S)) -
                            2*tr(t(x_minus_mu)%*%W%*%u) + sum(trWtWEuu) )
  return(sigma2.new)
}

W_hat = function(x_minus_mu, u, uu){
  d= dim(uu[[1]])[1]
  if (d==1) {
    uu.sum <- sum(simplify2array(uu))
  } else  {
    uu.sum <- apply(simplify2array(uu), c(1,2), sum)
  }
  W.new <- x_minus_mu %*%t(u)%*% solve(uu.sum)
  return(W.new)
}

getAlpha <- function(index, delta, covariates_data){
  return(glm(cbind(t(delta)[,index], covariates_data), family = gaussian)$coeff)
}


# CHANGE THE INITIAL!!
loadings_std = function(data, q, B){
  one_replica = function(data,q){
    n = nrow(data)
    data_boot = data[sample(nrow(data),size=n,replace=TRUE),] #sample the original data set
    EM = PPCA_one_q(data_boot,q=q, eps=0.01)
    out = EM$loadings
    return(out)
  }

  ori_plan <- plan()
  plan(multisession)

  list_loadings = 1:B %>%
    future_map(~ one_replica(data, q))

  sd_loading = apply(simplify2array(list_loadings), c(1,2), sd)
  on.exit(plan(ori_plan), add = TRUE)
  return(sd_loading)
}

loadings_alpha_std = function(data,covariates_data, q, B){
  one_replica = function(data,covariates_data,q){
    n = nrow(data); data = as.matrix(data); n_var = ncol(covariates_data)
    order = sample(nrow(data),size=n,replace=TRUE)
    data_boot = data[order,] #sample the original data set
    cov_boot = covariates_data[order,]

    EM = PPCA_one_q(data_boot,cov_boot,q=q)
    out = list(); out[[1]] = EM$loadings; out[[2]] = EM$alpha
    return(out)
  }
  ori_plan <- plan()
  plan(multisession)

  list_boot = 1:B %>%
    future_map(~ one_replica(data,covariates_data, q))

  loads = lapply(list_boot, "[", 1)
  alphas = lapply(list_boot, "[", 2)

  to_matrix = function(list, q){
    matrix(unlist(list), ncol = q)
  }

  alphas = lapply(alphas, to_matrix, q)
  loads = lapply(loads, to_matrix, q)

  sd_alpha = apply(simplify2array(alphas), c(1,2), sd)
  sd_loads = apply(simplify2array(loads), c(1,2), sd)

  on.exit(plan(ori_plan), add = TRUE)

  output = list(sd_alpha = sd_alpha,
                sd_loads = sd_loads)
  return(output)
}


manipulate_covariates = function(covariates_data){

  covariates <- covariates_data

  if(class(covariates) == "numeric" | class(covariates) == "character") {
    n = length(covariates)
    covariates <- as.data.frame(covariates)
  }
  else if(class(covariates) == "data.frame" | class(covariates) == "matrix") {
    n = nrow(covariates)
    covariates <- as.data.frame(covariates)
  }
  else {
    return(print("Please input proper covariates data"))
  }

  #Creating empty intercept column to ensure next function works as intended
  covariates$intercept <- NA

  #Standarize numerical covariates for stability
  j <- sapply(covariates, is.numeric)
  covariates[j] <- apply(covariates[j],2, standardize_col)

  #Input ones to intercept column
  covariates$intercept = rep(1, n)

  #Check for string columns
  i <- sapply(covariates, is.character)

  #Transforming factors to dummies:
  if (sum(as.numeric(i)) > 0) {
    covariates = fastDummies::dummy_columns(covariates, remove_first_dummy = TRUE)
  }

  #Removing the factors now that they are transformed
  keep = ifelse(i == FALSE, TRUE, FALSE)
  covariates = covariates[,keep]

  #Making the intercept the first column:
  col_order = c("intercept", names(covariates)[names(covariates) != "intercept"])
  covariates = covariates[,col_order]

  return(covariates)

}

# Results Calculation -----------------------------------------------------

complete_loglik = function(sigma2, W, u, uu, x_minus_mu){
  n = ncol(u)
  p = dim(W)[1]
  q = dim(W)[2]

  trWtWEuu = sapply(seq(1,n,1),trace_ind, W=W, sigma2=sigma2,uu=uu);

  c =  (-n*(p+q)/2) * log(2*pi) + (n*p/2) * log(1/sigma2)
  ll = - 0.5 * (tr((1/sigma2)*t(x_minus_mu)%*%x_minus_mu) -
                  (2/sigma2)*tr(t(x_minus_mu)%*%W %*%u) +
                  (1/sigma2)*sum(trWtWEuu)) + n
  #tr(t(u)%*%u)
  return(c+ll)
}

max_log_likelihood <- function(data, W, sigma2, delta){
  n <- nrow(data)
  p <- ncol(data)
  x_minus_mu <- t(scale(data, scale = FALSE))
  variance <- W%*%t(W)+sigma2*diag(p)
  if(is.na(log(det(variance)))){
    log_det_variance = 0
  }
  else {
    log_det_variance = log(det(variance))
  }
  if(missing(delta)){
    max_log_likelihood_values <- -(0.5*n*p)*log(2*pi) -
      0.5*n*log_det_variance -
      0.5*tr(t(x_minus_mu)%*%solve(variance)%*%x_minus_mu)
  }
  else {
    max_log_likelihood_values <- -(0.5*n*p)*log(2*pi) -
      0.5*n*log_det_variance -
      0.5*tr(t(x_minus_mu-(W%*%delta))%*%solve(variance)%*%(x_minus_mu-W%*%delta))
  }

  return(max_log_likelihood_values)
}

bic_value <- function(max_log_likelihood, number_of_free_param, number_of_observation) {
  return(2*max_log_likelihood - number_of_free_param*log(number_of_observation))
}

aic_value <- function(max_log_likelihood, number_of_free_param) {
  return(2*max_log_likelihood - 2*number_of_free_param)
}



# Helper function ---------------------------------------------------------

tr <- function(matrix) {
  return(sum(diag(matrix)))
}

standardize_col <-function(var) {
  rg<-range(var)
  st = (var - min(var))/(rg[2]-rg[1])
  return(st)
}
