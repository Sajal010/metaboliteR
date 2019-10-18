# PPCA Method -------------------------------------------------------------

#' @importFrom stats prcomp qnorm sd glm gaussian
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom furrr future_map

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
    n = nrow(data); data = as.matrix(data)
    covariates_data = as.data.frame(covariates_data); n_var = ncol(covariates_data)
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
