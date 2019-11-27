#' Dynamic Probabilistic Principal Components model (DPPCA)
#'
#' DPCCA is a model for analysing longitudinal metabolomic data.
#'
#' @importFrom stats var
#' @importFrom utils tail
#'
#' @param q Number of components.
#' @param chain_output Desired size of ouput chain.
#' @param prior_params Named list containing prior parameter specification. \cr The parameters to be supplied are: \cr
#'  \eqn{\alpha}, \eqn{\beta}: prior of v2, a IG(\eqn{\alpha}/2, \eqn{\beta}/2) distribution. \cr
#'  \eqn{\mu_\nu},  \eqn{\sigma^2_{\nu}}: prior of \eqn{\nu}, a N(\eqn{\mu_\nu},\eqn{\sigma^2_{\nu}}) distribution. \cr
#'  \eqn{\mu_\phi},  \eqn{\sigma^2_\phi}: prior of \eqn{\phi}, a N_{[-1,1]}(\eqn{\mu_\phi},\eqn{\sigma^2_\phi}) distribution. \cr
#'  \eqn{\alpha_V}, \eqn{\beta_V}: prior of V, a IG(\eqn{(\alpha_V)}/2, \eqn{(\beta_V)}/2) distribution. \cr
#'  \eqn{\sigma^2_\mu}: prior of \eqn{\mu}, a N(0,\eqn{\sigma^2_\mu}) distribution. \cr
#'  \eqn{\mu_\Phi},  \eqn{\sigma^2_{\Phi}}: prior of \eqn{\Phi}, a N_{[-1,1]}(\eqn{\mu_\Phi},\eqn{\sigma^2_\Phi}) distribution. \cr
#'  \eqn{\Omega_m}: prior covariance matrix of \eqn{W_m}, a MVN(0, \eqn{\Omega_m}) distribution. \cr
#' @param burn_in length of burn-in period.
#' @param thin thinning to be performed on chain.
#' @param data_time List of M matrixes or data frames containing the observed data for each time point. \cr
#' The data frame should contain observations in rows and spectral bins in columns.
#'
#' @param  post_burn_in further burn-in applied to chains of loadings and scores to calculate posteriors
#' @param post_thin further thinning applied to chains of loadings and scores to calculate posteriors
#'
#' @export
#'


DPPCA = function(q,chain_output, prior_params, burn_in, thin, data_time, post_burn_in = 10, post_thin = 5){
  M = length(data_time)
  n = length(data_time[[1]])
  #chain size: desired chain size

  chain_size = chain_output*(thin+1) + burn_in

  store = c( c(rep(FALSE,burn_in)), rep( c( TRUE,rep(FALSE, thin)),(chain_size-burn_in)/(thin+1) ))
  store_index = which(store)

  alpha = prior_params[["alpha"]];
  beta = prior_params[["beta"]];
  mu_nu = prior_params[["mu_nu"]];
  sigma2_nu = prior_params[["sigma2_nu"]];
  mu_phi = prior_params[["mu_phi"]];
  sigma2_phi = prior_params[["sigma2_phi"]];
  alpha_V = prior_params[["alpha_V"]];
  beta_V = prior_params[["beta_V"]];
  sigma2_mu = prior_params[["sigma2_mu"]];
  sigma2_Phi = prior_params[["sigma2_Phi"]];
  mu_Phi = prior_params[["mu_Phi"]];
  omega_inv = prior_params[["omega_inv"]];

  ## Center the data and (Pareto) scale
  data = sapply(seq(1,M,1),pareto_scale, data = data_time)

  #Initial values of parameters
  initial = sapply(seq(1,M,1), ppca_initial_values, q, data, simplify = F)

  H =  lapply(initial, "[[", "H")

  Sig = lapply(initial, "[[", "Sig")
  Sig = unlist(Sig)

  U =  lapply(initial, "[[", "U")
  names(U)=  paste0(rep("U", M), seq(1,M,1))
  W =  lapply(initial, "[[", "W")
  W_template <- W

  ###### Initialize the SV of errors ####
  eta = log(Sig)
  v2 = 0.1
  nu = 0
  phi = 0.8

  ##### Initialize the SV of U ##########
  V = rep(0.1, q)
  mu = rep(0,q)
  Phi <- rep(0.8,q)

  init_lambda = function(m,U){
    lambda<- log(apply(U[[m]], 1, var))
    out = list(); out[[1]]<- lambda
    return(out)
  }
  lambda = sapply(seq(1,M,1), init_lambda,U)

  #Storing the chains
  U_chain = rep(list(rep(list(matrix(NA, nrow = q, ncol = n)),M)), chain_output)
  W_chain = rep(list(rep(list(matrix(NA, nrow = n, ncol = q)),M)), chain_output)
  eta_chain = matrix(NA, ncol = M, nrow = chain_output)
  H_chain = rep(list(rep(list(rep(NA, q)),M)), chain_output)
  v2_chain = rep(NA, chain_output)
  nu_chain = rep(NA, chain_output)
  phi_chain = rep(NA, chain_output)
  V_chain =  rep(list(matrix(NA, nrow = 1, ncol = q)), chain_output)
  mu_chain = rep(list(matrix(NA, nrow = 1, ncol = q)), chain_output)
  Phi_chain = rep(list(matrix(NA, nrow = 1, ncol = q)), chain_output)

  #Store acception for MH steps
  eta_accept = rep(0, M)
  accept_lambda = matrix(0, nrow=M, ncol = q)
  accept_phi <- 0
  accept_Phi = rep(0, q)

  step = 1
  while(step <= chain_size){

    keep = sum(step == store_index)

    #Step 1A
    U = gibbs_U(data, eta, W, H)

    #Step 1B
    W = gibbs_W(data, eta, omega_inv, U)

    #Step 1C
    mh1= MH_eta(eta,U,W,v2,phi,nu, data, eta_accept)
    eta = mh1$eta
    eta_accept = mh1$accepted

    #Step 1D
    mh2= MH_lambda(lambda,mu,Phi,V,U, accept_lambda, data)
    lambda =mh2$lambda
    #lambda_chain[[step]] <- lambda
    accept_lambda = mh2$accepted

    update_H = function(m,lambda){
      H <- diag(exp(lambda[[m]])); H
    }
    H <- sapply(seq(1,M,1), update_H, lambda, simplify = FALSE)

    #Step 2A
    v2 = gibbs_v2(alpha, beta, eta, phi, nu)

    #Step 2B
    nu = gibbs_nu(sigma2_nu, phi, eta, v2,data)

    #Step 2C
    mh3 = MH_phi(mu_phi, sigma2_phi, eta, nu, v2,phi ,accept_phi)
    phi = mh3$phi
    accept_phi = mh3$accepted

    #Step 3A
    V = gibbs_V(alpha_V,beta_V,lambda,Phi, mu)

    #Step 3B
    mu = gibbs_mu(sigma2_nu, Phi,V,lambda)

    #Step 3C
    mh4 = MH_Phi(mu_Phi, sigma2_Phi,Phi, V,lambda, mu, accept_Phi)
    Phi = mh4$Phi
    accept_Phi = mh4$accepted

    if(keep ==1){
      k = which(step == store_index)
      U_chain[[k]]<- U
      W_chain[[k]]<- W
      eta_chain[k,]<- eta
      H_chain[[k]] <- H
      v2_chain[k] <- v2
      nu_chain[k] <- nu
      phi_chain[k] <- phi
      V_chain[[k]]<- V
      mu_chain[[k]] <- mu
      Phi_chain[[k]] <- Phi
    }

    step = step + 1

    if((step %% 500)==0){
      print(step)
    }

  }

  persistance_params = list();
  persistance_params[["phi_chain"]]<-phi_chain
  persistance_params[["Phi_chain"]]<-Phi_chain
  class(persistance_params) <- "DPPCA_persistance"

  ## Rotate loadings and scores to match MLE loadings ##
  rotation = rotate_W_U(W_chain, U_chain, W_template)
  rotated_W = rotation$W_rotated
  rotated_U = rotation$U_rotated

  #Calculate posterior estimates U and W (median) performing burn-in and thinning again
  U_post = sapply(seq(1,M,1), posterior_U, rotated_U, post_burn_in, post_thin)
  W_post = sapply(seq(1,M,1), posterior_W, rotated_W, post_burn_in, post_thin)

  #Rotate with respect to time 1: (transform the latent scores)
  rotate2 = rotate_post_scores(U_post, W_post)
  U_post_rot = rotate2$U_post_rot

  class(rotated_U)<-"scores_chain"
  class(rotated_W)<- "loadings_chain"

  output = list(U_chain = rotated_U,
                #U_chain = U_chain,
                W_chain = rotated_W,
                posterior_W = W_post,
                posterior_U_rotated = U_post_rot,
                eta_chain = eta_chain,
                H_chain = H_chain,
                v2_chain = v2_chain,
                nu_chain = nu_chain,
                V_chain = V_chain,
                mu_chain = mu_chain,
                persistance = persistance_params,
                W_rotate = W_template)
  return(output)
}
