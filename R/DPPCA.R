#' Dynamic Probabilistic Principal Components model (DPPCA)
#'
#' DPCCA is a model for analysing longitudinal metabolomic data.
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
#' @export
#'


DPPCA = function(q,chain_output, prior_params, burn_in, thin, data_time){

  #chain size: desired chain size

  chain_size = chain_output*(thin+1) + burn_in

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
  U_chain = rep(list(rep(list(matrix(NA, nrow = q, ncol = n)),M)), chain_size)
  W_chain = rep(list(rep(list(matrix(NA, nrow = n, ncol = q)),M)), chain_size)
  eta_chain = matrix(NA, ncol = M, nrow = chain_size)
  H_chain = rep(list(rep(list(rep(NA, q)),M)), chain_size)
  v2_chain = rep(NA, chain_size)
  nu_chain = rep(NA, chain_size)
  phi_chain = rep(NA, chain_size)
  V_chain = matrix(NA, nrow = chain_size, ncol = q)
  mu_chain = matrix(NA, nrow = chain_size, ncol = q)
  Phi_chain = rep(list(matrix(NA, nrow = 1, ncol = q)), chain_size)

  #Store acception for MH steps
  eta_accept = rep(0, M)
  accept_lambda = matrix(0, nrow=M, ncol = q)
  accept_phi <- 0
  accept_Phi = rep(0, q)

  step = 1
  while(step <= chain_size){

    #Step 1A
    U = gibbs_U(data, eta, W, H)
    U_chain[[step]]<- U

    #Step 1B
    W = gibbs_W(data, eta, omega_inv, U)
    W_chain[[step]]<- W

    #Step 1C
    mh1= MH_eta(eta,U,W,v2,phi,nu, data, eta_accept)
    eta = mh1$eta
    eta_chain[step,]<- eta
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
    H_chain[[step]] <- H

    #Step 2A
    v2 = gibbs_v2(alpha, beta, eta, phi, nu)
    v2_chain[step] <- v2

    #Step 2B
    nu = gibbs_nu(sigma2_nu, phi, eta, v2,data)
    nu_chain[step] <- nu

    #Step 2C
    mh3 = MH_phi(mu_phi, sigma2_phi, eta, nu, v2,phi ,accept_phi)
    phi = mh3$phi
    phi_chain[step] <- phi
    accept_phi = mh3$accepted

    #Step 3A
    V = gibbs_V(alpha_V,beta_V,lambda,Phi, mu)
    V_chain[step,]<- V

    #Step 3B
    mu = gibbs_mu(sigma2_nu, Phi,V,lambda)
    mu_chain[step,] <- mu

    #Step 3C
    mh4 = MH_Phi(mu_Phi, sigma2_Phi,Phi, V,lambda, mu, accept_Phi)
    Phi = mh4$Phi
    Phi_chain[[step]] <- Phi
    accept_Phi = mh4$accepted

    step = step + 1
    print(step)

  }

  ###### Burn in and thinning
  if(burn_in > 0){
    U_chain = tail(U_chain, -burn_in)
    W_chain = tail(W_chain, -burn_in)
    eta_chain = tail(eta_chain, -burn_in)
    H_chain = tail(H_chain, -burn_in)
    v2_chain = tail(v2_chain, -burn_in)
    nu_chain = tail(nu_chain, -burn_in)
    phi_chain = tail(phi_chain, -burn_in)
    V_chain = tail(V_chain, -burn_in)
    mu_chain = tail(mu_chain, -burn_in)
    Phi_chain = tail(Phi_chain, -burn_in)
  }

  if(thin>0){

    U_chain = U_chain[ c( TRUE,rep(FALSE, thin)) ]
    W_chain = W_chain[ c( TRUE,rep(FALSE, thin)) ]
    eta_chain = eta_chain[ c( TRUE,rep(FALSE, thin)) ]
    test = H_chain[ c( TRUE,rep(FALSE, thin)) ]
    H_chain= H_chain[ c( TRUE,rep(FALSE, thin)) ]
    v2_chain= v2_chain[ c( TRUE,rep(FALSE, thin)) ]
    nu_chain= nu_chain[ c( TRUE,rep(FALSE, thin)) ]
    phi_chain= phi_chain[ c( TRUE,rep(FALSE, thin))]
    V_chain= V_chain[ c( TRUE,rep(FALSE, thin)) ]
    mu_chain= mu_chain[ c( TRUE,rep(FALSE, thin)) ]
    Phi_chain= Phi_chain[ c( TRUE,rep(FALSE, thin))]

  }

  persistance_params[["phi_chain"]]<-phi_chain
  persistance_params[["Phi_chain"]]<-Phi_chain
  class(persistance_params) <- "DPPCA_persistance"

  output = list(U_chain = U_chain,
                W_chain = W_chain,
                eta_chain = eta_chain,
                H_chain = H_chain,
                v2_chain = v2_chain,
                nu_chain = nu_chain,
                V_chain = V_chain,
                mu_chain = mu_chain,
                persistance = persistance_params)
  return(output)
}
