#' @importFrom stats cov rnorm cmdscale dist
#' @importFrom mclust dmvnorm mclustBIC summary.mclustBIC summaryMclustBIC
#' @importFrom stats prcomp qnorm sd glm gaussian
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom furrr future_map

# MPPCA Method -------------------------------------------------------------

MPPCA_one_q_one_g <- function(data, q, g, max_it = 1000,eps = 0.1, initial.guesses = NULL){

  #Checking if g =1: run PPCA
  if (g ==1){
    PPCA_one_q(data, q=q, eps = eps)
  } else if (g>1) {

    #############################
    #     MPPCA algorithm
    #############################
    n <- nrow(data)
    p <- ncol(data)

    data_scaled <- scale(data, scale = FALSE) #Scaling the data
    df <- (p*q) - (0.5 *q*(q - 1)) + 1 + (g-1)

    #####################
    #     Initialize
    #####################
    ll_vector = c()
    iter <- 1
    conv <- FALSE

    #####################
    #  Initial guesses
    #####################

    if(is.null(initial.guesses) == FALSE){

      w_g = initial.guesses$w_g
      mu_g = initial.guesses$mu_g
      pi = initial.guesses$pi
      Sig = initial.guesses$Sig

    }
    if (is.null(initial.guesses)==TRUE) {

      #Estimate groups using packeage mclust
      m<-mclust::mclustBIC(cmdscale(dist(data_scaled)), G=g, verbose = FALSE)
      res<-summary.mclustBIC(m, cmdscale(dist(data_scaled)))
      tau<-res$z                                             #Probability of each obs belonging to each group
      pi<-res$parameters$pro                                 #Initial guess for membership probabilities

      #Make the groups with the estimated tau:
      colnames(tau) = seq(1,g,1)
      data_scaled = as.data.frame(data_scaled)
      data_scaled$groups_init = as.numeric(colnames(tau)[max.col(tau,ties.method="first")])
      data= as.data.frame(data)
      data$groups_init = as.numeric(colnames(tau)[max.col(tau,ties.method="first")])

      #Get column means for each of the groups
      mu_g  = sapply(X= seq(1,g,1), FUN = get_mu_g, data= data_scaled)

      #Perform a PPCA in each initial group and get initial W using PPCA

      if(g ==1){
        ppca =  PPCA_one_q(data = data, q)
        w_g = list(); w_g[[1]] = ppca$loadings; out[[2]] = ppca$sigma2;
      } else if (g > 1){

        ori_plan <- plan()
        plan(multisession)

        x= seq(1,g,1)
        w_g <- try(future_lapply(x, function(x) {
          get_w_g(data, g = x,q)
        }), silent=TRUE)


        #Perform a different option of initial guesses if PPCA fails
        if(class(w_g) == "try-error"){
          w_g2 = sapply(seq(1,g,1), initial_wg, q, data_scaled, tau, simplify = FALSE)
          w_g <- w_g2
        }

      }

      #Sum all second elements of the list
      Sig = sum(unlist(lapply(w_g, "[[", 2)))/g

      #Exclude elements sigma from list
      w_g = lapply(w_g, function(list) { list = list[[1]]})

      data_scaled$groups_init = NULL; data$groups_init =NULL

    }

    #################################
    #           CYCLE 1
    #################################

    while(conv==FALSE  & iter <= max_it){

      #######################
      #     Estep 1
      #######################

      tau = compute_tau(data_scaled, mu_g, pi, Sig, w_g)

      #######################
      #     Mstep 1
      #######################

      #update MU
      mu_g_new =sapply(seq(1,g,1),mu_g_est,data_scaled, tau)

      #update PI (probabilities of membership)
      pi_new =sapply(seq(1,g,1), pi_est,data_scaled,tau)

      #################################
      #           CYCLE 2
      #################################

      #######################
      #     Estep 2
      #######################

      #1. Recompute tau with new mu and pi
      tau = compute_tau(data_scaled,mu_g_new, pi_new, Sig, w_g)

      #2. Compute E(uig)
      e_uig = e_uig(w_g, mu_g_new, Sig, data_scaled)

      #Compute E[uit uitT]
      E_uu = E_uig_uig(w_g, Sig, e_uig,n)

      #######################
      #     Mstep 2
      #######################

      #Calculate new W
      w_g_new = sapply(seq(1,g,1), w_g_hat, data_scaled, Sig, tau, w_g, mu_g_new)

      #Calculate sigma2
      Sig_new = sigma2_hat_mppca(data_scaled,mu_g = mu_g_new, w_g_new, w_g, Sig,pi = pi_new,tau)

      ############################
      #   Assess convergence
      ############################
      ll = observed_log_lik_mppca(data_scaled, mu_g_new, w_g_new, Sig_new, pi_new, tau)
      ll_vector = c(ll_vector, ll)

      #Perform aitken calculation
      conv = ifelse(length(ll_vector) >= 4, Aitken_criterion(ll_vector, iter) <= eps, FALSE)

      mu_g = mu_g_new;
      w_g = w_g_new;
      Sig = Sig_new;
      pi = pi_new;

      iter <- iter + 1
    }

    bic <- bic_value(ll, df, n)  #Check if this is supposed to be the observed loglik

    output <- list(sigma2 =Sig_new,
                   loadings = w_g_new,
                   pi = pi_new,
                   mu = mu_g_new,
                   bic = bic)
    return(output)
  }

}

# Initial guesses  --------------------------------------------------------

get_mu_g = function(data, g){
  p = ncol(data) - 1
  data_g = subset(data, data$groups_init == g); data_g$groups_init = NULL
  mu_g = colMeans(data_g); out = list(); out[[1]] = mu_g[1:p];
  return(out)
}


get_w_g = function(data, g, q){
  p = ncol(data)-1
  data_g = subset(data, data$groups_init == g); data_g$groups_init = NULL
  ppca = PPCA_one_q(data = data_g, q = q)
  w_g = ppca$loadings ; w_g = as.matrix(w_g, ncol = q)
  sig = ppca$sigma2
  out = list(); out[[1]] = w_g; out[[2]] = sig;
  return(out)
}

initial_wg = function(g,q, data_scaled, tau){
  data_g = subset(data_scaled, data_scaled$groups_init == g ); data_g$groups_init = NULL
  p = ncol(data_g);
  S <- cov(data_g);
  decomp<-eigen(S)
  w_g <-decomp$vec[,1:q]
  sig_g<- abs(mean(decomp$values[(q+1):p]))

  if(sig_g<0.0001){sig_g<-abs(rnorm(1,0.01, 0.001))}
  out = list(); out[[1]] = w_g; out[[2]] = sig_g;
  return(out)}
# E-step  --------------------------------------------------------

exp_z_g = function(g,data_scaled,mu_g,pi,Sig, w_g){
  #This function calculates for a specific group g.
  #g: group index

  p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[1]]),  dim(w_g[[g]])[1])
  q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])

  res = dmvnorm(data_scaled,mu_g[[g]], w_g[[g]]%*%t(w_g[[g]])+ Sig*diag(p),log=TRUE) + log(pi[g])
  res = exp(res)
  res = matrix(res, ncol=1); return(res)
}


compute_tau = function(data_scaled, mu_g, pi, Sig, w_g){
  #Apply the previous function to all groups
  p = ifelse( is.null(dim(w_g[[1]]))==TRUE, length(w_g[[1]]),  dim(w_g[[1]])[1])
  q = ifelse( is.null(dim(w_g[[1]]))==TRUE, 1,  dim(w_g[[1]])[2])
  g = length(w_g)

  z = sapply(X = seq(1,g,1), exp_z_g, data_scaled, mu_g, pi,Sig, w_g)
  #Normalize by the sum
  tau<-z/apply(z,1,sum); colnames(tau) = seq(1,g,1)
  return(tau)
}

e_uig = function(w_g, mu_g, Sig, data_scaled){

  e_uig_i = function(index, w_g, mu_g, Sig,data_scaled){
    g = length(w_g)
    p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[g]]),  dim(w_g[[g]])[1])
    q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])
    ind = sapply(seq(1,length(w_g),1),e_uig_i_g, index, w_g,mu_g, Sig, data_scaled)
    #colnames(ind) =  sub(" ", "",paste(rep("group", g), seq(1,g,1)))
    #rownames(ind) = sub(" ", "",paste(rep("u", q), seq(1,q,1)))
    ind = matrix(ind, nrow = q)
    out = list(); out[[1]] <- ind
    return(out)
  }

  g = length(w_g)
  p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[g]]),  dim(w_g[[g]])[1])
  q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])
  n = nrow(data_scaled)
  all = sapply(seq(1,n,1),e_uig_i, w_g,mu_g, Sig, data_scaled)
  return(all)
}

e_uig_i_g = function(g,index,w_g, mu_g, Sig, data_scaled){
  g = as.numeric(length(w_g))
  p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[g]]),  dim(w_g[[g]])[1])
  q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])

  data_scaled = as.matrix(data_scaled)
  x_minus_mu = data_scaled[index,] - mu_g[[g]]
  x_minus_mu = matrix(x_minus_mu, nrow = 1)
  M = t(w_g[[g]])%*%w_g[[g]] + Sig*diag(q)
  e = t((solve(M)%*%t(w_g[[g]]))%*%t(x_minus_mu))
  e = matrix(e, nrow = q)
  return(e)
}


E_uig_uig_i_g = function(index, g, w_g, Sig, e_uig){
  q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])

  M = t(w_g[[g]])%*%w_g[[g]] + Sig*diag(q)               #DOUBT
  res = Sig*solve(M) + e_uig[[index]]%*%t(e_uig[[index]])
  out=list(); out[[1]] <- res
  return(out)
}

E_uig_uig_i = function(index, w_g, Sig, e_uig){
  g = as.numeric(length(w_g))
  ind = sapply(seq(1,g,1),E_uig_uig_i_g, index=index, w_g, Sig, e_uig)
  out = list(); out[[1]] <- ind
  return(out)
}

E_uig_uig = function(w_g, Sig, e_uig,n){
  all = sapply(seq(1,n,1),E_uig_uig_i, w_g, Sig, e_uig)
  return(all)
}

# M-step  --------------------------------------------------------

mu_g_est = function(g,data_scaled,tau){
  mu_new = apply(data_scaled*tau[,g],2,sum)/sum(tau[,g])
  out = list(); out[[1]] = mu_new; return(out)
}

pi_est = function(g,data_scaled, tau){
  pi = sum(tau[,g])/nrow(data_scaled)
}

w_g_hat = function(g, data_scaled, Sig, tau, w_g, mu_g){

  p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[g]]),  dim(w_g[[g]])[1])
  q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])

  data_g<-sweep(data_scaled,2,mu_g[[g]],"-")
  data_g = as.matrix(data_g)

  M<-(t(w_g[[g]])%*%w_g[[g]]) + (Sig)*diag(q)
  S_g = (t(data_g*tau[,g])%*%(data_g*tau[,g]))/sum(tau[,g])

  aux = Sig*diag(q) + solve(M)%*%t(w_g[[g]])%*%S_g%*%w_g[[g]]
  w_new = S_g%*%w_g[[g]]%*%solve(aux)

  out=list(); out[[1]] <- w_new;
  return(out)

}

sigma_aux = function(g, data_scaled, mu_g, w_g_new, w_g, Sig, pi, tau){

  p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[g]]),  dim(w_g[[g]])[1])
  q = ifelse( is.null(dim(w_g[[g]]))==TRUE, 1,  dim(w_g[[g]])[2])

  data_g<-sweep(data_scaled,2,mu_g[[g]],"-")
  data_g = as.matrix(data_g)

  M<-(t(w_g_new[[g]])%*%w_g_new[[g]]) + (Sig)*diag(q)
  S_g = (t(data_g*tau[,g])%*%(data_g*tau[,g]))/sum(tau[,g])

  aux = S_g%*%w_g_new[[g]]%*%solve(M)%*%t(w_g[[g]])

  res = pi[g]*(tr(S_g) - tr(aux))
  return(res)

}

sigma2_hat_mppca= function(data_scaled, mu_g, w_g_new, w_g, Sig, pi, tau){
  p = dim(w_g[[1]])[1]; g = as.numeric(length(w_g))
  terms = sapply(seq(1,g,1),sigma_aux,data_scaled, mu_g, w_g_new, w_g, Sig, pi, tau)
  return(sum(terms)/p)
}

# Results Calculation -----------------------------------------------------

observed_log_lik_mppca = function(data_scaled, mu_g, w_g, Sig, pi, tau){
  g = ncol(tau)

  single_group = function(g,data_scaled, mu_g, w_g, Sig, pi, tau){
    g = as.numeric(length(w_g))
    p = ifelse( is.null(dim(w_g[[g]]))==TRUE, length(w_g[[g]]),  dim(w_g[[g]])[1])
    store_g<-tau[,g]*log(pi[g]) + tau[,g]*dmvnorm(data_scaled, mu_g[[g]], w_g[[g]]%*%t(w_g[[g]])+Sig*diag(p),log = TRUE)
    store_g = matrix(store_g, ncol = 1)
    return(store_g)
  }

  all_g = sapply(seq(1,g,1), single_group, data_scaled, mu_g, w_g, Sig, pi,tau)
  obs_ll = sum(rowSums(all_g))
  return(obs_ll)

}

Aitken_criterion = function(ll_vector, iter){

  v = iter

  c_k = (ll_vector[v] - ll_vector[v-1])/(ll_vector[v-1] - ll_vector[v-2])
  c_k_minus1 =(ll_vector[v-1] - ll_vector[v-2])/(ll_vector[v-2] - ll_vector[v-3])

  lla_k = ll_vector[v-2] + (1 - c_k_minus1)^(-1)*(ll_vector[v-1] - ll_vector[v-2])
  lla_k_plus1 = ll_vector[v-1] + (1 - c_k)^(-1)*(ll_vector[v] - ll_vector[v-1])

  return(abs(lla_k_plus1 - lla_k) )

}

loadings_std_mppca = function(data, q, g, initial.guesses, B){

  one_replica = function(data,q,g, initial.guesses){
    n = nrow(data)
    data_boot = data[sample(nrow(data),size=n,replace=TRUE),] #sample the original data set
    EM = MPPCA_one_q_one_g(data_boot, q, g, eps =0.1, initial.guesses = initial.guesses)
    out = EM$loadings
    return(out)
  }

  ori_plan <- plan()
  plan(multisession)

  list_loadings = 1:B %>%
    future_map(~ one_replica(data, q, g, initial.guesses))

  sd_by_group = function(g,list_loadings){
    loads = lapply(list_loadings, "[[", g)
    sd = apply(simplify2array(loads), c(1,2), sd)
    out = list(sd = sd)
    return(out)
  }

  sd_loadings = sapply(seq(1,g,1), sd_by_group, list_loadings)

  return(sd_loadings)
}
