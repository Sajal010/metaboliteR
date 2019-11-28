#####################
### EM algorithm  ###
#####################
PPCAEM = function(X, NComp, tol=.00001, maxits=100, showits=T){
  # Arguments X: numeric data, NComp: number of components
  # tol = tolerance level, maxits: maximum iterations, showits: show iterations
  require(pracma) # for orthonormal basis of W; pcaMethods package has also
  require(psych)  # for tr

  # starting points and other initializations
  N = nrow(X)
  P = ncol(X)
  Q = NComp

  S=cov(X)


  u = t(replicate(Q, rnorm(N)))                                    # latent variables

  sigma2 = 1                                                         # variance
  W = prcomp(X,scale. = T)$rotation[,1:Q]                            # loadings;

  it = 0
  converged = FALSE
  ll = 0
  llstore = NULL

  while ((!converged) & (it < maxits)) {
    # create 'old' values for comparison
    if(exists('W.new')){
      W.old = W.new
    }
    else {
      W.old = W
    }

    ll.old = ll

    Psi = sigma2*diag(Q)
    M = t(W.old) %*% W.old + Psi

    W.new = S%*%W.old%*%solve(Psi + solve(M)%*%t(W.old)%*%S%*%W.old)   # E and M
    sigma2 = 1/P * tr(S - S%*%W.old%*%solve(M)%*%t(W.new))

    u = solve(M)%*%t(W.new)%*%t(X)
    uu = sigma2*solve(M) + u%*%t(u)


    Psi1 = sigma2*diag(P)
    C<-W.new %*% t(W.new) + Psi1


    # Likelihood
    ll=-P/2*log(2*3.14)-1/2*det(C)-1/2*tr(solve(C)*S)
    ll = -sum(ll)
    llstore = c(llstore, ll)

    it = it + 1

    converged = max(abs(ll.old-ll)) <= tol

  }

  #Score Calculation
  u =solve(M)%*%t(W.new)%*%t(X)
  u<-t(u)
  unc<-sigma2*solve(M)

  return(list(scores=u, loadings=W.new, llstore = llstore, ll=ll, sigma=sigma2,uncertainity=unc))
}


#####################
### Main Function ###
#####################

####################################################################################
# function for simulating data using pilot data
####################################################################################
sim.pilot.data<-function(n1,n2,p,Zerop,Ip,Zeroq,Iq,sig,W,mu)
{
  n<-n1+n2
  u<-rmvnorm(n,Zeroq,Iq)
  x<-rmvnorm(n,Zerop,sig*Ip)+tcrossprod(u,W)

  return(x)
}

####################################################################################
#              function for simulating data without pilot data                     #
####################################################################################
sim.pilot<-function(n1,n2,p,Zerop,Ip,q,Zeroq,Iq,alpha.sigma,beta.sigma,ao1,bo)
{
  n<-n1+n2

  sig<-1/rgamma(1,alpha.sigma,beta.sigma)
  u<-rmvnorm(n,Zeroq,Iq)

  v<-1/rgamma(q,ao1,bo)
  W<-rmvnorm(p,Zeroq,v*Iq)
  x<-rmvnorm(n,Zerop,sig*Ip)+tcrossprod(u,W)

  return(x)
}

####################################################################################
# function for sampling from the null distribution
####################################################################################
samp.dist<-function(T,S,TS,x,y,n11,n22,in1n2,nn2,cpcf)
{
  for(t in 1:T)
  {
    yperm<-sample(y, replace=FALSE)
    x1<-x[yperm==1,]
    x2<-x[yperm==2,]
    Sj<-sqrt(in1n2*(n11*diag(var(x1))+n22*diag(var(x2)))/nn2)  # FASTER!!!
    S[,t]<-Sj+sort(Sj)[cpcf]                     ## corrected standard deviation
    TS[,t]<-(colMeans(x1)-colMeans(x2))/S[,t]
  }#t
  return(list(S=S, TS=TS))
}

####################################################################################
# function to see if a number is an integer
####################################################################################
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

####################################################################################
# function for estimating sample size
####################################################################################

#' Title
#'
#' @param pilot
#' @param n1
#' @param n2
#' @param p
#' @param prop
#' @param plot.prop
#' @param target.fdr
#'
#' @return
#' @export
#'
#' @examples
metsize<-function(pilot=pilot, n1=5, n2=5, p=300, prop=0.25,  plot.prop, target.fdr= 0.05)
{
  if(length(pilot)!=0)
  {
    resppca<- PPCAEM(X=scale(pilot), NComp=2, tol=1e-20, maxit=100)
    W<-resppca$loadings
    sig<-resppca$sigma
    p<-ncol(pilot)
  }

  if(plot.prop==FALSE)
  {
    mprop<-c(prop)                        ## proportion of significant metabolites
    mrange<-ceiling(mprop*p)
    mtry <- length(mrange)
    nprop.increment <- 1
    sfactors<-(n1+n2)*nprop.increment
  }else{
    mprop<-c(prop,seq(0.1,0.5,0.1))       ## proportion of significant metabolites
    mrange<-ceiling(mprop*p)
    mtry <- length(mrange)
    nprop.increment <- 1
    sfactors<-(n1+n2)*nprop.increment
  }
  ## Setting up the initial values used in the algorithm
  T<-20                                   ## number of permutations to estimate the sampling distribution.
  Sim<-20  				                        ## Number of pilot data sets simulated.
  n<-n1+n2                                ## sample size for pilot data
  q<-2                                    ## dimension of the latent space
  alpha.sigma<-3                          ## the scale parameter of the prior distribution of the variance.
  beta.sigma<-4                           ## the shape parameter of the prior distribution of the variance.
  n.increment = c(1:8)		                ## Sample size increments considered.
  ntry = length(n.increment)              ## number of different samples sizes to be considered.

  ## Storing statistics for each sample size
  fdr50_sim = matrix(NA, ntry, 1)        ## FDR values for each sample simulated from the PPCA model
  fdr90_sim = matrix(NA, ntry, 1)
  fdr10_sim = matrix(NA, ntry, 1)
  fdr_sim = matrix(NA, ntry, Sim)

  ## Storing statistics for each m values
  fdr50_prop = matrix(NA, mtry-1, length(sfactors))
  fdr90_prop = matrix(NA, mtry-1, length(sfactors))
  fdr10_prop = matrix(NA, mtry-1, length(sfactors))
  Add<-array(NA, c(p,T,ntry))
  TSstore<-array(NA, c(p,T,ntry))

  delta<-qnorm(0.99)   # Based on variance of underlying simulation model
  if(length(pilot)!=0){delta<-qnorm(0.89)}
  cf<-0.05
  Zeroq<-rep(0,q)
  Iq<-diag(q)
  Zerop<-rep(0,p)
  Ip<-diag(p)
  TS<-S<-matrix(NA,p,T)
  cpcf<-ceiling(p*cf)
  for(m in 1:mtry)
  {
    pstat<-1-(mrange[m]/p)
    ind <- matrix(FALSE, p, T)
    pos <- sample(1:p, size=mrange[m])    ## sampling mrange[m] metabolites
    ind[pos,] <- TRUE                     ## matrix indicating truly significant and non significant metabolites
    i<-0
    for(k in 1:ntry)
    {
      n1star<-n1*n.increment[k]           ## sample size for treatment group 1
      n2star<-n2*n.increment[k]           ## sample size for treatment group 2
      nstar<-n1star+n2star
      in1n2<-1/n1star+1/n2star; n11<-n1star-1; n22<-n2star-1; nn2<-n1star+n2star-2
      Add.sd<-sqrt(in1n2)

      y<-c(rep(1,n1star),rep(2,n2star))
      if(m==1)                            ## If influence of different proportion of significant metabolites
        #is not of interest...
      {
        for(s in 1:Sim)                  ## assessing the effect of repeated simulations from the underlying model.
        {
          ## Simulating the pilot data
          if(length(pilot)!=0)
          {
            x<-sim.pilot.data(n1star,n2star,p,Zerop,Ip,Zeroq,Iq,sig,W)

          }else{
            x<-sim.pilot(n1star,n2star,p,Zerop,Ip,q,Zeroq,Iq,alpha.sigma,beta.sigma)
          }
          ## Estimating the sampling distribution of the test statistic using permutations.
          res.sampdist<-samp.dist(T,S,TS,x,y,n11,n22,in1n2,nn2,cpcf)
          TS<-res.sampdist$TS
          S<-res.sampdist$S

          ## Store the test statistics for the current sample size.
          TSstore[,,k]<-TS

          ## calculating the shift in metabolites in grp 2
          vars<-S/Add.sd
          Add[,,k]<-delta/(vars*Add.sd)

          ## estimating the FDR
          tsB<-TS
          tsB[pos,] <- tsB[pos,] + Add[pos,,k]      ## Add an increment factor to the t.statistic values of the truly significant metabolites
          atsB <- abs(tsB)
          crit <- quantile(atsB, pstat)         ## identifying a cut-off point (m-th largest absolute value of the p TsB values)
          errors <- (colSums(atsB > crit & !ind))/(colSums(atsB > crit))        ## number of false positives
          fdr_sim[k,s] <- quantile(errors[!is.na(errors)], 0.5)      ## median FDR of the Tstar permutations
        }#s
        ## assessing the effect of repeated simulations from the underlying model.
        emp<-quantile(fdr_sim[k,], c(0.1,0.5,0.9))
        fdr10_sim[k] <- emp[1]
        fdr50_sim[k] <- emp[2]
        fdr90_sim[k] <- emp[3]
      }else{
        ## assessing the effect of varying the proportion of truly significant metabolites (increasing m values) on four different sample sizes.
        if(any(nprop.increment==n.increment[k]))
        {
          i<-i+1
          ## estimating the FDR
          tsB<-TSstore[,,k]
          tsB[pos,] <- tsB[pos,] + Add[pos,,k]            ## adding a shift component to truly significant metabolites
          atsB <- abs(tsB)
          crit <- quantile(atsB, 1 - (mrange[m]/p))
          errors <- (colSums(atsB > crit & !ind ))/(colSums(atsB > crit)) ## (number of false positives for Tstar permutations)/(number of metabolites declared significant for Tstar permutations)
          emp<-quantile(errors[!is.na(errors)], c(0.1,0.5,0.9))
          fdr10_prop[m-1,i] <- emp[1]
          fdr50_prop[m-1,i] <- emp[2]          ## median FDR for Tstar permutations permutations
          fdr90_prop[m-1,i] = emp[3]
        }#if(any)
      }#if
    }#k
  }#m
  results_sim <- cbind(n.increment*n, fdr50_sim, fdr90_sim, fdr10_sim)
  results_prop <- cbind(mprop[-1], fdr50_prop, fdr10_prop, fdr90_prop)
  colnames(results_sim) <- c("sample size","fdr50_sim", "fdr90_sim", "fdr10_sim")
  colnames(results_prop) <- c("prop", paste("fdr50_prop", sfactors, sep = "_"), paste("fdr10_prop", sfactors, sep = "_"), paste("fdr90_prop", sfactors, sep = "_"))
  results_prop <- list(results_prop, sfactors)
  names(results_prop) <- c("results_prop", "sample_sizes")

  ######################################### Determine the sample size at which the FDR line is equal to 0.05
  opty <- rep(0, 2)
  ind1 <- min(c(1:nrow(results_sim))[results_sim[,2]<0.05])
  ind2 <- max(c(1:ind1)[results_sim[1:ind1,2]>0.05])
  opty <- c(results_sim[ind1,2], results_sim[ind2,2])
  optx <- c(results_sim[ind1,1], results_sim[ind2,1])
  optres<-lm(opty~optx)
  nhat <- round((target.fdr - optres$coef[1])/optres$coef[2])
  if(is.na(nhat)){print("Disclaimer:Error has occurred. Rerun the function."); stop()}
  if(n1==n2)
  {
    if(is.wholenumber(nhat/2)){n1<-n2<-nhat/2}else{nhat<- nhat +1; n1<-n2<-nhat/2}
  }else{
    if(is.wholenumber(n1*nhat/(n1+n2))){n1 <- n1*nhat/(n1+n2); n2 <- nhat-n1
    }else{
      n1.user <- n1
      n2.user <- n2
      n1 <- ceiling(n1.user*nhat/(n1.user+n2.user))
      n2 <- ceiling(n2.user*nhat/(n1.user+n2.user))
      nhat <- n1 + n2
    }
  }
  n_values<-c(nhat, n1, n2)
  names(n_values)<-c("n","n1","n2")
  ############################################## Plotting the sample size estimation results
  if(plot.prop == FALSE)
  {
    par(mfrow=c(1,1))
    ## Plot to illustrate variability due to simulation of the pilot data
    plot(results_sim[, "sample size"], results_sim[, "fdr50_sim"], xlab="Sample size", ylab="FDR", pch=16, col=2, ylim = c(0, 1))
    lines(results_sim[, "sample size"], results_sim[, "fdr50_sim"], col = 2, lwd=2)
    lines(results_sim[, "sample size"], results_sim[, "fdr90_sim"], col = 2, lty=2, lwd=2)
    lines(results_sim[, "sample size"], results_sim[, "fdr10_sim"], col = 2, lty=2, lwd=2)
    abline(h = target.fdr, lty = 3, col=1)
    legend("topleft", bty="n", paste("FDR = ", target.fdr), col=1, lty=3)
    arrows(nhat, 0.8, nhat,-0.03, length = 0.1,lwd=2, col=3)
    title(paste("Sample size estimation"), font=1)
    text(nhat, 0.95, labels=substitute(hat(n)==nhat, list(nhat=nhat)), col=3, cex=1.2)
    text(nhat, 0.85, labels=substitute((list(n[1]==n1,n[2]==n2)), list(n1=n1,n2=n2)), col=3, cex=1.2)
  }else{
    par(mfrow=c(1,1), mar=c(4,3,1.5,0.5), oma=c(0,0,2,0),mgp=c(2,1,0))
    for(k in 1:length(sfactors))
    {
      ## variability due to different number of k values
      plot(results_prop$results_prop[, "prop"], results_prop$results_prop[, paste("fdr50_prop",sfactors[k],sep="_")], xlab="Proportion of significant bins", ylab="FDR", pch=16, col=2, ylim = c(0, 1))
      lines(results_prop$results_prop[, "prop"], results_prop$results_prop[, paste("fdr50_prop",sfactors[k],sep="_")], col = 2, lwd=2)
      lines(results_prop$results_prop[, "prop"], results_prop$results_prop[, paste("fdr90_prop",sfactors[k],sep="_")], col = 2, lty=2, lwd=2)
      lines(results_prop$results_prop[, "prop"], results_prop$results_prop[, paste("fdr10_prop",sfactors[k],sep="_")], col = 2, lty=2, lwd=2)
      abline(h = target.fdr, lty = 3, col=1)
      legend("topleft", bty="n", paste("FDR = ", target.fdr), col=1, lty=3)
      title(paste("Sample size=", sfactors[k]), cex = 0.7)
    }
  }
  return(list(nhat=n_values, results_sim = results_sim, results_prop = results_prop, p=p, prop=prop,  n1=n1, n2=n2, target.fdr = target.fdr))
}#End of metsize function

