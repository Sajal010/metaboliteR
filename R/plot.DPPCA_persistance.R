#' Plotting DPPCA persistance parameters
#'
#' @importFrom stats acf density
#'
#' @param x object of class DPPCA_persistance
#' @param ... additional parameters
#' @type either histogram or trace plot. Default is histogram.
#'
#' @export
#'

plot.DPPCA_persistance = function(x,type = "histogram",...){

  persistance_params <- x
  phi_chain = persistance_params[["phi_chain"]]
  Phi_chain = persistance_params[["Phi_chain"]]
  k = length(Phi_chain[[1]])

  par(mfrow = c(1,k+1))
  if(type == "histogram"){
    hist(phi_chain, freq = F, xlab = expression(phi),  main = " ", breaks = 30)
    lines(density(phi_chain), col="purple", lwd=2)
    Phi = list();
    i = 1
    while(i <= k){
      Phi[[paste0("Phi",i)]] = unlist(lapply(Phi_chain, "[[", i))
      hist(Phi[[i]], freq = F, xlab = paste0("Phi",i),  main = " ", breaks = 30)
      lines(density(Phi[[i]]), col="purple", lwd=2)
      i = i+1
    }
    mtext("Histogram of persistance parameters", side = 3, line = -3, outer = TRUE)

  } else if (type == "chain"){
    plot(phi_chain, type = "l", ylab = expression(paste(phi, " chain")))
    Phi = list();
    i = 1
    while(i <= k){
      Phi[[paste0("Phi",i)]] = unlist(lapply(Phi_chain, "[[", i))
      plot(Phi[[i]], type = "l", ylab =paste(paste0("Phi",i), "chain"))
      i = i+1
    }
    mtext("Trace plot of persistance parameters", side = 3, line = -3, outer = TRUE)

  }
  par(mfrow=c(1,1))
}
