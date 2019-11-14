#' Plotting DPPCA persistance parameters
#'
#' @importFrom stats acf density
#'
#' @param x object of class DPPCA_persistance
#' @param ... additional parameters
#'
#' @export
#'

plot.DPPCA_persistance = function(x,...){

  persistance_params <- x
  phi_chain = persistance_params[["phi_chain"]]
  Phi_chain = persistance_params[["Phi_chain"]]

  k = length(Phi_chain[[1]])

  par(mfrow = c(1,3))
  plot(phi_chain, type = "l", ylab = expression(paste(phi, " chain")))
  acf(phi_chain, main = "")
  hist(phi_chain, freq = F, xlab = expression(phi),  main = " ", breaks = 30)
  lines(density(phi_chain), col="purple", lwd=2)
  mtext(expression(paste("Persistance of errors - ",phi, " chain")), side = 3, line = -3, outer = TRUE)

  ask(msg = "Press <RETURN> to view the next plot: ")

  Phi = list();
  i = 1
  while(i <= k){

    Phi[[paste0("Phi",i)]] = unlist(lapply(Phi_chain, "[[", i))

    par(mfrow=c(1,3))
    plot(Phi[[i]], type = "l", ylab =paste(paste0("Phi",i), "chain"))
    acf(Phi[[i]], main = "")
    hist(Phi[[i]], freq = F, xlab = paste0("Phi",i),  main = " ", breaks = 30)
    lines(density(Phi[[i]]), col="purple", lwd=2)
    mtext(paste("Persistance of latent scores - ",paste0("Phi",i), "chain"), side = 3, line = -3, outer = TRUE)

    ask(msg = "Press <RETURN> to view the next plot: ")
    i = i+1
  }

}
