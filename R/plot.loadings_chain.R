
#' Trace plot of sampled loadings
#'
#' Chain of loadings for a randomly sampled spectral bin on a randomly sampled time point are plotted for convergence assessment.
#'
#' @param x object of class loadings_chain
#' @param ... additional arguments
#'
#' @export
#'

plot.loadings_chain= function(x,...){

  W_chain <-x

  p = dim(W_chain[[1]][[1]])[1]
  M = length(W_chain[[1]])

  rand.time = sample(seq(1,M,1),1)
  rand.bin = sample(seq(1,p,1),1)

  #selecting the time point
  W = lapply(W_chain, "[[", rand.time)

  #selecting the animal
  W = lapply(W, "[",rand.bin,)
  q = length(W[[1]])

  chain = matrix(unlist(W), ncol = q, byrow = TRUE)

  ymin = min(chain); ymax = max(chain);
  plot(chain[,1]+0.3, type="l", ylab = "Loadings", xlab = "Thinned iteration number", ylim = c(ymin-0.3,ymax+0.3 ))
  lines(chain[,2]-0.3, type = "l", col = "red")
  legend("topright", legend=c("PC 1", "PC 2"),
         col=c("black", "red"), lty=1, cex=0.8, box.lty=0)

}
