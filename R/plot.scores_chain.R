
#' Trace plot of sampled latent scores
#'
#' Chain of latent scores of a randomly sampled observation on a randomly sampled time point are plotted for convergence assessment.
#'
#' @param x object of class scores_chain
#' @param ... additional arguments
#'
#' @export
#'

plot.scores_chain= function(x,...){
  U_chain <- x
  n = dim(U_chain[[1]][[1]])[2]
  M = length(U_chain[[1]])

  rand.time = sample(seq(1,M,1),1)
  rand.obs = sample(seq(1,n,1),1)

  #selecting the time point
  U = lapply(U_chain, "[[", rand.time)
  #selecting the animal
  U = lapply(U, "[",,rand.obs)
  q = length(U[[1]])

  chain = matrix(unlist(U), ncol = q, byrow = TRUE)

  ymin = min(chain); ymax = max(chain);
  plot(chain[,1], type="l", ylab = "Latent score", xlab = "Thinned iteration number", ylim = c(mean(chain[,1])-1.5,mean(chain[,1])+1.5))
  lines(chain[,2]+ mean(chain[,1]), type = "l", col = "red")
  legend("topright", legend=c("PC 1", "PC 2"),
         col=c("black", "red"), lty=1, cex=0.8, box.lty=0)

}
