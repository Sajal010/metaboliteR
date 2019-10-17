#' Title
#'
#' @param x A PPCA_diagnostic class (e.g. PPCA$diagnostic)
#' @param max_ll When \code{TRUE}, prints Likelihood Function Convergence Plot. When \code{FALSE}, print BIC and Proportion of Variance plot
#' @param ... Other plots parameters
#'
#' @return Diagnostic plots
#' @export plot.PPCA_diagnostic
#' @export
#'
#' @import graphics
#'
plot.PPCA_diagnostic <- function(x, max_ll = FALSE, ...) {
  if(max_ll){
    plot.default(x$max_ll_values, main='Convergence Check', xlab='Number of Iteration',
                 ylab='Max Log Likelihood', type='b')
  }
  else {
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for 2nd y axis
    plot.default(x$BIC_values, col=2, cex=1.5, pch=16, type ='b',
                 main="BIC and PoV vs Number of PC's", xlab='Number of PCs', ylab='BIC values')
    par(new = TRUE)
    plot.default(x$PoV_values, col=4, cex=1.5, pch=17, type ='b',
                 axes = FALSE, bty = "n", xlab = "", ylab = "",ylim=c(0,1.2))
    axis(side=4, at = pretty(range(0,1)), col.axis =4)
    mtext("Proportion of Variance (PoV)", side=4, line=2, col=4)
    legend("topleft",legend=c("BIC","PoV"),pch=c(16,17), col=c(2,4), cex=1, bty="", y.intersp=0.7)
    par(mar = c(5, 4, 4, 2) + 0.1) # reset to default

  }
}

