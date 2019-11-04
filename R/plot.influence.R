#' Plot influence of covariates
#'
#' @param x object of class influence_report
#' @param PC specific principal component
#' @param ... additional arguments
#'
#' @export
#' @export plot.influence_report
#'
#'
#'
plot.influence_report = function(x, PC, ...){
  if(missing(PC)) { # PC not given
    index = 1
    q = length(x)
    while(index <= q){

      plot_cov = x[[index]]

      lower_CI_ylim <- min(plot_cov[,"lower"])
      upper_CI_ylim <- max(plot_cov[,"upper"])

      par(mar=c(5.1,4.1,4.1,4.1))
      colors = c("grey","deepskyblue")
      barplot(plot_cov[, "estimate"], border=FALSE,
              main=paste0('Influence of covariates in PC ',index), ylab=NULL,
              names.arg=names(plot_cov), ylim = c(lower_CI_ylim, upper_CI_ylim),
              col = colors[plot_cov[,"significant"]+1])
      grid(nx=0, ny=NULL)
      barplot(plot_cov[, "estimate"], col = colors[plot_cov[,"significant"]+1],
              xaxt='n', yaxt='n', border=FALSE, add=TRUE) # redraw bars to cover grid
      abline(h=0)
      n_var= nrow(plot_cov); n = nrow(plot_cov)
      arrows_x <- seq(1-.275,n*1.2-.275,1.2)[1:(n_var)]
      arrows(x0=arrows_x,y0=plot_cov[,"lower"],y1=plot_cov[,"upper"],angle=90,code=3,length=0.6/n) # CI Error bars

      legend("topright", inset=c(-0.12, 0), xpd=TRUE, legend = c("No", "Yes"), fill = colors,
             title = "Significant", cex = 0.8, y.intersp = 0.8, border = "black", bty = "n")

      ask(msg = "Press <RETURN> to view the influence plot for the next component: ")
      index = index+1
    }
  } else {
    plot_cov = x[[PC]]

    lower_CI_ylim <- min(plot_cov[,"lower"])
    upper_CI_ylim <- max(plot_cov[,"upper"])

    par(mar=c(5.1,4.1,4.1,4.1))
    colors = c("grey","deepskyblue")
    barplot(plot_cov[, "estimate"], border=FALSE,
            main=paste0('Influence of covariates in PC ',PC), ylab=NULL,
            names.arg=names(plot_cov), ylim = c(lower_CI_ylim, upper_CI_ylim),
            col = colors[plot_cov[,"significant"]+1])
    grid(nx=0, ny=NULL)
    barplot(plot_cov[, "estimate"], col = colors[plot_cov[,"significant"]+1],
            xaxt='n', yaxt='n', border=FALSE, add=TRUE) # redraw bars to cover grid
    abline(h=0)
    n_var= nrow(plot_cov); n = nrow(plot_cov)
    arrows_x <- seq(1-.275,n*1.2-.275,1.2)[1:(n_var)]
    arrows(x0=arrows_x,y0=plot_cov[,"lower"],y1=plot_cov[,"upper"],angle=90,code=3, length=.6/n) # CI Error bars

    legend("topright", inset=c(-0.12, 0), xpd=TRUE, legend = c("No", "Yes"), fill = colors,
           title = "Significant", cex = 0.8, y.intersp = 0.8, border = "black", bty = "n")
  }
  par(mar=c(5.1, 4.1, 4.1, 2.1)) # recover original margin
}
