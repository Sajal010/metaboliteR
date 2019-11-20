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
      num <- length(plot_cov[, "estimate"])

      lower_CI_ylim <- min(plot_cov[,"lower"]) + .2*min(plot_cov[,"lower"])
      upper_CI_ylim <- max(plot_cov[,"upper"]) + .2*max(plot_cov[,"upper"])

      # par(mar=c(5.1,4.1,4.1,4.1))
      colors = c("grey","deepskyblue")
      barplot(plot_cov[, "estimate"], border=FALSE,
              main=paste0('Influence of Covariates in PC ',index),
              xlab = "Covariates", ylab = "Coefficient Estimates",
              names.arg=names(plot_cov),
              xlim = c(0.2, (num*1.2)+.4), # 0.2 spacing between each bar
              ylim = c(lower_CI_ylim, upper_CI_ylim),
              col = colors[plot_cov[,"significant"]+1])
      grid(nx=0, ny=NULL)
      barplot(plot_cov[, "estimate"], col = colors[plot_cov[,"significant"]+1],
              xaxt='n', yaxt='n', border=FALSE, add=TRUE) # redraw bars to cover grid
      abline(h=0)
      n_var= nrow(plot_cov); n = nrow(plot_cov)
      arrows_x <- seq(1-.275,n*1.2-.275,1.2)[1:(n_var)]
      arrows(x0=arrows_x,y0=plot_cov[,"lower"],y1=plot_cov[,"upper"],angle=90,code=3,length=0.6/n) # CI Error bars

      legend("topright", # inset=c(-0.12, 0),
             xpd=TRUE, legend = c("No", "Yes"), fill = colors,
             title = "Significant", cex = 0.8, y.intersp = 0.8, border = "black", bty = "n")

      if (index < q) {
        ask(msg = "Press <RETURN> to view the scores plot for the next pair of dimensions: ")
      }
      index = index+1
    }
  } else {
    plot_cov = x[[PC]]
    num <- length(plot_cov[, "estimate"])

    lower_CI_ylim <- min(plot_cov[,"lower"]) + .2*min(plot_cov[,"lower"])
    upper_CI_ylim <- max(plot_cov[,"upper"]) + .2*max(plot_cov[,"upper"])

    colors = c("grey","deepskyblue")
    barplot(plot_cov[, "estimate"], border=FALSE,
            main=paste0('Influence of Covariates in PC ',PC),
            xlab = "Covariates", ylab = "Coefficient Estimates",
            names.arg=names(plot_cov),
            xlim = c(0.2, (num*1.2)+.4), # 0.2 spacing between each bar
            ylim = c(lower_CI_ylim, upper_CI_ylim),
            col = colors[plot_cov[,"significant"]+1])
    grid(nx=0, ny=NULL)
    barplot(plot_cov[, "estimate"], col = colors[plot_cov[,"significant"]+1],
            xaxt='n', yaxt='n', border=FALSE, add=TRUE) # redraw bars to cover grid
    abline(h=0)
    n_var= nrow(plot_cov); n = nrow(plot_cov)
    arrows_x <- seq(1-.275,n*1.2-.275,1.2)[1:(n_var)]
    arrows(x0=arrows_x,y0=plot_cov[,"lower"],y1=plot_cov[,"upper"],angle=90,code=3, length=.6/n) # CI Error bars

    legend("topright", xpd=TRUE, legend = c("No", "Yes"), fill = colors,
           title = "Significant", cex = 0.8, y.intersp = 0.8, border = "black", bty = "n")
  }
  # par(mar=c(5.1,4.1,4.1,2.1)) # recover default
}
