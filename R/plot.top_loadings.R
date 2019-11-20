#' Plot top influential loadings by time point
#'
#' @param x object of class top_loadings
#' @param ... additional arguments
#'
#' @export
#'

plot.top_loadings = function(x,...){

  top <- x
  M = length(top)
  index = 1
  while(index <= M){
    plot_m = top[[index]]
    lower_CI_ylim <- min(plot_m[,"lower"])
    upper_CI_ylim <- max(plot_m[,"upper"])

    n = nrow(plot_m)
    barplot(plot_m[, "estimate"], border=FALSE,
            main= paste("Top", n, "influential bins in dimension", index),
            names.arg=plot_m[,1], ylim = c(lower_CI_ylim, upper_CI_ylim),
            col = "red", ylab = paste("PC", index, "loadings"))
    grid(nx=0, ny=NULL)
    abline(h=0)
    arrows_x <- seq(1-.275,n*1.2-.275,1.2)[1:(n)]
    arrows(x0=arrows_x,y0=plot_m[,"lower"],y1=plot_m[,"upper"],angle=90,code=3,length=0.6/n) # CI Error bars

    ask(msg = "Press <RETURN> to view the influence plot for the next time dimension: ")
    index = index +1
  }
}

