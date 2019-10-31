#' Plot influence of covariates
#'
#' @param object of class influence_report
#' @param ...
#'
#' @export
#'
#'
#'
plot.influence = function(x, ...){

index = 1
while(index <= q){

plot_cov = x[[index]]

lower_CI_ylim <- min(plot_cov[,"lower"])
upper_CI_ylim <- max(plot_cov[,"upper"])

par(mar=c(5,5,5,10)+2, xpd = TRUE)
colors = c("grey","deepskyblue")
barplot(plot_cov[, "upper"], border=FALSE,
        main=paste0('Influence of covariates in PC ',index), ylab=NULL,
        names.arg=names(plot_cov), ylim = c(lower_CI_ylim, upper_CI_ylim),
        col = colors[plot_cov[,"significant"]+1])
grid(nx=0, ny=NULL)
abline(h=0)
n_var= nrow(plot_cov); n =33
arrows_x <- seq(1-.275,n*1.2-.275,1.2)[1:(n_var)]
arrows(x0=arrows_x,y0=plot_cov[,"lower"],y1=plot_cov[,"upper"],angle=90,code=3,length=0.6/n) # CI Error bars

legend("topright",inset=c(-0.45,-0.3), legend = c("No", "Yes"), fill = colors,
title = "Significant", cex = 0.8, y.intersp = 0.8, border = "black", bty = "n")

ask(msg = "Press <RETURN> to view the influece plot for the next component: ")
index = index+1
}

}
