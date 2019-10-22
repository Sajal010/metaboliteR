#' Heatmap of BIC values for different g and q.
#'
#' @import ggplot2
#'
#' @param bic_results pbject of class MPPCA_BIC from MPPCA results
#'
#' @export
#'

plot.MPPCA_BIC = function(bic_results) {


  comb <- bic_results #tranform it to df
  comb = data.frame(matrix(unlist(comb),ncol = 3))
  names(comb) = c("g", "q", "BIC")

  opt_g = comb$g[comb$BIC == max(comb$BIC)]
  opt_q = comb$q[comb$BIC == max(comb$BIC)]

  ggplot(comb,aes(x=g,y=q,fill=BIC))+
    geom_tile(alpha = 0.8) + theme_bw() +
    labs(title = "BIC", x = "Groups", y = "Components") +
    scale_fill_gradient(low="gold1",high="firebrick1")+
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
          plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    geom_point(aes(x = opt_g, y = opt_q), shape = 4, size = 7)

}

