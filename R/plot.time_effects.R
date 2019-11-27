#' Plot top metabolites time effect
#'
#' @param x object of class time_effects
#' @param ... additional arguments
#'
#' @export
#' @import ggplot2
#' @importFrom tidyr gather

plot.time_effects = function(x,...){

  sig_metabolites <- x

  t = seq(1, 8, 0.01)
  sigs = length(sig_metabolites)

  get_intensity = function(i, sig_metabolites){
    t = seq(1, 8, 0.01)
    betas = sig_metabolites[[i]]
    if(length(betas)==4){
      fit = betas$intercept + betas$linear_effect*t + betas$quadratic_effect*t^2 + betas$cube_effect*t^3
    }else if(length(betas)==3){
      fit =  betas$intercept +betas$linear_effect*t + betas$quadratic_effect*t^2
    }else if(length(betas)==2){
      fit =betas$intercept +betas$linear_effect*t
    }
    fit = matrix(fit, ncol =1)
    return(fit)
  }

  effects = sapply(seq(1, sigs,1), get_intensity, sig_metabolites)
  effects = cbind(effects, t)
  colnames(effects) = c(names(sig_metabolites), "time")

  effects = data.frame(effects)

  effects_long = tidyr::gather(effects, "bin", "intensity", 1:sigs)
  effects_long$bin = gsub("X","", effects_long$bin)

  ggplot2::ggplot(effects_long, aes(x = effects_long$time, y = effects_long$intensity, colour = effects_long$bin)) + geom_line() +
    theme_bw() + labs(colour = "spectral bin", y = 'Average intensity', x = "time") +
    scale_x_continuous(limits = c(1, 8), expand = c(0, 0))

}
