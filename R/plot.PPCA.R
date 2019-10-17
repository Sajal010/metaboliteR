
#' Title
#'
#' @param x a
#' @param PC a
#' @param n a
#' @param conf_level a
#' @param starting_n a
#' @param ... a
#'
#' @return a
#' @export plot.PPCA
#' @export
#'
plot.PPCA <- function(x, PC=1, n=5, conf_level=0.95, starting_n = 1, ...) {
  number <- nrow(x$loadings)
  ### Add conditions to check for arguments

  if(sum(names(x) == 'loadings_sd') != 1 | is.null(x$loadings_sd)==TRUE){
    return(print('Bootstrap samples needed to check significant loadings.'))
  }

  # Confidence interval based on normal distribution x̄ ± z* σ / (√n) n=1 here because each Jackknife is 1?
  lower_CI <- x$loadings - (qnorm(conf_level+(1-conf_level)/2)*x$loadings_sd)/sqrt(number)
  upper_CI <- x$loadings + (qnorm(conf_level+(1-conf_level)/2)*x$loadings_sd)/sqrt(number)

  # Farthest loadings from 0
  extract_signi_row <- rownames(x$loadings) %in% x$significant_x[[paste0('PC',PC)]] # Extract significant row index
  signi_loadings <- x$loadings[extract_signi_row, PC] # Extract significant data

  top_spectral_bins <- names(sort(abs(signi_loadings), decreasing = TRUE)[starting_n:(starting_n+n-1)]) # Extract top bins

  top_data <- signi_loadings[names(signi_loadings) %in% top_spectral_bins] # Extract top bins values
  top_index <- order(abs(top_data), decreasing = TRUE) # Index of farthest to least
  top_data <- top_data[top_index] # Reorder data farthest to least

  top_lower_CI <- lower_CI[rownames(lower_CI) %in% top_spectral_bins, PC] # Extract top bins values
  top_lower_CI <- top_lower_CI[top_index] # Reorder data

  top_upper_CI <- upper_CI[rownames(upper_CI) %in% top_spectral_bins, PC] # Extract top bins values
  top_upper_CI <- top_upper_CI[top_index] # Reorder data

  lower_CI_ylim <- x$loadings - (qnorm(0.9999+(1-0.9999)/2)*x$loadings_sd)/sqrt(number)
  top_lower_CI_ylim <- lower_CI_ylim[rownames(lower_CI_ylim) %in% top_spectral_bins, PC] # Extract top bins values
  upper_CI_ylim <- x$loadings + (qnorm(0.9999+(1-0.9999)/2)*x$loadings_sd)/sqrt(number)
  top_upper_CI_ylim <- upper_CI_ylim[rownames(upper_CI_ylim) %in% top_spectral_bins, PC] # Extract top bins values

  barplot(top_data, col='red', border=FALSE, ylim=c(min(c(top_data,top_lower_CI_ylim,0)), max(c(top_data,top_upper_CI_ylim,0))),
          main='Significant Spectral Regions', xlab='Spectral Regions', ylab=paste0('PC',PC,' Loadings'))
  grid(nx=0, ny=NULL)
  barplot(top_data, col='red', xaxt='n', yaxt='n', border=FALSE, add=TRUE) # redraw bars to cover grid
  abline(h=0)
  arrows_x <- seq(1-.275,n*1.2-.275,1.2)
  arrows(x0=arrows_x,y0=top_lower_CI,y1=top_upper_CI,angle=90,code=3,length=0.6/n) # CI Error bars
}
