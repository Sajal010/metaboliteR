#'  Obtain initial values of mean vector in MPPCA.
#'
#'  This function is used to get initial values for the mean vector \eqn{\mu} of a particular group in MPPCA.
#'
#' @param data data to perform MPPCA on
#' @param g integer. Number of the group to get initial values for
#'
#' @return List
#'
#' @examples
#'
#'
get_mu_g = function(data, g){
  p = ncol(data) - 1
  data_g = subset(data, data$groups_init == g); data_g$groups_init = NULL
  mu_g = colMeans(data_g); out = list(); out[[1]] = mu_g[1:p];
  return(out)
}
