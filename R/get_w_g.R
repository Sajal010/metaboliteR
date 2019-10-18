#' Obtain initial value of loading matrix in MPPCA.
#'
#' Function used to obtain initial loadings matrix a group \code{g}.
#' Perfoms PPCA using only the observations in group \code{g}.
#'
#' @param data data to perform MPPCA on
#' @param g integer. Number of the group to get initial values for
#' @param q number of components
#'
#' @return
#' @seealso [PPCA()]
#'
#' @examples
get_w_g = function(data, g, q){
  p = ncol(data)-1
  data_g = subset(data, data$groups_init == g); data_g$groups_init = NULL
  ppca = PPCA_one_q(data = data_g, q = q)
  w_g = ppca$loadings ; w_g = as.matrix(w_g, ncol = q)
  sig = ppca$sigma2
  out = list(); out[[1]] = w_g; out[[2]] = sig;
  return(out)
}
