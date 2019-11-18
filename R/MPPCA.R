#' MPPCA
#'
#' Fits Mixtures of Probabilistic Principal Components model to data.
#'
#' @importFrom mclust mclustBIC summary.mclustBIC summaryMclustBIC
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>%
#' @importFrom future plan multisession
#' @importFrom furrr future_pmap
#'
#' @param data  Data to perform MPPCA on
#' @param q_min Minumum number of components
#' @param q_max Maximum number of components
#' @param g_min Minimum number of clusters
#' @param g_max Maximum number of clusters
#' @param B Number of bootstrap replicas for calculation of loadings sd
#' @param eps Tolerance for convergence of EM algorithm
#'
#' @return List of outputs.
#' @export

MPPCA = function(data, q_min=1, q_max=2, g_min=1, g_max=2, eps = 0.1, B=5){

  if(q_min == q_max && g_min == g_max){

    mppca = MPPCA_one_q_one_g(data, q = q_min, g = g_min, eps = eps)
    result = mppca
  } else {
  q_sequence = seq(q_min, q_max, 1)
  g_sequence = seq(g_min, g_max, 1)

  comb = expand.grid(q_sequence, g_sequence)
  names(comb) = c("q", "g")

  multiple = list(q = comb$q, g = comb$g)
  ori_plan <- plan()
  plan(multisession)
  out = furrr::future_pmap(multiple, MPPCA_one_q_one_g, data = data, eps = 0.1, .progress = TRUE)

  ######### Selecting optimal model #########

  BICs = unlist(lapply(out, "[" , "bic"));

  # Recovering the groups
  get_g = lapply(out, "[[", "pi")
  get_g = lengths(get_g)
  get_g[get_g ==0] <-1
  # Recovering the components
  get_q = lapply(out, "[[", "loadings")
  q_from_list = function(element){
    if( is.null(ncol(element)) == TRUE){
      x = ncol(element[[1]])
    } else {
      x = ncol(element)
    }
    return(x)}
  get_q = unlist(lapply(get_q, q_from_list))
  comb = data.frame(get_g,get_q); names(comb)=c("g", "q"); comb$BIC = BICs

  opt_g = comb$g[comb$BIC == max(comb$BIC)]
  opt_q = comb$q[comb$BIC == max(comb$BIC)]

  #Return the outputs from maximum BIC output
  index_optimal = which(BICs == max(BICs))
  result = out[[index_optimal]]

  initial.guesses = list(w_g = result$loadings,
                         mu_g = result$mu,
                         pi = result$pi,
                         Sig = result$sigma2)

  if(!missing(B)){
    loadings_sd = loadings_std_mppca(data, q = opt_q, g = opt_g, initial.guesses, B)
    names(loadings_sd) <- c(paste0("Group", g_min:opt_g)) # Rename list names
    result$loadings <- list(loadings=result$loadings, loadings_sd=loadings_sd)
    class(result$loadings) <- "MPPCA_loadings"
  } else {
    loadings_sd = NULL
  }

  # result$loadings_sd = loadings_sd
  result$optimal_q = opt_q
  result$optimal_g = opt_g
  result$bic_results = comb

  class(result$bic_results) <- "MPPCA_BIC"

  }

  class(result) <- 'MPPCA'

  return(result)

}
