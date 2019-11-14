#' Summary of Significant Spectral bins
#'
#' @param object a
#' @param ... a
#'
#' @return a
#' @export summary.PPCA_significant
#' @export
#'
summary.PPCA_significant <- function(object, ...) {
  p2 <- length(object)
  PC_number <- as.numeric(sub('PC', '', names(object)))

  cat('Total number of Principal Component =', p2, '\n')
  for(i in PC_number){
    cat(paste0('Significant bins in PC', i, ' = ',length(object[[i]]), '\n'))
  }
  occur_n <- as.data.frame(table(table(unlist(object$significant_x))))
  names(occur_n)[1] <- "PC_total"
  cat('\nFrequency of variables that exists in different principals:\n')
  print(occur_n)

  variable_occur <- sort(table(unlist(object$significant_x)), decreasing = TRUE)
}
