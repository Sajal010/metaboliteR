#' Function to calculate the trace of a matrix.
#'
#' @param matrix Matrix to calculate trace from.
#'
#' @return Numeric. Trace value.
#'
trace = function(matrix){
  sum(diag(matrix))
}
