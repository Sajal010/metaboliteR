# Export Significant X as a csv file
#' Title
#'
#' @param PPCA_significant a
#' @param filename a
#' @param shiny_path a
#'
#' @return a
#' @export
#' @importFrom utils write.table
#'
PPCA_export_significant <- function(PPCA_significant, filename = 'PPCA_Significant_x', shiny_path = NULL){
  if(class(PPCA_significant)!='PPCA_significant'){
    return(print('Please use output from PPCA (i.e. PPCA$significant) to export csv'))
  }
  else {
    copy_significant <- PPCA_significant
    max_p <- max(unlist(lapply(copy_significant, length)))
    for(i in 1:length(copy_significant)) {
      length(copy_significant[[i]]) <- max_p
    }
    copy_data <- sapply(copy_significant, cbind)
    if(is.null(shiny_path)) {
      write.table(copy_data, file = paste0(filename,'.csv'), sep = ",", row.names = FALSE)
    }
    else {
      write.table(copy_data, file = shiny_path, sep = ",", row.names = FALSE)
    }

  }
}
