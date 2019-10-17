#' Title
#'
#' @param x a
#' @param ori_data a
#' @param PC a
#' @param sample_no a
#' @param xlim a
#' @param ... a
#'
#' @return a
#' @export plot.PPCA_significant
#' @export
#'
plot.PPCA_significant <- function(x, ori_data, PC=1, sample_no = 1, xlim, ...) {
  ori_data_names <- as.numeric(names(ori_data[PC,]))
  if(missing(xlim)==TRUE) {
    data_filtered <- ori_data[sample_no,]
  }
  else if(length(xlim)==2) {
    if(xlim[1] > xlim[2]) {
      data_filtered <- ori_data[sample_no, ori_data_names >=xlim[2] & ori_data_names <=xlim[1]]
    }
    else {
      data_filtered <- ori_data[sample_no, ori_data_names >=xlim[1] & ori_data_names <=xlim[2]]
    }
  }
  else {
    return(print("Please input xlim lower limit and upper limit. eg: c(1,2)"))
  }

  color <- ifelse(names(data_filtered)%in%x[[PC]], "red","grey")

  # Have to seperate names and data as Shiny will give error
  data_names <- names(data_filtered)
  data_only <- as.vector(as.matrix(data_filtered))

  barplot(height=data_only, names.arg = data_names, las = 2,
          col=color, main="Data vs Significant bins",
          xlab="Spectral Bins (ppm)", ylab="Spectral Peak Heights")

}
