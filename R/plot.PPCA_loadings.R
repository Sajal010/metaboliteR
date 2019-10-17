#' Title
#'
#' @param x a
#' @param x_axis_PC a
#' @param y_axis_PC a
#' @param ... a
#'
#' @return a
#' @export plot.PPCA_loadings
#' @export
#'
#' @importFrom gtools ask
#'
plot.PPCA_loadings <- function(x, x_axis_PC, y_axis_PC, ...){
  PC_number <- as.numeric(sub('PC', '', colnames(x)))
  if(missing(x_axis_PC) == FALSE & missing(y_axis_PC)==FALSE){ # if PC choice both present
    if(x_axis_PC %in% PC_number == FALSE | y_axis_PC %in% PC_number == FALSE){ # if PC's don't exists
      return(print("PC's for x-axis or y-axis is not in the loadings"))
    }
    else {
      plot(x[,x_axis_PC], x[,y_axis_PC], pch='', main='PPCA Loadings', xlab=colnames(x)[x_axis_PC], ylab=colnames(x)[y_axis_PC])
      text(x[,y_axis_PC]~x[,x_axis_PC], labels= row.names(x), cex=0.6, font=1)
      abline(v=0,h=0, col='red')
    }
  }
  else { # Plot all loadings PC
    count = 1
    for(x_PC in PC_number) {
      for(y_PC in x_PC:length(PC_number)){
        if(y_PC != x_PC) {
          plot(x[,x_axis_PC], x[,y_axis_PC], pch='', main='PPCA Loadings', xlab=colnames(x)[x_PC], ylab=colnames(x)[y_PC])
          text(x[,y_PC]~x[,x_PC], labels= row.names(x), cex=0.6, font=1)
          abline(v=0,h=0, col='red')

          if (count < factorial(length(PC_number)-1)) {
            ask(msg = "Press <RETURN> to view the scores plot for the next pair of dimensions: ")
            count = count + 1
          }
        }
      }
    }
  }
}
