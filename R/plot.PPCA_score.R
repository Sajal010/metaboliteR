
# Plot Score Plots
#' Title
#'
#' @param x a
#' @param x_axis_PC a
#' @param y_axis_PC a
#' @param labels a
#' @param conf_level a
#' @param ... a
#'
#' @return a
#' @export plot.PPCA_score
#' @export
#'
#' @importFrom ellipse ellipse
#' @importFrom gtools ask
#'
plot.PPCA_score <- function(x, x_axis_PC, y_axis_PC, labels, conf_level = 0.95, ...) {
  if(dim(x$score)[1]==1){
    if(missing(labels)) {
      stripchart(x$score[1,], main='Score Plot', xlab='PC1', pch=16)
    }
    else {
      print("1D PCA can't run with labels")
    }

  }
  else if(dim(x$score)[1]>1){
    PC_number <- as.numeric(sub('PC', '', rownames(x$score)))
    if(missing(x_axis_PC) == FALSE & missing(y_axis_PC)==FALSE){ # if PC choice both present
      if(x_axis_PC %in% PC_number == FALSE | y_axis_PC %in% PC_number == FALSE){ # if PC's don't exists
        return(print("PC's for x-axis or y-axis is not in the score"))
      }
      else {
        if(missing(labels)) { # no labels
          plot(x$score[x_axis_PC,], x$score[y_axis_PC,], pch = 16,
               main='Score Plot', xlab = rownames(x$score)[x_axis_PC], ylab = rownames(x$score)[y_axis_PC])
          #symbols(x$score[x_axis_PC,], x$score[y_axis_PC,], circles=CI_radius, inches=F, add=T)
          for (l in 1:ncol(x$score)) { # Plot 95% ellipse
            points(ellipse(x$score_var[c(x_axis_PC,y_axis_PC),c(x_axis_PC,y_axis_PC)], centre = x$score[c(x_axis_PC,y_axis_PC),l],
                           level = conf_level), type = "l", col = "grey50")
          }
          legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)
        }
        else if(labels[1]==-1){ # for shiny app
          plot(x$score[x_axis_PC,], x$score[y_axis_PC,], pch = 16,
               main='Score Plot', xlab = rownames(x$score)[x_axis_PC], ylab = rownames(x$score)[y_axis_PC])
          for (l in 1:ncol(x$score)) {
            points(ellipse(x$score_var[c(x_axis_PC,y_axis_PC),c(x_axis_PC,y_axis_PC)], centre = x$score[c(x_axis_PC,y_axis_PC),l],
                           level = conf_level), type = "l", col = "grey50")
          }
          legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)
        }
        else { # has labels
          plot(x$score[x_axis_PC,], x$score[y_axis_PC,], col = c(as.factor(labels)), pch = 16,
               main='Score Plot', xlab = rownames(x$score)[x_axis_PC], ylab = rownames(x$score)[y_axis_PC])
          for (l in 1:ncol(x$score)) { # Plot 95% ellipse
            points(ellipse(x$score_var[c(x_axis_PC,y_axis_PC),c(x_axis_PC,y_axis_PC)], centre = x$score[c(x_axis_PC,y_axis_PC),l],
                           level = conf_level), type = "l", col = "grey50")
          }
          legend('topright', col=c(unique(labels)), legend = levels(as.factor(labels)), pch=16)
          legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)
        }
      }

    }
    else{ # print all PC score plots
      if(missing(labels)) { # no labels
        count = 1
        for(x_PC in PC_number) {
          for(y_PC in x_PC:length(PC_number)){
            if(y_PC != x_PC) {
              plot(x$score[x_PC,], x$score[y_PC,], pch = 16,
                   main='Score Plot', xlab = rownames(x$score)[x_PC], ylab = rownames(x$score)[y_PC])
              for (l in 1:ncol(x$score)) { # Plot 95% ellipse
                points(ellipse(x$score_var[c(x_PC,y_PC),c(x_PC,y_PC)], centre = x$score[c(x_PC,y_PC),l],
                               level = conf_level), type = "l", col = "grey50")
              }
              legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)

              if (count < factorial(length(PC_number)-1)) {
                ask(msg = "Press <RETURN> to view the scores plot for the next pair of dimensions: ")
                count = count + 1
              }
            }
          }
        }
      }
      else { # has labels
        if(class(labels) != "numeric"){
          labels = unlist(labels)
        }
        count = 1
        for(x_PC in PC_number) {
          for(y_PC in x_PC:length(PC_number)){
            if(y_PC != x_PC) {
              plot(x$score[x_PC,], x$score[y_PC,], col = c(as.factor(labels)), pch = 16,
                   main='Score Plot', xlab = rownames(x$score)[x_PC], ylab = rownames(x$score)[y_PC])
              for (l in 1:ncol(x$score)) { # Plot 95% ellipse
                points(ellipse(x$score_var[c(x_PC,y_PC),c(x_PC,y_PC)], centre = x$score[c(x_PC,y_PC),l],
                               level = conf_level), type = "l", col = "grey50")
              }
              legend('topright', col=c(unique(labels)), legend = levels(as.factor(labels)), pch=16)
              legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)

              if (count < factorial(length(PC_number)-1)) {
                ask(msg = "Press <RETURN> to view the scores plot for the next pair of dimensions: ")
                count = count + 1
              }
            }
          }
        }
      }
    }
  }
  else
    print("Something Wrong with Score Plot!")
}
