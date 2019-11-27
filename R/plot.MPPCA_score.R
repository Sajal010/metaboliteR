
# Plot Score Plots
#' Title
#'
#' @param x a
#' @param x_axis_PC a
#' @param y_axis_PC a
#' @param clustering a
#' @param labels a
#' @param conf_level a
#' @param ... a
#'
#' @return a
#' @export plot.MPPCA_score
#' @export
#'
#' @importFrom ellipse ellipse
#' @importFrom gtools ask
#'
plot.MPPCA_score <- function(x, x_axis_PC, y_axis_PC, clustering, labels, conf_level = 0.95, ...) {
  if(length(x)==2){
    PC_number <- as.numeric(sub('PC', '', rownames(x$score[[1]])))
    group_n <- length(x$score)

    if(!missing(clustering) & !missing(labels)){
      clustering_vec <- unlist(clustering, use.names=FALSE)
      cluster <- list()

      if(class(labels) == "data.frame") {
        labels <- unlist(labels, use.names=FALSE)
      }

      for(i in unique(clustering_vec)){
        cluster[[i]] <- labels[clustering_vec == i]
      }
    }

    if(missing(x_axis_PC) == FALSE & missing(y_axis_PC)==FALSE){ # if PC choice both present
      if(x_axis_PC %in% PC_number == FALSE | y_axis_PC %in% PC_number == FALSE){ # if PC's don't exists
        return(print("PC's for x-axis or y-axis is not in the score"))
      }
      else {
        if(missing(labels) | missing(clustering)) { # no labels
          par(mfrow=c(ceiling(group_n/2),2))
          output <- c()
          for(group in 1:group_n) {
            plot(x$score[[group]][x_axis_PC,], x$score[[group]][y_axis_PC,], pch = 16,
                 main=paste('Group', group, 'Score Plot'), xlab = rownames(x$score[[group]])[x_axis_PC], ylab = rownames(x$score[[group]])[y_axis_PC])
            #symbols(x$score[x_axis_PC,], x$score[y_axis_PC,], circles=CI_radius, inches=F, add=T)
            for (l in 1:ncol(x$score[[group]])) { # Plot 95% ellipse
              points(ellipse(x$score_var[[group]][c(x_axis_PC,y_axis_PC),c(x_axis_PC,y_axis_PC)], centre = x$score[[group]][c(x_axis_PC,y_axis_PC),l],
                             level = conf_level), type = "l", col = "grey50")
            }
            legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)

            output <- cbind(output, x$score[[group]][x_axis_PC,], x$score[[group]][y_axis_PC,])
            colnames(output)[2*group] <- paste0("Group", group, "_PC", y_axis_PC) # this has to go first if not error on first initialize
            colnames(output)[2*group-1] <- paste0("Group", group, "_PC", x_axis_PC)

          }
          par(mfrow=c(1,1))

          invisible(output)
        }
        else if(labels[1]==-1){ # for shiny app
          par(mfrow=c(ceiling(group_n/2),2))
          output <- c()
          for(group in 1:group_n) {
            plot(x$score[[group]][x_axis_PC,], x$score[[group]][y_axis_PC,], pch = 16,
                 main=paste('Group', group, 'Score Plot'), xlab = rownames(x$score[[group]])[x_axis_PC], ylab = rownames(x$score[[group]])[y_axis_PC])
            #symbols(x$score[x_axis_PC,], x$score[y_axis_PC,], circles=CI_radius, inches=F, add=T)
            for (l in 1:ncol(x$score[[group]])) { # Plot 95% ellipse
              points(ellipse(x$score_var[[group]][c(x_axis_PC,y_axis_PC),c(x_axis_PC,y_axis_PC)], centre = x$score[[group]][c(x_axis_PC,y_axis_PC),l],
                             level = conf_level), type = "l", col = "grey50")
            }
            legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)

            output <- cbind(output, x$score[[group]][x_axis_PC,], x$score[[group]][y_axis_PC,])
            colnames(output)[2*group] <- paste0("Group", group, "_PC", y_axis_PC) # this has to go first if not error on first initialize
            colnames(output)[2*group-1] <- paste0("Group", group, "_PC", x_axis_PC)
          }
          par(mfrow=c(1,1))

          invisible(output)
        }
        else { # has labels
          par(mfrow=c(ceiling(group_n/2),2))
          output <- c()
          for(group in 1:group_n) {
            plot(x$score[[group]][x_axis_PC,], x$score[[group]][y_axis_PC,], col = c(as.factor(cluster[[group]])), pch = 16,
                 main=paste('Group', group, 'Score Plot'), xlab = rownames(x$score[[group]])[x_axis_PC], ylab = rownames(x$score[[group]])[y_axis_PC])
            #symbols(x$score[x_axis_PC,], x$score[y_axis_PC,], circles=CI_radius, inches=F, add=T)
            for (l in 1:ncol(x$score[[group]])) { # Plot 95% ellipse
              points(ellipse(x$score_var[[group]][c(x_axis_PC,y_axis_PC),c(x_axis_PC,y_axis_PC)], centre = x$score[[group]][c(x_axis_PC,y_axis_PC),l],
                             level = conf_level), type = "l", col = "grey50")
            }
            legend('topright', col=c(unique(as.factor(cluster[[group]]))), legend = levels(as.factor(cluster[[group]])), pch=16)
            legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)

            output <- cbind(output, x$score[[group]][x_axis_PC,], x$score[[group]][y_axis_PC,])
            colnames(output)[2*group] <- paste0("Group", group, "_PC", y_axis_PC) # this has to go first if not error on first initialize
            colnames(output)[2*group-1] <- paste0("Group", group, "_PC", x_axis_PC)
          }
          par(mfrow=c(1,1))

          invisible(output)
        }
      }

    }
    else{ # print all PC score plots
      if(missing(labels) | missing(clustering)) { # no labels
        group_count = 1
        for(group in 1:group_n) {
          count = 1
          for(x_PC in PC_number) {
            for(y_PC in x_PC:length(PC_number)){
              if(y_PC != x_PC) {
                plot(x$score[[group]][x_PC,], x$score[[group]][y_PC,], pch = 16,
                     main=paste('Group', group, 'Score Plot'), xlab = rownames(x$score[[group]])[x_PC], ylab = rownames(x$score[[group]])[y_PC])
                for (l in 1:ncol(x$score[[group]])) { # Plot 95% ellipse
                  points(ellipse(x$score_var[[group]][c(x_PC,y_PC),c(x_PC,y_PC)], centre = x$score[[group]][c(x_PC,y_PC),l],
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
          if (group_count < group_n & length(PC_number)!=1) {
            ask(msg = "Press <RETURN> to view the scores plot for the next group: ")
            group_count = group_count + 1
          }
        }
      }
      else { # has labels
        if(class(labels) != "numeric"){
          labels = unlist(labels)
        }
        group_count = 1
        for(group in 1:group_n) {
          count = 1
          for(x_PC in PC_number) {
            for(y_PC in x_PC:length(PC_number)){
              if(y_PC != x_PC) {
                plot(x$score[[group]][x_PC,], x$score[[group]][y_PC,], col = c(as.factor(cluster[[group]])), pch = 16,
                     main=paste('Group', group, 'Score Plot'), xlab = rownames(x$score[[group]])[x_PC], ylab = rownames(x$score[[group]])[y_PC])
                for (l in 1:ncol(x$score[[group]])) { # Plot 95% ellipse
                  points(ellipse(x$score_var[[group]][c(x_PC,y_PC),c(x_PC,y_PC)], centre = x$score[[group]][c(x_PC,y_PC),l],
                                 level = conf_level), type = "l", col = "grey50")
                }
                legend('topright', col=c(unique(as.factor(cluster[[group]]))), legend = levels(as.factor(cluster[[group]])), pch=16)
                legend("topleft", bty = "n", c("95% Posterior Set"), col = "grey50", lty = c(1), cex=0.7)

                if (count < factorial(length(PC_number)-1)) {
                  ask(msg = "Press <RETURN> to view the scores plot for the next pair of dimensions: ")
                  count = count + 1
                }
              }
            }
          }
          if (group_count < group_n & length(PC_number)!=1) {
            ask(msg = "Press <RETURN> to view the scores plot for the next group: ")
            group_count = group_count + 1
          }
        }
      }
    }
  }
  else
    print("Something Wrong with Score Plot!")
}
