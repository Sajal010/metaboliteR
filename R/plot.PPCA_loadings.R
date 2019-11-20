#' Title
#'
#' @param x a
#' @param x_axis_PC a
#' @param y_axis_PC a
#' @param analysis a
#' @param PC a
#' @param n a
#' @param conf_level a
#' @param starting_n a
#' @param ... a
#'
#'
#' @return a
#' @export plot.PPCA_loadings
#' @export
#'
#' @importFrom gtools ask
#'
plot.PPCA_loadings <- function(x, x_axis_PC, y_axis_PC, analysis = FALSE, PC=1, n=5, conf_level=0.95, starting_n = 1, ...){
  PC_number <- as.numeric(sub('PC', '', colnames(x$loadings)))
  if(analysis == FALSE) {
    if(missing(x_axis_PC) == FALSE & missing(y_axis_PC)==FALSE){ # if PC choice both present
      if(x_axis_PC %in% PC_number == FALSE | y_axis_PC %in% PC_number == FALSE){ # if PC's don't exists
        return(print("PC's for x-axis or y-axis is not in the loadings"))
      }
      else {
        plot(x$loadings[,x_axis_PC], x$loadings[,y_axis_PC], pch='', main='PPCA Loadings',
             xlab=colnames(x$loadings)[x_axis_PC], ylab=colnames(x$loadings)[y_axis_PC])
        text(x$loadings[,y_axis_PC]~x$loadings[,x_axis_PC], labels= row.names(x$loadings), cex=0.6, font=1)
        abline(v=0,h=0, col='red')
      }
    }
    else { # Plot all loadings PC
      count = 1
      for(x_PC in PC_number) {
        for(y_PC in x_PC:length(PC_number)){
          if(y_PC != x_PC) {
            plot(x$loadings[,x_axis_PC], x$loadings[,y_axis_PC], pch='', main='PPCA Loadings',
                 xlab=colnames(x$loadings)[x_PC], ylab=colnames(x$loadings)[y_PC])
            text(x$loadings[,y_PC]~x$loadings[,x_PC], labels= row.names(x$loadings), cex=0.6, font=1)
            abline(v=0,h=0, col='red')

            if (count < factorial(length(PC_number)-1)) {
              ask(msg = "Press <RETURN> to view the scores plot for the next pair of dimensions: ")
              count = count + 1
            }
          }
        }
      }
    }
    invisible(x$loadings)
  }
  else { # analysis == TRUE, Loadings analysis plot
    number <- nrow(x$loadings)
    q <- dim(x$loadings)[2]
    ### Add conditions to check for arguments

    if(sum(names(x) == 'loadings_sd') != 1 | is.null(x$loadings_sd)==TRUE){
      return(print('Bootstrap samples needed to check significant loadings.'))
    }

    # Confidence interval based on normal distribution x̄ ± z* σ / (√n) n=1 here because each Jackknife is 1?
    lower_CI <- x$loadings - (qnorm(conf_level+(1-conf_level)/2)*x$loadings_sd)/sqrt(number)
    upper_CI <- x$loadings + (qnorm(conf_level+(1-conf_level)/2)*x$loadings_sd)/sqrt(number)

    #For each component this function creates the table with the name of significant variable,
    #loading estimate and loading confidence interval.
    #index: number of the component
    get_table = function(est, lower, upper, index){
      significant = sign(lower[,index]) == sign(upper[,index])
      sig_names = matrix(rownames(est)[significant == TRUE], ncol =1)
      result = cbind(sig_names, est[significant == TRUE, index], lower[significant == TRUE,index], upper[significant==TRUE,index])
      colnames(result) = c("variable", "loading", "lower_bound", "upper_bound")
      return(result)
    }

    all = lapply(X = seq(1,q,1), FUN = get_table, est = x$loadings, lower = lower_CI, upper = upper_CI)

    significant_x = lapply(all, "[", i =, j = 1)
    significant_x = lapply(significant_x, as.vector)

    names(significant_x) <- c(paste0("PC", 1:q)) # Rename col names

    # Farthest loadings from 0
    extract_signi_row <- rownames(x$loadings) %in% significant_x[[paste0('PC',PC)]] # Extract significant row index
    signi_loadings <- x$loadings[extract_signi_row, PC] # Extract significant data

    top_spectral_bins <- names(sort(abs(signi_loadings), decreasing = TRUE)[starting_n:(starting_n+n-1)]) # Extract top bins

    top_data <- signi_loadings[names(signi_loadings) %in% top_spectral_bins] # Extract top bins values
    top_index <- order(abs(top_data), decreasing = TRUE) # Index of farthest to least
    top_data <- top_data[top_index] # Reorder data farthest to least

    top_lower_CI <- lower_CI[rownames(lower_CI) %in% top_spectral_bins, PC] # Extract top bins values
    top_lower_CI <- top_lower_CI[top_index] # Reorder data

    top_upper_CI <- upper_CI[rownames(upper_CI) %in% top_spectral_bins, PC] # Extract top bins values
    top_upper_CI <- top_upper_CI[top_index] # Reorder data

    lower_CI_ylim <- x$loadings - (qnorm(0.9999+(1-0.9999)/2)*x$loadings_sd)/sqrt(number)
    top_lower_CI_ylim <- lower_CI_ylim[rownames(lower_CI_ylim) %in% top_spectral_bins, PC] # Extract top bins values
    upper_CI_ylim <- x$loadings + (qnorm(0.9999+(1-0.9999)/2)*x$loadings_sd)/sqrt(number)
    top_upper_CI_ylim <- upper_CI_ylim[rownames(upper_CI_ylim) %in% top_spectral_bins, PC] # Extract top bins values

    plot_lower_ylim <- min(c(min(top_lower_CI_ylim) + .2*min(top_lower_CI_ylim), 0)) # min between lowest CI or 0
    plot_upper_ylim <- max(c(max(top_upper_CI_ylim) + .2*max(top_upper_CI_ylim), 0)) # max between highest CI or 0

    barplot(top_data, col='red', border=FALSE, ylim=c(plot_lower_ylim, plot_upper_ylim),
            main='Spectral Regions with Loadings \nSignificantly Different from 0',
            xlab='Spectral Regions', ylab=paste0('PC',PC,' Loadings'))
    grid(nx=0, ny=NULL)
    barplot(top_data, col='red', xaxt='n', yaxt='n', border=FALSE, add=TRUE) # redraw bars to cover grid
    abline(h=0)
    arrows_x <- seq(1-.275,n*1.2-.275,1.2)
    arrows(x0=arrows_x,y0=top_lower_CI,y1=top_upper_CI,angle=90,code=3,length=0.8/n) # CI Error bars

    outputs <- cbind(top_data,top_lower_CI,top_upper_CI)
    colnames(outputs) <- c(paste0("PC", PC, "_Loads_Est."),
                           paste0(conf_level*100, "%", "_Lower_CI"),
                           paste0(conf_level*100, "%", "_Upper_CI")
    )
    return(outputs)
  }
}
