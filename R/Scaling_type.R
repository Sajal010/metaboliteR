st_dev<-function(x){
  return(sqrt(sum((x-mean(x))^2/(length(x)-1))))
}

autoscale<-function(x){
  return((x-mean(x))/st_dev(x))
}

paretoscale<-function(x){
  return((x-mean(x))/sqrt(st_dev(x)))
}

rangescale<-function(x){
  return((x-mean(x))/(max(x)-min(x)))
}

vastscale<-function(x){
  return(((x-mean(x))/st_dev(x))*(mean(x)/st_dev(x)))
}


#' Title
#' @importFrom KODAMA normalization
#' @param x data
#' @param scale_type scaling type to be used by the user
#'
#' @export
#'
scale_data <- function(x, scale_type="none"){

  if(scale_type=="none")
    x

  else if(scale_type=="PQN")
    normalization(x, method = "pqn")

  else if(scale_type=="autoscale")
    autoscale(x)

  else if (scale_type=="paretoscale")
    paretoscale(x)

  else if (scale_type=="rangescale")
    rangescale(x)

  else if (scale_type=="vastscale")
    vastscale(x)

  else print("There are only four types of scaling present in this function.
             Please select one of the four types of scaling : Autoscale, Pr
             aretoscale, RangeScale or Vastscale")
}
