summary.optim.yourcast <- function(x,...){

  # Extract aux information
  aux <- x$aux.robust
  #args <- x$aux.robust$args.yourcast

  # Sample frame
  cat("Observed period: ",aux$sample.frame[1],"-",aux$sample.frame[2],"\n",
    sep="")
  cat("Forecast period: ",aux$sample.frame[3],"-",aux$sample.frame[4],"\n",
    sep="")
  cat("\n")

  # Validation block
  cat("Validation block:\n")
  print(aux$blocks)
  cat("\n")

  # Weights
  weights <- aux$weights
  names(weights) <- c("RSS","Age AL","Time AL","Trend Dev")
  cat(paste("Weights:\n"))
  print(weights)
  cat("\n")

  # Optimization method
  cat(paste("Optimization method:",aux$method,"\n"))
  cat("\n")

  # Optimal sigma combination
  cat("Optimal sigma combination:\n")
  print(round(x$par.opt,3))

}
