summary.robust.yourcast <- function(x,...){

  # Extract aux information
  aux <- x$aux.robust
  #args <- x$aux.robust$args.yourcast

  # Sample frame
  cat("Observed period: ",aux$sample.frame[1],"-",aux$sample.frame[2],"\n",
    sep="")
cat("Forecast period: ",aux$sample.frame[3],"-",aux$sample.frame[4],"\n",
    sep="")
cat("\n")

  # Validation block(s)
  cat("Validation blocks:\n")
  print(aux$blocks)
  cat("\n")

  # Weights
  weights <- aux$weights
  names(weights) <- c("RSS","Age AL","Time AL","Trend Dev")
  cat(paste("Weights:\n"))
  print(weights)
  cat("\n")


  # Grid search range
  rangemat <- apply(x$sigma,2,range)
  cat("Grid search range:\n")
  if(is.na(rangemat[1,1])){cat("Ha.sigma:  NA\n")}
  if(!is.na(rangemat[1,1])){
    cat(paste("Ha.sigma:  ",rangemat[1,1]," - ",rangemat[2,1],"\n",sep=""))}
  if(is.na(rangemat[1,2])){cat("Ht.sigma:  NA\n")}
  if(!is.na(rangemat[1,2])){
    cat(paste("Ht.sigma:  ",rangemat[1,2]," - ",rangemat[2,2],"\n",sep=""))}
  if(is.na(rangemat[1,3])){cat("Hat.sigma: NA\n")}
  if(!is.na(rangemat[1,3])){
    cat(paste("Hat.sigma: ",rangemat[1,3]," - ",rangemat[2,3],"\n",sep=""))}
  cat("\n")

  # Optimal sigma combination
  cat("Optimal sigma combination:\n")
  print(round(x$par.opt,3))

}
