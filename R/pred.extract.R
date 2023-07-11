# Function to extract predictions from yourcast output object

pred.extract <- function(run.list,colnames) {
  y.mat <- as.data.frame(run.list$yhat)
  yhat <- y.mat[seq(2,ncol(y.mat),by=2)]
  colnames(yhat) <- colnames
  return(t(yhat))
}
