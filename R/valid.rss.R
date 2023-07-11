# Function to compute RSS for validation exercise and assign ranks to
 # different prior combinations for each year

valid.rss <- function(pred,valid.store,runs.list,blocks,years.omit,sigma) {
 # Calculate residual for each year that was omitted
  resid.valid <- mapply(function(pred,index){
    pos <- as.character(blocks[index,])
    out <- as.matrix((pred[,pos]-valid.store[,pos])^2)
    return(out)
  },pred=pred,index=runs.list[,c("dataobj")],SIMPLIFY=FALSE)


  rss.year <- sapply(resid.valid,function(x){
                      apply(x,2,sum)})
   # colnames(rss.year) <- 1:ncol(rss.year)
   # rownames(rss.year) <- paste("y.",1:ncol(blocks),sep="")

  if(ncol(blocks)==1){rss.res <- rss.year}
  if(ncol(blocks)>1){rss.res <- apply(rss.year,2,sum)}

  rss.valid <- matrix(rss.res,nrow=nrow(sigma),ncol=nrow(blocks),
                      byrow=FALSE)
  if(ncol(blocks)==1){colnames(rss.valid) <- years.omit}
  if(ncol(blocks)>1){colnames(rss.valid) <-
                       paste(blocks[,1],"-",blocks[,ncol(blocks)],sep="")}
  
  
  # Calculate ranks
  rank.valid <- apply(rss.valid,2,rank)

  # Bind ranks to sigma matrix
  sigma <- cbind(sigma,rank.valid)

  rss.valid <- cbind(sigma[,c(1:3)],rss.valid)
  
  return(list(rss.valid=rss.valid,sigma=sigma))
}
