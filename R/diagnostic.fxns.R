rss <- function(pred.mat,obs.data,year.min=NA,year.max=NA,year.maxf=NA,
                # Is this a validation exercise?
                validation){

  if(validation) {
    test.pred <- pred.mat[,as.character((year.max+1):year.maxf)]
    rss.pred <- sum((test.pred-log(obs.data))^2)
  }

  else{
  insamp.pred <- pred.mat[,as.character(year.min:year.max)]
  rss.pred <- sum((insamp.pred-log(obs.data))^2)
}
  return(rss.pred)
}


movement.over.time <- function(pred.mat) {
  diffs.time <- apply(X=pred.mat,MARGIN=1,
                     FUN=function(x){sum(abs(diff(x)))})
  diff.time <- mean(diffs.time)
  return(diff.time)
}


movement.over.age <- function(pred.mat) {
  diffs.age <- apply(X=pred.mat,MARGIN=2,
                      FUN=function(x){sum(abs(diff(x)))})
  diff.age <- mean(diffs.age)
  return(diff.age)
}
