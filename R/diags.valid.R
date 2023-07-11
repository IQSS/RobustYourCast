# Function to compute RSS and arc length for validation exercise and assign ranks to
 # different prior combinations for each year

diags.valid <- function(pred,valid.store,runs.list,
                        time.degree,
                        blocks,years.omit,sigma,ages) {

  # RSS  
  # Calculate residual for each year that was omitted
  # Each item in pred list is a matrix of years in columns and ages in rows
  resid.valid <- mapply(function(pred,index){
    pos <- as.character(blocks[index,])
    out <- as.matrix((pred[,pos]-valid.store[,pos])^2)
    return(out)
  },pred=pred,index=runs.list[,c("dataobj")],SIMPLIFY=FALSE)


  rss.year <- sapply(resid.valid,function(x){
                      apply(x,2,sum,na.rm=TRUE)})
   # colnames(rss.year) <- 1:ncol(rss.year)
   # rownames(rss.year) <- paste("y.",1:ncol(blocks),sep="")

  if(ncol(blocks)==1){rss.res <- rss.year}
  if(ncol(blocks)>1){rss.res <- apply(rss.year,2,sum,na.rm=TRUE)}

  rss.valid <- matrix(rss.res,nrow=nrow(sigma),ncol=nrow(blocks),
                      byrow=FALSE)
  if(ncol(blocks)==1){colnames(rss.valid) <- years.omit}
  if(ncol(blocks)>1){colnames(rss.valid) <-
                       paste(blocks[,1],"-",blocks[,ncol(blocks)],sep="")}
  

  # Arc length - age profile

  # Create detrended age profiles--take out mean age profile
  pred.age.detrend <- lapply(pred,function(x){
    mean.age.profile <- apply(x,1,mean)
    resid.age <- x-mean.age.profile
    return(resid.age)
  })
  
  # Calculate lengths of age profiles
  arcs.age.years <- mapply(function(pred,index){
    pos <- as.character(blocks[index,])
    arc.out <- apply(pred[,pos],2,function(resp){
      spline.out <- spline(x=ages, y=resp,
                           xout=seq(min(ages),max(ages),
                             length.out=length(ages)*4))
      arc.length <- sum(abs(diff(spline.out$y)))
      return(arc.length)
    })
    return(arc.out)
  },pred=pred.age.detrend,index=runs.list[,c("dataobj")])

  arc.age <- apply(arcs.age.years,2,sum)

  arc.age.valid <- matrix(arc.age,nrow=nrow(sigma),ncol=nrow(blocks),
                      byrow=FALSE)
  if(ncol(blocks)==1){colnames(arc.age.valid) <- years.omit}
  if(ncol(blocks)>1){colnames(arc.age.valid) <-
                       paste(blocks[,1],"-",blocks[,ncol(blocks)],sep="")}


  # Arc length - time profile

  # Create detrended time profiles--take out desired trend
  time <- as.numeric(colnames(pred[[1]]))

  # Calculate discount factor since all years used
  # Factor is ratio of years used here to years used for RSS and
  # arc.age
  discount <- ncol(blocks)/length(time)

  # terms to include in regression
  # Baseline regression
  formula <- as.formula("row.vec ~ 1")

  # Add terms up to desired degree
  if(time.degree>0) {
    terms <- 1:time.degree
    x.terms <- paste("+ I(time^",terms,")",sep="",collapse="")
    formula <- update.formula(formula,paste("~ .",x.terms,sep=""))
  }

  pred.time.detrend <- lapply(pred,function(x){
    resid.time <- apply(x,1,function(row.vec){
      data.lm <- as.data.frame(cbind(row.vec=row.vec,time=time))
      return(resid(lm(formula,data=data.lm)))})
    return(t(resid.time))
  })
  
  # Calculate lengths of time profiles
  arcs.time.years <- mapply(function(pred,index){
    #pos <- as.character(blocks[index,])
    arc.out <- apply(pred,
                     #pred[,pos],
                     1,function(resp){
      spline.out <- spline(x=colnames(pred), y=resp,
                           xout=seq(time[1],time[ncol(pred)],
                             length.out=ncol(pred)*4))
      #spline.out <- spline(x=blocks[index,], y=resp,
      #                     xout=seq(min(blocks[index,]),max(blocks[index,]),
      #                       length.out=ncol(blocks)*4))
      arc.length <- sum(abs(diff(spline.out$y)))
      return(arc.length)
    })
    return(arc.out)
  },pred=pred.time.detrend,index=runs.list[,c("dataobj")])

  arc.time <- apply(arcs.time.years,2,sum)*discount

  arc.time.valid <- matrix(arc.time,nrow=nrow(sigma),
                           #ncol=nrow(blocks),
                           ncol=1,
                      byrow=FALSE)
  colnames(arc.time.valid) <- "All years"
  #if(ncol(blocks)==1){colnames(arc.time.valid) <- years.omit}
  #if(ncol(blocks)>1){colnames(arc.time.valid) <-
  #                     paste(blocks[,1],"-",blocks[,ncol(blocks)],sep="")}


  # Time trend smoothness

  # Calculate demeaned time profiles
  pred.time.deviate <- lapply(pred,function(x){
    demean.series <- t(apply(x,1,function(row.vec){
      row.vec-mean(row.vec)}))
    # Mean demeaned profile
    mean.profile <- apply(demean.series,2,mean)
    deviate.pred <- apply(demean.series,1,function(row.vec){row.vec-mean.profile})
    return(t(deviate.pred))
  })

  # Calculate arc length of deviations from mean demeaned profile 
  trend.deviate.years <- mapply(function(pred,index){
    
    # Deviateions from mean profile
    deviate.out <- apply(pred,1,function(resp){
      spline.out <- spline(x=colnames(pred), y=resp,
                           xout=seq(time[1],time[ncol(pred)],
                             length.out=ncol(pred)*4))
      deviate.length <- sum(abs(diff(spline.out$y)))
      return(deviate.length)
    })
    return(deviate.out)
  },pred=pred.time.deviate,index=runs.list[,c("dataobj")])

  trend.deviate <- apply(trend.deviate.years,2,sum)*discount

  trend.deviate.valid <- matrix(trend.deviate,nrow=nrow(sigma),
                           #ncol=nrow(blocks),
                           ncol=1,
                      byrow=FALSE)
  colnames(trend.deviate.valid) <- "All years"
  
  
  rss.valid.out <- cbind(sigma,rss.valid)
  arc.age.valid.out <- cbind(sigma,arc.age.valid)
  arc.time.valid.out <- cbind(sigma,arc.time.valid)
  trend.deviate.valid.out <- cbind(sigma,trend.deviate.valid)
  diag.valid <- cbind(sigma,"rss"=apply(rss.valid,1,sum),
                      "arc.age"=apply(arc.age.valid,1,sum),
                      "arc.time"=arc.time.valid,
                      "trend.deviate"=trend.deviate.valid)
  colnames(diag.valid)[6:7] <- c("arc.time","trend.deviate")
  
  
  return(list(rss.valid=rss.valid.out,arc.age.valid=arc.age.valid.out,
              arc.time.valid=arc.time.valid.out,
              trend.deviate.valid=trend.deviate.valid.out,
              diag.valid=diag.valid,sigma=sigma))
}
