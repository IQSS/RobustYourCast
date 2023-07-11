# Objective function to be used by optim in optim.yourcast

optim.fn <- function(par,block,which.na,ages,weights,dataobj.valid,
                     valid.store,args.yourcast,reparam,time.degree){

  # Reparameterize if desired
  if(reparam){par <- exp(par)}
  
  # Enforce possible NAs desired in par vec
  if(length(which.na)==0){prior.vec <- par}
  if(length(which.na)>0){prior.vec <- rep(NA,3)
                       prior.vec[-which.na] <- par}

  # Run yourcast on validation object
  run.yourcast <- do.call("parse.robust",
                            append(list(prior.vec=par,
                                        verb.parse=FALSE,
                                        dataobj=dataobj.valid),
                                   args.yourcast))

  # Extract predictions
  pred <- pred.extract(run.list=run.yourcast,colnames=ages)

  runs.list <- matrix(c(1,1),1,2)
  years.omit <- "Year"
  sigma <- matrix(par,nrow=1,ncol=3)
  colnames(runs.list) <- c("sigma","dataobj")
  blocks <- t(as.matrix(block))

  valid.out <- diags.valid(pred=list(pred),valid.store=valid.store,
                           runs.list=runs.list,time.degree=time.degree,
                           blocks=blocks,years.omit=years.omit,
                           sigma=sigma,ages=ages)

  diag.valid <- valid.out$diag.valid[,-c(1:3)]
  #print(valid.out)


  
#
#  # Calculate diagnostics
#  # RSS
#  pos <- as.character(block)
#  rss.valid <- sum((pred[,pos]-valid.store[,pos])^2,na.rm=TRUE)
#
#  
#  # Arc length - age profile
#
#  # Create detrended age profiles--take out mean age profile
#  mean.age.profile <- apply(pred,1,mean)
#  pred.age.detrend <- pred-mean.age.profile
#  
#  arc.age.out <- apply(pred.age.detrend[,pos],2,function(resp){
#    spline.out <- spline(x=ages, y=resp,
#                         xout=seq(min(ages),max(ages),
#                           length.out=length(ages)*4))
#    arc.length <- sum(abs(diff(spline.out$y)),na.rm=TRUE)
#    return(arc.length)
#  })
# 
#  arc.age.valid <- sum(arc.age.out,na.rm=TRUE)
#
#  
#  # Arc length - time profile
#
#  # Create detrended time profiles--take out desired trend
#  time <- as.numeric(colnames(pred))
#  # Factor is ratio of years used here to years used for RSS and
#  # arc.age
#  discount <- length(block)/length(time)
#
#  # terms to include in regression
#  # Baseline regression
#  formula <- as.formula("row.vec ~ 1")
#
#  # Add terms up to desired degree
#  if(time.degree>0) {
#    terms <- 1:time.degree
#    x.terms <- paste("+ I(time^",terms,")",sep="",collapse="")
#    formula <- update.formula(formula,paste("~ .",x.terms,sep=""))
#  }
#
#  resid.time <- apply(pred,1,function(row.vec){
#    data.lm <- as.data.frame(cbind(row.vec=row.vec,time=time))
#    return(resid(lm(formula,data=data.lm)))})
#  pred.time.detrend <- t(resid.time)
#
#  # Calculate lengths of time profiles
#  arc.time.out <- apply(pred.time.detrend,1,function(resp){
#    spline.out <- spline(x=colnames(pred), y=resp,
#                         xout=seq(time[1],time[ncol(pred)],
#                           length.out=ncol(pred)*4))
#    #spline.out <- spline(x=block, y=resp,
#    #                     xout=seq(min(block),max(block),
#    #                         length.out=length(block)*4))
#    arc.length <- sum(abs(diff(spline.out$y)),na.rm=TRUE)
#  })
#
#  arc.time.valid <- sum(arc.time.out,na.rm=TRUE)*discount
#
#  
#  # Time trend smoothness
#
#  # Calculate demeaned time profiles
#  demean.series <- t(apply(pred,1,function(row.vec){
#    row.vec-mean(row.vec)}))
#  # Mean demeaned profile
#  mean.profile <- apply(demean.series,2,mean)
#  pred.time.deviate <- t(apply(demean.series,1,function(row.vec){
#      row.vec-mean.profile}))
#  
#  # Calculate arc length of deviations from mean demeaned profile 
#    
#    # Deviations from mean profile
#  deviate.trend <- apply(pred.time.deviate,1,function(resp){
#    spline.out <- spline(x=colnames(pred), y=resp,
#                         xout=seq(time[1],time[ncol(pred)],
#                           length.out=ncol(pred)*4))
#    deviate.length <- sum(abs(diff(spline.out$y)))
#    return(deviate.length)})
#
#  deviate.trend.valid <- sum(deviate.trend,na.rm=TRUE)*discount


  # Evaluate objective function
  out <- weights[1]*sqrt(diag.valid[1])+weights[2]*diag.valid[2]+
    weights[3]*diag.valid[3] + weights[4]*diag.valid[4] 

  return(out)
    
}
