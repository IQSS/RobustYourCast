# Quantile version of robust.yourcast -- at the moment only works for one geographical area
 # at a time

quantile.yourcast <- function(
                              # Pararmeter space to search for priors
                              Ha.sigma.range=c(0.01,4),
                              Ht.sigma.range=c(0.01,4),
                              N.Ha=5, N.Ht=5,
                              logscale=TRUE,

                              # Diagnostic parameters
                              quantile.rss=c(0,0.8),
                              quantile.move.time=c(0,0.4),
                              quantile.move.age=c(0.2,0.8),
                              validation=FALSE,

                              # Use condor to process runs?
                              condor=FALSE,
                              condor.dir=getwd(),

                              # Inputs to yourcast()
                              # See help(yourcast) for details
                              ...) {


  # See if necessary yourcast inputs supplied; if not grab defaults
  # yourcast inputs from user
  # If necessary inputs supplied, need to move them out of '...'
  # temporarily
  args.yourcast <- list(...)
  yc.in <- names(args.yourcast)
  if(length(grep("sample.frame",yc.in))==0) {
    sample.frame <- formals(yourcast)$sample.frame}
  else{sample.frame <- args.yourcast$sample.frame}
  if(length(grep("formula",yc.in))==0) {
    formula <- formals(yourcast)$formula}
  else{args.yourcast$formula <- as.formula(args.yourcast$formula)
       formula <- args.yourcast$formula
       environment(args.yourcast$formula) <- NULL
     }
  if(length(grep("dataobj",yc.in))==0) {
    stop("Please supply 'dataobj' input for yourcast(). See help(yourcast) for more details")}
  dataobj <- args.yourcast$dataobj
  if(length(grep("model",yc.in))==0) {
    model <- formals(yourcast)$model}
  else{model <- args.yourcast$model}
  if(model=="OLS") {
    Ha.sigma.range <- NA
    Ht.sigma.range <- NA
    Hat.sigma.range <- NA
    }

  
  # Check to see only one geographical area
  # Parse index.code to find out how to extract geo codes
  index.code <- dataobj$index.code
  split.index <- strsplit(index.code,"")[[1]]
  N.g <- length(split.index[split.index=="g"])
  csids <- names(dataobj$data)
  geo.codes <- sapply(csids,substr,start=1,stop=N.g)

  if(length(unique(geo.codes)) > 1) {stop("Function currently only
capable to processing single geographic unit at a time. Please load
only one CSID code into dataobj")}
  
  
  # Recover relevant model details
  # What are the age groups?
  # Parse index.code to find out how to extract age groups
  N.a <- length(split.index[split.index=="a"])
  ages <- as.numeric(sapply(csids,substr,start=N.g+1,stop=N.g+N.a))
  age.min <- min(ages)
  age.max <- max(ages)
  
  # What are the in and out sample years? (parse from sample.frame)
  # Is this a training set exercise?
  year.min <- sample.frame[1]
  year.max <- sample.frame[2]
  year.maxf <- sample.frame[4]

  # What is the response variable?
  response <- all.vars(formula)[1]
  
  # If this a validation exercise, store data from the 'test' set
  # separately and put NA in its place 
  if(validation){
    test.data <- t(sapply(dataobj$data,extract.test.fun,year.max=year.max,
                               year.maxf=year.maxf,response=response))
    rownames(test.data) <- ages

    dataobj.orig <- dataobj
    dataobj$data <- lapply(dataobj$data,train.fun,
             year.min=year.min,year.max=year.max,
             year.maxf=year.maxf,response=response)
    
  }

  # If a noromal prediction, then store all observed mortality data
  # for the purposes of calculating RSS
  else{
  response.array <- matrix(NA, nrow=length(age.min:age.max),
                     ncol=length(year.min:year.max),
                     dimnames=list(age.min:age.max,year.min:year.max))
  for (a in 1:length(age.min:age.max)) {
    response.array[a,] <- dataobj$data[[a]][as.character(year.min:year.max),response]
  }
}

  # Now dump specially loaded yourcast inputs
  #rm(sample.frame,formula,dataobj)

  # Parse prior search specifications to create matrix of possible
  # combinations
  # For now forcing Hat to be NA and others to be non-NA
  if(logscale){
    Ha.sigma.list <- logseq(Ha.sigma.range[1],Ha.sigma.range[2],
                            length.out=N.Ha)
    Ht.sigma.list <- logseq(Ht.sigma.range[1],Ht.sigma.range[2],
                            length.out=N.Ht)}
  else{
    Ha.sigma.list <- seq(Ha.sigma.range[1],Ha.sigma.range[2],
                            length.out=N.Ha)
    Ht.sigma.list <- seq(Ht.sigma.range[1],Ht.sigma.range[2],
                            length.out=N.Ht)}
  Hat.sigma.list <- NA
  sigma <- as.data.frame(expand.grid(Ha.sigma.list, Ht.sigma.list, Hat.sigma.list))
  names (sigma) <- c("Ha.sigma", "Ht.sigma", "Hat.sigma")

  # Number of runs
  runs <- nrow(sigma)

  
  # Run yourcast with each of the combinations of priors
  # Uses 'parse.yourcast' to load prior values into yourcast properly
  
  # If condor option selected, use condor
  if(condor){
    runs.yourcast <- quantile.condor.run(dir=condor.dir,sigma=sigma,
                                         args.yourcast=args.yourcast)
}

  # Else use a simple (if slow) 'sapply()'
  else{
  print(paste("Starting",nrow(sigma),"yourcast() runs"))
  # Extra column to tell user iteration number
  sigma <- cbind(sigma,iter=c(1:nrow(sigma)))
  runs.yourcast <- apply(X=sigma,MARGIN=1,FUN=parse.quantile,
                         condor=condor,args.yourcast=args.yourcast)
  sigma <- sigma[,-ncol(sigma)]
}


  # Now take yourcast output and extract predictions
  pred <- sapply(runs.yourcast,pred.extract,
                 colnames=c(age.min:age.max), simplify=FALSE)

  # Running diagnostic functions
   # RSS diagnostic depends on whether this validation exercise
  if(validation) {rss.pred <- sapply(pred,rss,obs.data=test.data,
                                     year.max=year.max,year.maxf=year.maxf,
                                     validation=validation)}
  else{rss.pred <- sapply(pred,rss,obs.data=response.array,
                          year.min=year.min,year.max=year.max,
                          validation=validation)}
  diff.time <- sapply(pred,movement.over.time)
  diff.age <- sapply(pred,movement.over.age)

  # Concatenating sigma matrix and diagnostic output
  sigma <- cbind(sigma,rss.pred,diff.time,diff.age)

  # Determine cutoff values from vectors of diagnostics
  # 'quantile.*' values provided by user
  rss.cutoff <- quantile(rss.pred, probs=quantile.rss, type=3)
  move.time.cutoff <- quantile(diff.time,
                               probs=quantile.move.time, type=3)
  move.age.cutoff <- quantile(diff.age, probs=quantile.move.age, type=3)
  
  # Decide set of 'kept' predictions: those that meet all cutoff
  # criteria
  index <- 1:runs
  index.kept <- index[rss.pred<=rss.cutoff[2] & rss.pred>=rss.cutoff[1] &
        diff.time<=move.time.cutoff[2] &
        diff.time>=move.time.cutoff[1] &
        diff.age<=move.age.cutoff[2] & diff.age>=move.age.cutoff[1]]

  # Did anything actually make it into the 'kept' set?
  if(length(index.kept)==0){write.table(sigma,file="sigmaout.txt")
    stop("No predictions actually fell within all three quantile
cutoffs! See file 'sigmaout.txt' saved to working directory to see
final sigma matrix.")}

  # 'Final' prediction one with lowest RSS
  index.final <- which(rss.pred==min(rss.pred[index.kept]))
  
  # Saving the kept runs and predictions
  runs.kept <- runs.yourcast[index.kept]
  pred.kept <- pred[index.kept]

  # Choosing final prediction -- kept pred with lowest RSS
  runs.final <- runs.yourcast[index.final]
  pred.final <- pred[index.final][[1]]

  # Mark kept and final in sigma matrix
  kept.vec <- rep(FALSE,runs)
  final.vec <- rep(FALSE,runs)
  kept.vec[index.kept] <- TRUE
  final.vec[index.final] <- TRUE
  sigma <- cbind(sigma,kept=kept.vec,final=final.vec)

  # Saving auxiliary information
  aux.quantile <- list(ages=ages,geo.codes=geo.codes,response=response,
                     sample.frame=sample.frame,index.code=index.code)

  # Create output list and assign it class 'robust.yourcast'
  out.list <- list(runs.kept=runs.kept,runs.final=runs.final,
              pred.kept=pred.kept,pred.final=pred.final,sigma=sigma,
                   aux.quantile=aux.quantile)
  
  class(out.list) <- "quantile.yourcast"
  
  return(out.list)
}
