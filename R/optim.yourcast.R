# Main function -- at the moment only works for one geographical area
 # at a time
# This version uses optim rather than a slower grid search
# Should be fast if less robust

optim.yourcast <- function(# Robust.yourcast output object?
                           robust.out=NULL,
                           
                           # Starting values for optim
                           # Vector order is c(Ha,Ht,Hat)
                           # Set any dimension to 'NA' if don't want
                           # to use
                           par=ifelse(rep(is.null(robust.out),3),c(1,1,1),robust.out$par.opt),

                           # Constrained or unconstrained
                           # optimization?
                           reparam=TRUE,
                           
                           # Arguments for optim
                           method="BFGS",
                           args.optim=list(),

                           # Use rgenoud
                           rgenoud=FALSE,
                           upper.bound=100,
                           args.rgenoud=list(),

                           # Weights for objective function
                           weights=c(0.5,0.25,0.25,0),
                           time.degree=1,

                           # Set up blocks
                           length.block=5,
                           end.block="last",

                           # Inputs to yourcast()
                           # See help(yourcast) for details
                           ...) {


  # Is robust.yourcast output object provided?
  if(!is.null(robust.out)){
    #par <- robust.out$par.opt
    weights <- robust.out$aux.robust$weights
    args.yourcast <- robust.out$aux.robust$args.yourcast
    length.block <- robust.out$aux.robust$length.block
    end.block <- robust.out$aux.robust$end.block
  }
  
  # See if necessary yourcast inputs supplied; if not grab defaults
  # yourcast inputs from user
  # If necessary inputs supplied, need to move them out of '...'
  # temporarily
  if(is.null(robust.out)){args.yourcast <- list(...)}
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
  if(model=="OLS") {stop("OLS model does not allow smoothing; choose different model")}

  
  # Check to see only one geographical area
  # Parse index.code to find out how to extract geo codes
  index.code <- dataobj$index.code
  split.index <- strsplit(index.code,"")[[1]]
  N.g <- length(split.index[split.index=="g"])
  csids <- names(dataobj$data)
  geo.codes <- sapply(csids,substr,start=1,stop=N.g)

  if(length(unique(geo.codes)) > 1) {stop("Function currently only capable to processing single geographic unit at a time. Please load only one CSID code into dataobj")}
  
  
  # Recover relevant model details
  # What are the age groups?
  # Parse index.code to find out how to extract age groups
  N.a <- length(split.index[split.index=="a"])
  ages <- as.numeric(sapply(csids,substr,start=N.g+1,stop=N.g+N.a))
  age.min <- min(ages)
  age.max <- max(ages)
  
  # What are the in and out sample years? (parse from sample.frame)
  year.min <- sample.frame[1]
  year.max <- sample.frame[2]
  year.maxf <- sample.frame[4]

  # What is the response variable?
  formula <- as.formula(formula)
  response <-  deparse(attr(terms(formula),which="variables")[[2]])

  # Store validation data
  valid.store <- t(sapply(dataobj$data,function(x){
    x <- as.data.frame(x)[as.character(year.min:year.max),]
    resp <- eval(parse(text=response),envir=x)
    return(resp)
  }))

  colnames(valid.store) <- year.min:year.max

  # Fix weights if necessary
  if(length(weights)!=4){stop("'weights' must be a vector of length four")}
  weights <- weights/sum(weights)

  # Figure out which type of smoothing desired
  which.na <- which(is.na(par))
  par <- as.numeric(na.omit(par))

  # Set up validation exercise
  # Identify years of interest
  # Deal with first year omission problem in sloppy and hopefully
  # short term manner
  
  # Set up years.omit
  years.omit <- seq(year.min+1,year.max)
  if(length(years.omit)<length.block){stop("More years to omit than in sample")}

  # Set up validation block
  ifelse(end.block=="last",max <- years.omit[length(years.omit)],
         max <- end.block)
  block <- c((max-length.block+1):max)

  
  # Create dataobj for validation
  dataobj.valid <- createvalidobj(block.row=block,dataobj=dataobj,
                                  response=all.vars(formula)[1])

  # Fix args.yourcast to remove original dataobj
  pos.dataobj <- grep("dataobj",names(args.yourcast))
  args.yourcast <- args.yourcast[-pos.dataobj]

  
  # Optimization
  
  # Change starting values if reparameterization requested
  ifelse(reparam,start <- log(par),start <- par)

  # Use optim if requested
  if(!rgenoud){
    # Create list of arguments for optim
    list.optim <- append(list(block=block,
                       which.na=which.na,ages=ages,weights=weights,
                       dataobj.valid=dataobj.valid,valid.store=valid.store,
                       args.yourcast=args.yourcast,
                       reparam=reparam,time.degree=time.degree),args.optim)

    # Set obvious things
    list.optim$par <- start
    list.optim$fn <- optim.fn

    if(reparam){if(is.null(list.optim$method)){list.optim$method <- "BFGS"}}
    # If not reparameterizing, method needs to be L-BFGS-B
    if(!reparam){list.optim$method <- "L-BFGS-B"}
    # If not reparameterizing, have lower bound of zero
    if(!reparam){
      if(is.null(list.optim$lower)){
        list.optim$lower <- 0.0000001
    }}
    if(is.null(list.optim$control)){list.optim$control <- list(trace=1)}
    if(is.null(list.optim$control$trace)){list.optim$control$trace <- 1}
    
    # Run optim using specified starting values
    out.optim <- do.call("optim",list.optim)

  if(out.optim$convergence!=0){warning("optim() failed to converge or hit maximum number of iterations")}
}

  # Use genetic optimization if requested
  if(rgenoud){
    list.genoud <- append(list(fn=optim.fn,
                               nvars=length(par),
                               reparam=reparam,
                               time.degree=time.degree,
                               starting.values=start,
                               block=block,
                               which.na=which.na,ages=ages,
                               weights=weights,
                               dataobj.valid=dataobj.valid,
                               valid.store=valid.store,
                               args.yourcast=args.yourcast),
                          args.rgenoud)

    # Change some of the defaults if user does not override
    if(is.null(list.genoud$pop.size)){list.genoud$pop.size <- 30}
    if(is.null(list.genoud$max.generations)){list.genoud$max.generations <- 10}
    if(is.null(list.genoud$wait.generations)){list.genoud$wait.generations <- 3}
    if(is.null(list.genoud$gradient.check)){list.genoud$gradient.check <- FALSE}

    # If no reparam and user doesn't specify Domains, create own with
    # upper.bound argument
    if(!reparam){
       if(is.null(list.genoud$Domains)){
         list.genoud$Domains <- matrix(rep(c(0.0000001,upper.bound),
                                       length(par)),
                                   nrow=length(par),ncol=2,byrow=TRUE)
       }
     }
    
    # Run optim
    out.optim <- do.call("genoud",list.genoud)
  }

  # Get optim and undo reparameterization if used
  ifelse(reparam,par.out <- exp(out.optim$par),par.out <- out.optim$par)
  if(length(which.na)==0){par.opt <- par.out}
  if(length(which.na)>0){par.opt <- rep(NA,3)
                         par.opt[-which.na] <- par.out}
  names(par.opt) <- c("Ha.sigma","Ht.sigma","Hat.sigma")

  # Put back in original dataobj
  args.yourcast$dataobj <- dataobj
  # Determine method
  if(rgenoud){method <- "rgenoud"}
  # Saving auxiliary information
  aux.robust <- list(ages=ages,geo.codes=geo.codes,response=response,
                     sample.frame=sample.frame,index.code=index.code,
                     years.omit=years.omit,blocks=block,method=method,
                     args.yourcast=args.yourcast,weights=weights)

  # Create output list and assign it class 'robust.yourcast'
  out.list <- list(par.opt=par.opt,
                   aux.robust=aux.robust)
  class(out.list) <- "optim.yourcast"
  return(out.list)
}

  


