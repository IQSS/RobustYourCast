# Main function -- at the moment only works for one geographical area
 # at a time

robust.yourcast <- function(
                            # Pararmeter space to search for priors
                            Ha.sigma.range=c(0.01,20),
                            Ha.list=NULL,
                            Ht.sigma.range=c(0.01,20),
                            Ht.list=NULL,
                            Hat.sigma.range=NA,
                            Hat.list=NULL,
                            N.Ha=5, N.Ht=5,N.Hat=5,
                            logscale=TRUE,

                            # Weights for objective function
                            weights=c(0.5,0.25,0.25,0),
                            time.degree=1,

                            # Set up blocks
                            length.block=5,
                            end.block="last",

                            # Store validation data for future use?
                            # (changing weights)
                            runs.save=NULL,
                            # Use stored data from previous run?
                            runs.load=NULL,

                            # Verbose sapply() loop?
                            print.runs=TRUE,

                            # Use condor to process runs?
                            condor=FALSE,
                            condor.dir=getwd(),
                            condor.fld=NULL,
                            condor.comp=NULL,

                            # Inputs to yourcast()
                            # See help(yourcast) for details
                            ...) {


  # Is information from a previous run of function supplied? Then load
  # that information and skip most of the function
  if(!is.null(runs.load)){load(runs.load)}

  if(is.null(runs.load)){
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
  #formula.old <- formula
  valid.store <- t(sapply(dataobj$data,function(x){
    x <- as.data.frame(x)[as.character(year.min:year.max),]
    resp <- eval(parse(text=response),envir=x)
    return(resp)
  }))

  colnames(valid.store) <- year.min:year.max

  # Fix weights if necessary
  if(length(weights)!=4){stop("'weights' must be a vector of length four")}
  weights <- weights/sum(weights)

  
  # Parse prior search specifications to create matrix of possible
  # combinations
  
  # Fix small NA bug
  if(all(is.na(Ha.sigma.range))){N.Ha <- NA}
  if(all(is.na(Ht.sigma.range))){N.Ht <- NA}
  if(all(is.na(Hat.sigma.range))){N.Hat <- NA}

  # Create grid of points with regular or log scale
  if(logscale){
    ifelse(is.na(N.Ha),Ha.sigma.list <- NA,
           Ha.sigma.list <- logseq(Ha.sigma.range[1],Ha.sigma.range[2],
                            length.out=N.Ha))
    ifelse(is.na(N.Ht),Ht.sigma.list <- NA,
           Ht.sigma.list <- logseq(Ht.sigma.range[1],Ht.sigma.range[2],
                            length.out=N.Ht))
    ifelse(is.na(N.Hat),Hat.sigma.list <- NA,
           Hat.sigma.list <- logseq(Hat.sigma.range[1],Hat.sigma.range[2],
                            length.out=N.Hat))
  }
  else{
    ifelse(is.na(N.Ha),Ha.sigma.list <- NA,
           Ha.sigma.list <- seq(Ha.sigma.range[1],Ha.sigma.range[2],
                            length.out=N.Ha))
    ifelse(is.na(N.Ht),Ht.sigma.list <- NA,
           Ht.sigma.list <- seq(Ht.sigma.range[1],Ht.sigma.range[2],
                            length.out=N.Ht))
     ifelse(is.na(N.Hat),Hat.sigma.list <- NA,
           Hat.sigma.list <- seq(Hat.sigma.range[1],Hat.sigma.range[2],
                            length.out=N.Hat))
  }

  # Check that user not giving points only from H*.list
  if(all(is.na(Ha.sigma.range)) & !is.null(Ha.list)){
    Ha.sigma.list <- sort(Ha.list)
    Ha.list <- NULL}
  if(all(is.na(Ht.sigma.range)) & !is.null(Ht.list)){
    Ht.sigma.list <- sort(Ht.list)
    Ht.list <- NULL}
  if(all(is.na(Hat.sigma.range)) & !is.null(Hat.list)){
    Hat.sigma.list <- sort(Hat.list)
    Hat.list <- NULL}

  # Add on the list specified values if specified
  if(!is.null(Ha.list)){Ha.sigma.list <- c(Ha.sigma.list,Ha.list)
                      Ha.sigma.list <- sort(Ha.sigma.list)}
  if(!is.null(Ht.list)){Ht.sigma.list <- c(Ht.sigma.list,Ht.list)
                      Ht.sigma.list <- sort(Ht.sigma.list)}
  if(!is.null(Hat.list)){Hat.sigma.list <- c(Hat.sigma.list,Hat.list)
                       Hat.sigma.list <- sort(Hat.sigma.list)}

  
  sigma <- as.data.frame(expand.grid(Ha.sigma.list, Ht.sigma.list,
                                     Hat.sigma.list))
  names (sigma) <- c("Ha.sigma", "Ht.sigma", "Hat.sigma")

  
  # Need more than one sigma combination
  if(nrow(sigma)==1){stop("Need more than one sigma combination...please increase N.Ha, N.Ht, or N.Hat")}

#  # Check that max.rank not greater than number of sigma combinations
#  if(max.rank>nrow(sigma)){max.rank <- nrow(sigma)
#                         warning(paste("'max.rank' value greater than number of prior combinations be ranked. 'max.rank' reset to",nrow(sigma)))}

  # Set up validation exercise
  # Identify years of interest
  # Deal with first year omission problem in sloppy and hopefully
  # short term manner
  
  # Set up years.omit
  years.omit <- seq(year.min+1,year.max)

  if(end.block[1]=="last"){
    if(length(years.omit)<length.block){stop("More years to omit than in sample")}
    max <- years.omit[length(years.omit)]
    blocks <- matrix((max-length.block+1):max,nrow=1)
  }

  # Create blocks from given endpoints and block length
  if(is.numeric(end.block)){
    blocks <- c()
    for(i in 1:length(end.block)){
      blocks <- rbind(blocks,
                      c((end.block[i]-length.block+1):end.block[i]))
    }
  }

  # Give some labels to blocks matrix
  nblocks <- nrow(blocks)
  rownames(blocks) <- paste("run.",1:nrow(blocks),sep="")
  colnames(blocks) <- paste("year.",1:ncol(blocks),sep="")

  # Do all years specified in blocks make sense?
  check.years <- match(as.vector(blocks),years.omit)
  if(any(is.na(check.years))){
    cat("Your blocks:\n")
    print(blocks)
    stop("One of requested blocks includes first year (not allowed) or year not in observed period")
  }

  # Create list of dataobjs with each year omitted
  list.valid <- apply(X=blocks,MARGIN=1,FUN=createvalidobj,
                      dataobj=dataobj,
                      response=all.vars(formula)[1])
 
  # Run yourcast with each of the combinations of priors
  # Uses 'parse.yourcast' to load prior values into yourcast properly
  runs.list <- expand.grid(1:nrow(sigma),1:nblocks)
  colnames(runs.list) <- c("sigma","dataobj")

  # Go ahead with runs unless already completed set of condor runs available
  if(is.null(condor.comp)){
  # If condor option selected, use condor
  if(condor){
    runs.yourcast <- condor.run(dir=condor.dir,
                                sigma=sigma,
                                list.valid=list.valid,
                                runs.condor=runs.list,
                                condor.fld=condor.fld,
                                args.yourcast=args.yourcast)
}

  # Else use a simple (if slow) 'sapply()'
  else{
    if(print.runs){print(paste("Starting",nrow(runs.list),"yourcast() runs"))}
    # Extra column to tell user iteration number
    runs.list <- cbind(runs.list,iter=c(1:nrow(runs.list)))
    # Get rid of dataobj supplied by user 
    pos.dataobj <- grep("dataobj",names(args.yourcast))
    args.yourcast <- args.yourcast[-pos.dataobj]
    runs.yourcast <- apply(X=runs.list,MARGIN=1,FUN=function(x){
      run <- x[length(x)]
      sigma.run <- as.numeric(sigma[runs.list[run,1],])
      dataobj.run <- list.valid[[runs.list[run,2]]]
      run.yourcast <- do.call("parse.robust",
                              append(list(prior.vec=sigma.run,
                                          run=run,
                                          verb.parse=print.runs,
                                          dataobj=dataobj.run),
                                     args.yourcast))
      return(run.yourcast)
    })
    runs.list <- runs.list[,-ncol(runs.list)]
  }
# Closing condor.comp if statement
}

  if(!is.null(condor.comp)){runs.yourcast <- condor.load(dir=condor.dir,
                                               condor.comp=condor.comp)}


  # Now take yourcast output and extract predictions
  pred <- sapply(runs.yourcast,pred.extract,
                 colnames=ages, simplify=FALSE)
  
  # Closing part that runs.load should skip
}

  # Only recalculate diagnostics if time.degree was changed
  if(is.null(runs.load)){old.degree <- -999}
  if(is.null(runs.load) | all(!is.null(runs.load),time.degree!=old.degree)){  
  # Calculate RSS and rank different sigma combinations for each year
  # Attach ranks to sigma matrix
  valid.out <- diags.valid(pred=pred,valid.store=valid.store,
                           runs.list=runs.list,
                           time.degree=time.degree,blocks=blocks,
                           years.omit=years.omit,sigma=sigma,ages=ages)
}
  if(all(!is.null(runs.load),time.degree==old.degree)){valid.out <- old.valid.out}
  sigma <- valid.out$sigma
  rss.valid <- valid.out$rss.valid
  arc.age.valid <- valid.out$arc.age.valid
  arc.time.valid <- valid.out$arc.time.valid
  trend.deviate.valid <- valid.out$trend.deviate.valid
  diag.valid <- valid.out$diag.valid

  
  # Evaluate objective function
  obj.fun.out <- apply(diag.valid[,-c(1:3)],1,function(diag.vec){
    rss <- diag.vec[1]
    arc.age <- diag.vec[2]
    arc.time <- diag.vec[3]
    trend.deviate <- diag.vec[4]
    out <- weights[1]*sqrt(rss) + weights[2]*arc.age +
      weights[3]*arc.time + weights[4]*trend.deviate 
    return(out)
  })

  obj.fun <- cbind(sigma[,c(1:3)],obj.fun=obj.fun.out)
  diag.valid <- as.data.frame(cbind(diag.valid,obj.fun=obj.fun.out))

  # Extract optimal sigma combination
  cand.opt <- which.min(obj.fun[,"obj.fun"])
  # Make sure no ties for optimal combination
  if(length(cand.opt)>1){cand.opt <- cand.opt[length(cand.opt)]
                         warning("Point with minimum objective function not unique; choosing sigma combination with highest smoothing parameters.")}
  par.opt <- as.numeric(obj.fun[cand.opt,-4])
  names(par.opt) <- c("Ha.sigma","Ht.sigma","Hat.sigma")

  # Save data from runs if requested
  old.degree <- time.degree
  old.valid.out <- valid.out
  if(!is.null(runs.save)){
    save(pred,valid.store,old.valid.out,old.degree,runs.list,blocks,
         years.omit,sigma,ages,geo.codes,response,sample.frame,
         index.code,args.yourcast,dataobj,file=runs.save)
  }


  # Put back in original dataobj
  args.yourcast$dataobj <- dataobj
  # Saving auxiliary information
  aux.robust <- list(ages=ages,geo.codes=geo.codes,response=response,
                     sample.frame=sample.frame,index.code=index.code,
                     years.omit=years.omit,blocks=blocks,
                     length.block=length.block,end.block=end.block,
                     args.yourcast=args.yourcast,weights=weights)

  # Create output list and assign it class 'robust.yourcast'
  out.list <- list(par.opt=par.opt,sigma=sigma,rss.valid=rss.valid,
                   arc.age.valid=arc.age.valid,
                   arc.time.valid=arc.time.valid,
                   trend.deviate=trend.deviate.valid,
                   diag.valid=diag.valid,
                   obj.fun=obj.fun,
                   aux.robust=aux.robust)
  class(out.list) <- "robust.yourcast"
  return(out.list)
}

  


