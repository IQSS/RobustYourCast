# Function to compute yourcast runs through condor batch processing
 # system
# This version for the quantile version of robust.yourcast

quantile.condor.run <- function(dir,sigma,args.yourcast) {
  runs <- nrow(sigma)
 
  # Create temporary directory in working directory and set as new wd
  dir.gen <- paste("tmp_",paste(letters[sample(c(1:26),4)],collapse=""),
                   paste(sample(c(0:9),4),collapse=""),sep="")
  full.dir.gen <- paste("/",dir.gen,"/",sep="")
  wd.condor <- paste(dir,full.dir.gen,sep="")

  status <- system(paste("mkdir -p",wd.condor))
  stopifnot(status==0)

  # Save sigma and args to yourcast
  save(sigma,args.yourcast,
       file=paste(wd.condor,"yourcast.inputs.RData",sep=""))

  # Create parse function
  l.1 <- "parse.condor <- function(prior.vec,...) {"
  l.2 <- "Ha.sigma <- prior.vec[1]"
  l.3 <- "Ht.sigma <- prior.vec[2]"
  l.4 <- "Hat.sigma <- prior.vec[3]"
  l.5 <- "out <- yourcast(Ha.sigma=Ha.sigma,Ht.sigma=Ht.sigma,Hat.sigma=Hat.sigma,...)"
  l.6 <- "return(out)}"
  cat(l.1,l.2,l.3,l.4,l.5,l.6,fill=2,file=paste(wd.condor,"parse.condor.R",sep=""))
  
  # Create R script for condor
  #wdset <- paste("setwd(\"",wd.condor,"\")",sep="")
  sys <- "Sys.setenv(GOTO_NUM_THREADS=1)"
  run <- "run <- commandArgs(TRUE); run <- as.numeric(run) + 1"
  print <- "print(run)"
  parser <- paste("source(\"",wd.condor,"/parse.condor.R\")",sep="")
  load <- paste("load(file=\"",wd.condor,"/yourcast.inputs.RData\")",sep="")
  lib <- "library(YourCast)"
  sig <- "sigma.run <- as.numeric(sigma[run,])"
  run.name <- "run.name <- paste(\"run.yourcast\",as.character(run),sep=\"\")"
  run.yc <- "assign(run.name, do.call(\"parse.condor\",append(list(prior.vec=sigma.run),args.yourcast)))" 
  file <- paste("file.name <- paste(\"",wd.condor,"/ycrun\",run,\".RData\",sep=\"\")",sep="")
  save.yc <- "do.call(\"save\",args=list(paste(run.name),file=file.name))"
  warn <- paste("if(!is.null(warnings())){sink(file=\"",dir,"/warnings.txt\",append=TRUE);print(warnings());sink()}",sep="")
  cat(sys,run,print,parser,load,lib,sig,run.name,run.yc,file,save.yc,warn,
      fill=2,file=paste(wd.condor,"condor.script.R",sep=""))

  
  # Create submit file for condor
  uni <- "Universe = vanilla"
  exe <- "Executable = /usr/bin/R"
  trans.out <- "when_to_transfer_output = ON_EXIT_OR_EVICT"
  req <- "Requirements = Memory >= 256"
  output <- "Output = out.$(Process)"
  err <- "Error = error.$(Process)"
  # these values need to be updated
  # this is "-i" to submit_util
  input <- paste("Input = ",wd.condor,"/condor.script.R",sep="")
  # this is "-a" to submit_util
  args <- "Arguments = --vanilla --no-save --args $(Process)"
  # this is "-l" to submit_util
  log <- "Log = log.condor"
  # this is "-n" to submit_util
  queue <- paste("Queue",runs)
  cat(uni,exe,trans.out,req,output,err,input,args,log,queue,
      fill=2,file=paste(wd.condor,"condor.submit.txt",sep=""))

  
  # Run condor and wait for it to finish
  status <- system(paste("cd ",wd.condor,
                         "; condor_submit condor.submit.txt",sep=""),
                   ignore.stderr=TRUE)
  stopifnot(status==0)

  status <- system(paste("cd ",wd.condor,"; condor_wait log.condor",sep=""))
  stopifnot(status==0)

  # Load .RData objects
  files.to.load <- dir(wd.condor)[grep("ycrun",dir(wd.condor))]
  #sapply(files.to.load,load)
  runs.yourcast <- list()
  for(i in 1:length(files.to.load)) {
    load(paste(wd.condor,"/",files.to.load[i],sep=""))
  }

  # Bizarrely, define new lapply function not in base envir
  new.lapply <- function (X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    .Internal(lapply(X, FUN))
 }

  # Load all the yourcast runs into a list
  runlist <- ls(pattern="run.yourcast")
  runs.yourcast.load <- new.lapply(runlist,get)
  # Sort runs.yourcast so that its order matches runs.list
  run.ord <- as.numeric(unlist(lapply(runlist,substring,
                                      first=nchar("run.yourcast")+1)))
  run.sort <- order(run.ord)
  runs.yourcast <- runs.yourcast.load[run.sort]

  # Clean up---Delete all condor files
  system(paste("rm ",wd.condor,"/*",sep=""))
  system(paste("rmdir",wd.condor))
  
  # Return list of output
  return(runs.yourcast)
}

