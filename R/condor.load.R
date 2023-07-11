# Function to load already completed condor runs

condor.load <- function(dir,condor.comp) {

  # Set directory path for condor runs
  full.dir.comp <- paste("/",condor.comp,"/",sep="")
  wd.condor <- paste(dir,full.dir.comp,sep="")

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
  #if(condor.rm){
  #system(paste("rm ",wd.condor,"/*",sep=""))
  #system(paste("rmdir",wd.condor))}

  # Return list of output
  return(runs.yourcast)
}
