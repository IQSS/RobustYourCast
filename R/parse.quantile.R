parse.quantile <- function(prior.vec,condor,args.yourcast) {

  Ha.sigma <- prior.vec[1]
  Ht.sigma <- prior.vec[2]
  Hat.sigma <- prior.vec[3]
  if(!condor) {iter <- prior.vec[4]}

  out  <- do.call("yourcast",
                  append(list(Ha.sigma=Ha.sigma,
                  Ht.sigma=Ht.sigma,
                  Hat.sigma=Hat.sigma),args.yourcast))

  if(!condor) {print(paste("Done with run",iter))}

  return(out)
}
