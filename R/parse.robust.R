parse.robust <- function(prior.vec,run=1,verb.parse=TRUE,...) {

  Ha.sigma <- prior.vec[1]
  Ht.sigma <- prior.vec[2]
  Hat.sigma <- prior.vec[3]

  out <- yourcast(Ha.sigma=Ha.sigma,
                  Ht.sigma=Ht.sigma,
                  Hat.sigma=Hat.sigma,
                  ...)

  if(verb.parse){print(paste("Done with run",run))}

  return(out)
}
