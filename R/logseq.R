# Function to create a sequence with points set on log scale

logseq <- function(from,to,...) {
  if(any(from<=0,to<=0)) {stop("Cannot have bounds less than or equal to zero")}
  a <- log(from)
  b <- log(to)
  log.seq <- seq(a,b,...)
  out <- exp(log.seq)
  return(out)
}
