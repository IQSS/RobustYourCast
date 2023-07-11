# Bizarrely, define new lapply function not in base envir
  new.lapply <- function (X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    .Internal(lapply(X, FUN))
 }
