\name{optim.yourcast}
\alias{optim.yourcast}

\title{Robust Bayesian Forecasting with YourCast Using Optimization}

\description{Finds sigma parameters that produce optimal forecast using
  optimization algorithms}

\usage{optim.yourcast(# Robust.yourcast output object?
                      robust.out=NULL,
                           
                      # Starting values for optim
                      # Vector order is c(Ha,Ht,Hat)
                      # Set any dimension to 'NA' if don't want
                      # to use
                      par=ifelse(rep(is.null(robust.out),3),
                                 c(1,1,1),robust.out$par.opt),

                      # Constrained or unconstrained optimization?
                      reparam=TRUE,
                           
                      # Arguments for optim
                      method="BFGS",
                      args.optim=list(),

                      # Use rgenoud?
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
                      ...)     

}

\arguments{

  \item{robust.out}{A object of class \sQuote{robust.yourcast}. If
    changed from \code{NULL}, will take information from previous run of
    \code{robust.yourcast} to continue optimization where that function
    left off. Specifically, the function will use the optimal sigma
    combination identified for the \code{par} argument and recover all
    arguments pertaining to the weights, blocks, and yourcast input from
    the output object. However, a different starting point can be
    specified if the \code{par} argument is changed from its
    default. All arguments after \code{args.rgenoud} will be ignored.}

  \item{par}{Vector of length three. Provides starting values for the
    optimization algorithm. The entries in the vector coorespond to
    Ha.sigma, Ht.sigma, and Hat.sigma, respectively.}
  
  \item{reparam}{Logical. Since objective function lives in positive
    quadrant/octant, should parameters be transformed to log-space to
    allow unconstrained optimization? If \code{FALSE}, only
    \code{"L-BFGS-B"} method will be allowed for \code{optim}. For both
    \code{optim} and \code{rgenoud} lower boundries close to zero will
    be enforced; the upper boundary for \code{rgenoud} can be set with
    the \code{upper.bound} argument.}

  \item{args.optim}{List. If \code{rgenoud=FALSE}, a list of arguments
  (must be labeled) to be passed to \code{optim}. For example, if wanted
  to turn off verbose option, could add \code{control=list(trace=0)}.}

  \item{rgenoud}{Logical. Should \code{rgenoud} be used instead of
    \code{optim} for the optimization?}

  \item{upper.bound}{Numeric. If \code{rgenoud=TRUE} and
  \code{reparam=FALSE}, specifies an upper bound for parameters. If
  \code{reparam=FALSE}, the lower bound is set close to zero. Whether or
  not \code{reparam=TRUE}, the user can set his or her own bounds for
  \code{rgenoud} by adding the \code{Domains} argument to
  \code{args.rgenoud}.}

  \item{weights}{Vector of length four. Provides weights for the four
    components of the objective function. See \sQuote{Details}.}

  \item{time.degree}{Non-negative integer. Specifies the degree of the
  baseline polynomial to which time profiles are smoothed. For example,
  if \code{time.degree=1}, then the forecasts closest to a straight line
  be scored highest. If \code{time.degree=0}, then forecasts closest to
  a flat line will be scored highest.}

  \item{length.block}{Numeric. How many years should be omitted at a time
  in the validation block?}

  \item{end.block}{Numeric. Specifies year in which validation block
  should end.  Alternatively, if set to \code{"last"}, will choose the
  last possible block in the observed data period. Unlike
  \code{robust.yourcast}, in this function only one validation block can
  be used at a time.}

  \item{...}{Arguments to be passed to \code{yourcast}.}
}

\details{Function to find the optimal sigma parameters for
  \code{yourcast} forecasts using an optimization algorithm. Starting from
  values for the sigma parameters specified in \code{par},
  \code{optim.yourcast} will call either \code{optim} or \code{rgenoud}
  to search over the parameter space for the point that performs best in
  a validation exercise.

  \code{optim.yourcast} only allows a single validation period to be
  used at a time. This period is specified by indicating in which year the
  block should end (\code{end.block}) and the number of years in the
  block (\code{length.block}). Alternatively, if \code{end.block} is set
  to \code{"last"}, the function will choose the last possible block in
  the observed data period.

  To set up the validation, \code{optim.yourcast} generates a
  \code{yourcast} input object with a block of validation
  years specified by the user marked as \code{NA} and then for each
  set of parameter values guessed by the optimization routine generates a
  forecast for that block of responses.

  The quality of the forecast is quantified by an objective
  function which considers four diagnostics: the sum of squares of the
  prediction error for the block (RSS), the arc length of the age
  profile for that block (age AL), the arc length of the time
  profile for that block (time AL), and the deviations from the mean
  time trend (trend dev). Thus the ideal forecast will produce
  the most linear trends possible while minimizing prediction error.

  Given a length-four vector of weights specified in the \code{weights}
  argument, the objective function is

  f(RSS, age AL, time AL) = weights[1]*sqrt(RSS) + weights[2]*age AL +
  weights[3]*time AL + weights[4]*trend dev

  The optimal combination of sigma parameters will minimize this function
  for the validation period.

  Since the parameter space is restricted to positive values for
  Ha.sigma, Ht.sigma, and Hat.sigma, it is necessary to use constrained
  optimization (on [0,Inf]) or reparameterize to log-space (so that the
  algorithm guesses of log(*.sigma) cannot be negative when transformed
  back to the original space). By default, \code{optim.yourcast} uses
  reparameterization since unconstrained optimization is more
  straightforward.

  In practice, the objective function is rarely unimodal. Thus most
  optimization algorithms will fail to find the global minima if started
  in the wrong place. We recommend that users first perform a grid
  search with \code{robust.yourcast} and then start
  \code{optim.yourcast} at the best guess found by the grid search. If
  users adopt this strategy, they can pass their \code{robust.yourcast}
  output object to \code{optim.yourcast} with the \code{robust.out}
  argument. Then only arguments pertaining to the optimization will need
  to be considered; arguments pertaining to weights, validation blocks,
  and \code{yourcast} inputs will be recovered from the output object.
}

\value{
  \item{list}{A list object of class \sQuote{optim.yourcast} with the following
  components:

  \describe{
    \item{par.opt}{A vector of the optimal value for each sigma
      parameter as indicated by the objective function.}
    \item{aux.robust}{A list of information about the run of
      \code{optim.yourcast} used by other functions in the
      \code{RobustYourCast} library.}
}}}

\author{Jon Bischof \email{jbischof@fas.harvard.edu}}

\references{\url{http://gking.harvard.edu/yourcast}}

\seealso{\code{\link{yourcast}},\code{\link{robust.yourcast}},\code{\link{run.opt}}}
