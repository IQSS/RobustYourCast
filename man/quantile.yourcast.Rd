\name{quantile.yourcast}
\alias{quantile.yourcast}

\title{Robust Bayesian Forecasting with YourCast Using Quantile Diagnostics}

\description{Generate Yourcasts under range of prior specifications and
  evaluates predications with diagnostics}

\usage{quantile.yourcast <- function(
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
                              ...)
}

\arguments{

\item{Ha.sigma.range}{Two element vector of non-negative numbers. Range of sigma.Ha
values to search over.}

\item{Ht.sigma.range}{Two element vector of non-negative numbers. Range of sigma.Ht
values to search over.}

\item{N.Ha}{Scalar. Number of values to test (evenly spaced out) along
  Ha.sigma.range.}

\item{N.Ht}{Scalar. Number of values to test (evenly spaced out) along
  Ht.sigma.range.}

\item{logscale}{Logical. Should sigma values be even spaced on a log
  scale and then exponentiated? If \code{FALSE}, sigma values will be
  equally spaced out on normal scale.}

\item{quantile.rss}{Two element vector of numbers between 0 and 1. Specify quantile
  values of RSS to serve as cutoffs for predictions to be kept. For
  example, if \code{quantile.rss=c(0,0.8)}, only predictions below the
  0.8 quantile for RSS will be candidates to be kept.}

\item{quantile.move.time}{Two element vector of numbers between 0 and 1. Specify
  quantile values of RSS to serve as cutoffs for predictions to be
  kept. For example, if \code{quantile.move.time=c(0.2,0.8)}, only
  predictions below the 0.8 quantile and above the 0.2 quantile for
  move.age.time will be candidates to be kept.}

\item{quantile.move.age}{Two element vector of numbers between 0 and 1. Specify
  quantile values of RSS to serve as cutoffs for predictions to be
  kept. For example, if \code{quantile.move.age=c(0.2,0.8)}, only
  predictions below the 0.8 quantile and above the 0.2 quantile for
  move.age.age will be candidates to be kept.}

\item{validation}{Logical. Is this a validation exercise where
  forecasts from a training set of data are compared to a test set? If
  so, the dataobj will be altered so that values of the response from
  years after the last entry of \code{sample.frame} will be removed and
  values from the test period will be set aside and marked as \code{NA}
  in the \code{dataobj}. Additionally, the RSS diagnostic will only
  compare values of the test set to predictions from the training set in
  those years.}

\item{condor}{Logical. Use the Condor batch processing software for
  parallel processing of \code{yourcast()} runs on the RCE
  servers. Note: this is only available to members of Harvard's IQSS
  working on the RCE servers.}

\item{condor.dir}{String. Directory in which to write condor
  files. Generated folder will be deleted before function exits.}

\item{...}{Arguments to be passed to \code{yourcast()}. See
    \code{help(yourcast)} for more details. *Not clear what prior
    arguments might also be set by user apart from ones here.*}
}

\details{Function to generate predictions using \code{yourcast()} under
  a range of prior specifications. Given the different values (specified
  by the user) of the two priors to test, the function will
  generate predictions from \code{yourcast()} using all possible
  combinations. The total number of runs of \code{yourcast()} is the
  product of the two \code{N.*} arguments.

  \code{quantile.yourcast()} processes the \code{yourcast()} runs
  locally, or, if \code{condor=TRUE}, parallel on the RCE servers with
  the Condor batch processing software.

  After completing the runs of \code{yourcast()} under all combinations
  of prior specifications, \code{quantile.yourcast()} performs a number of
  diagnostics on each set of predictions. The first, \code{rss},
  calculated the residual sum of squares of the predictions when
  compared to the in-sample data. The second, \code{move.time}, computes
  the average sum of absolute first differences of the age profiles over
  time. The third, \code{move.age}, computes the average sum of absolute
  first differences of the time profiles over the different age groups.

  Predictions are then ranked according to their scores on each of the
  diagnostics, and only those that fall within the specified quantile
  cutoffs for all of the diagnostics are placed in the \sQuote{kept} set
  of predictions. The \sQuote{final} prediction is the one in the
  \sQuote{kept} group with the lowest RSS. 

}

\value{
  \item{list}{A list object of class \sQuote{quantile.yourcast} with the following
  components:

  \describe{
    \item{runs.kept}{A list of the complete \code{yourcast()} objects
      that are kept.}
    \item{runs.final}{The final \code{yourcast()} object.}
    \item{pred.kept}{A list of the matrices of the predictions for each
      year and age group from the kept runs.}
    \item{pred.final}{The matrix of predictions from the final run.}
    \item{sigma}{A matrix in which each row records the results of each
      run of \code{yourcast()}. The first three rows specify the prior
      values used, the next three rows show the results of the three
      diagnostics, and the final two rows show whether each run was
      in the kept or final group.}
    \item{aux.quantile}{A list of information about the run of \code{quantile.yourcast()}}
  }
}}

\author{Jon Bischof \email{jbischof@fas.harvard.edu}}

\references{\url{http://gking.harvard.edu/yourcast}}

\seealso{\code{\link{yourcast}} function and documentation
(\code{help(yourcast)})}
