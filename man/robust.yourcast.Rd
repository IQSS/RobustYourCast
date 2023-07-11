\name{robust.yourcast}
\alias{robust.yourcast}

\title{Robust Bayesian Forecasting with YourCast}

\description{Generate Yourcasts under range of prior specifications and
  evaluates predictions with cross validation}

\usage{robust.yourcast(
                            # Pararmeter space to search for priors
                            Ha.sigma.range=c(0.01,20),
                            Ha.list=NULL,
                            Ht.sigma.range=c(0.01,20),
                            Ht.list=NULL,
                            Hat.sigma.range=NA,
                            Hat.list=NULL,
                            N.Ha=5, N.Ht=5,N.Hat=5,
                            logscale=FALSE,

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
                            ...)

}

\arguments{

\item{Ha.sigma.range}{Two element vector of non-negative numbers. Range of Ha.sigma
  values to search over. If do not want to use age smoothing, set as
  \code{NA} and leave \code{Ha.list} as \code{NULL}.}

\item{Ha.list}{Vector. If changed from \code{NULL}, a vector of
  additional values of Ha.sigma to include in the grid search.}

\item{Ht.sigma.range}{Two element vector of non-negative numbers. Range of Ht.sigma
  values to search over. If do not want to use time smoothing, set as
  \code{NA} and leave \code{Ht.list} as \code{NULL}.}

\item{Ht.list}{Vector. If changed from \code{NULL}, a vector of
  additional values of Ht.sigma to include in the grid search.}

\item{Hat.sigma.range}{Two element vector of non-negative numbers. Range of Hat.sigma
  values to search over. If do not want to use trend smoothing, set as
  \code{NA} and leave \code{Hat.list} as \code{NULL}.}

\item{Hat.list}{Vector. If changed from \code{NULL}, a vector of
  additional values of Hat.sigma to include in the grid search.}

\item{N.Ha}{Scalar. Number of values to test (evenly spaced out) along
  Ha.sigma.range. Set to \code{NA} if do not want to use this parameter.}

\item{N.Ht}{Scalar. Number of values to test (evenly spaced out) along
  Ht.sigma.range. Set to \code{NA} if do not want to use this parameter.}

\item{N.Hat}{Scalar. Number of values to test (evenly spaced out) along
  Hat.sigma.range. Set to \code{NA} if do not want to use this parameter.}

\item{logscale}{Logical. Should sigma values be even spaced on a log
  scale and then exponentiated? If \code{FALSE}, sigma values will be
  equally spaced out on normal scale.}

\item{weights}{Vector of length four. Provides weights for the four
  components of the objective function. See \sQuote{Details}.}

\item{time.degree}{Non-negative integer. Specifies the degree of the
  baseline polynomial to which time profiles are smoothed. For example,
  if \code{time.degree=1}, then the forecasts closest to a straight line
  be scored highest. If \code{time.degree=0}, then forecasts closest to
  a flat line will be scored highest.}

\item{length.block}{Numeric. How many years should be omitted at a time
  in validation blocks?}

\item{end.block}{Vector. Specifies years in which validation blocks
  should end. Length of vector determines how many vaidation exercises
  done. Alternatively, if set to \code{"last"}, will choose the last
  possible block in the observed data period only.}

\item{runs.save}{String. If changed from \code{NULL}, specifies a file
  name in the form of \code{*.RData} in which the raw output from
  validation exercises will be stored. Saving this information allows
  users to evaluate the objective function using different weights
  without having to recompute forecasts with yourcast function.}

\item{runs.load}{String. If changed from \code{NULL}, specifies a file
  name in the form of \code{*.RData} from which previous runs of
  yourcast (based on a specific model, dataset and grid of points to
  evaluate) will be loaded. If provided, all other arguments except
  \code{weights} will be ignored.}

\item{print.runs}{Logical. If \code{TRUE}, will print notification each
  time run of \code{yourcast} completed.}

\item{condor}{Logical. Use the Condor batch processing software for
  parallel processing of \code{yourcast} runs on the RCE
  servers. Note: this is only available to members of Harvard's IQSS
  working on the RCE servers.}

\item{condor.dir}{String. Directory in which to write condor
  files. Generated folder will be deleted before function exits.}

\item{condor.fld}{String. A name for the folder in which condor output
  is stored in the \code{condor.dir}. If left as \code{NULL}, folder
  will be given a random name starting with \code{tmp_} and will be
  deleted after the runs are completed and loaded in R. NOTE: This
  will not delete the \code{condor.dir}, but a folder created within it.}

\item{condor.comp}{If changed from \code{NULL}, specifies a folder in
  the \code{condor.dir} in which already completed runs of condor are
  stored. All other arguments supplied in the first run of
  \code{robust.yourcast} must again be supplied, but the function will
  skip sending the jobs to condor and instead load them from the
  specified folder as if they had just been completed. NOTE: All other
  condor-related arguments will be ignored when this is changed from
  \code{NULL} since condor is never called. NOTE: This feature exists
  mainly for debugging purposes and will not be useful to most users.}

\item{...}{Arguments to be passed to \code{yourcast}. See
    \code{help(yourcast)} for more details. *Not clear what prior
    arguments might also be set by user apart from ones here.*}
}

\details{Function to evaluate predictions using \code{yourcast} under
  a range of prior specifications. Given the different values (specified
  by the user) of the three sigma priors to test, the function will
  perform a validation exercise for \code{yourcast} using the blocks of
  time periods specified by the user.

  This validation blocks are specified by indicating the year each
  block should end (\code{end.block}) and the number of years in each
  block (\code{length.block}). The number of years in \code{end.block}
  will determine the number of validation periods. Alternatively, if
  \code{end.block} is set to \code{"last"}, the function will choose
  the last possible block in the observed data period only.

  For each block to be omitted,  \code{robust.yourcast}
  generates a \code{yourcast} input object with those years marked as
  \code{NA} and generates a prediction for that block of responses under
  each of the prior combinations.
  
  The total number of runs of \code{yourcast} is the
  product of the non-\code{NA} \code{N.*} arguments and the number of
  blocks to be omitted. \code{robust.yourcast} processes the
  \code{yourcast} runs locally, or, if \code{condor=TRUE}, parallel on
  the RCE servers with the Condor batch processing software.

  After predictions for the validation blocks under each sigma
  combination are generated, \code{robust.yourcast} calculates values
  of RSS, age profile arc length, time profile arc length, and time
  trend deviation for each. These diagnostics are then used as inputs
  into a univariate objective function that employs weights specified by
  the user to evaluate each set of forecasts.

  Given a length-four vector of weights, the function is

  f(RSS, age AL, time AL, trend dev) = weights[1]*sqrt(RSS) +
  weights[2]*age AL + weights[3]*time AL + weights[4]*trend dev

  The optimal combination of prior values will minimize this function
  over the specified validation periods. 
  
}

\value{
  \item{list}{A list object of class \sQuote{robust.yourcast} with the following
  components:

  \describe{
    \item{par.opt}{A vector of the optimal value for each sigma
      parameter as indicated by the objective function.}
    \item{sigma}{A matrix of all combinations of sigma parameters
      compared in validation exercise.}
    \item{rss.valid}{A matrix that lists the RSS value estimated for
      each combination of sigma parameters in the \code{sigma}
      matrix. Results are broken down by validation period.}
    \item{arc.age.valid}{A matrix that lists the age profile arc length
      value estimated for each combination of sigma parameters in the
      \code{sigma} matrix. Results are broken down by validation period.}
    \item{arc.time.valid}{A matrix that lists the time profile arc
      length value estimated for each combination of sigma parameters in
      the \code{sigma} matrix.}
    \item{trend.dev.valid}{A matrix that lists the trend deviation arc
      length value estimated for each combination of sigma parameters in
      the \code{sigma} matrix.}
    \item{diag.valid}{A matrix that lists all diagnostic values
      estimated for each combination of sigma parameters in
      the \code{sigma} matrix, including the objective function using
      the specified weights. Results are summed over all specified
      validation periods.}
    \item{aux.robust}{A list of information about the run of
      \code{robust.yourcast} used by other functions in the
      \code{RobustYourCast} library.}
}}}

\author{Jon Bischof \email{jbischof@fas.harvard.edu}}

\references{\url{http://gking.harvard.edu/yourcast}}

\seealso{\code{\link{yourcast}}, \code{\link{plot.robust.yourcast}}}
