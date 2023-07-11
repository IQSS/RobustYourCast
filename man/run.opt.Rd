\name{run.opt}
\alias{run.opt}

\title{Generate forecasts using optimal sigma combinations}

\description{Uses output from robust.yourcast or optim.yourcast function
to call yourcast using the identified optimal sigma combination and
other arguments sent to yourcast in original call to robust.yourcast
or optim.yourcast. Will produce a plot of the forecasts if requested
using plot.yourcast.}

\usage{run.opt(x,quant="best",plot=TRUE,
               Ha.set=NULL,Ht.set=NULL,Hat.set=NULL,
               create.main=TRUE,...)
}

\arguments{
  \item{x}{\sQuote{robust.yourcast} or \sQuote{optim.yourcast} output
    object or equivalent}

  \item{quant}{Numeric. If using \sQuote{robust.yourcast} output object,
  specifies the sigma combination to be plotted by its quantile of the
  objective function among those tested. For example, if
  \code{quant=0.5}, the function will use the median sigma combination
  considered. If left as \code{"best"}, will use combination with
  lowest objective function value. If using \sQuote{optim.yourcast}
  output object, will be fixed to \code{"best"}.}

  \item{plot}{Logical. Should yourcast output object be plotted?}

  \item{Ha.set}{Numeric. If changed from \code{NULL}, specifies an
    alternate value of Ha.sigma to be used in yourcast() run.}

  \item{Ht.set}{Numeric. If changed from \code{NULL}, specifies an
    alternate value of Ht.sigma to be used in yourcast() run.}

  \item{Hat.set}{Numeric. If changed from \code{NULL}, specifies an
    alternate value of Hat.sigma to be used in yourcast() run.}

  \item{create.main}{Logical. If \code{plot=TRUE}, should a title for the plots
    be created that lists the sigma combination used and its quantile
    (or \sQuote{Best}) of the objective function?}

  \item{...}{Additional arugments to be passed to
    plot.yourcast(). Commonly used arguments are \code{print} and
    \code{filename}.}

}

\details{Extracts the optimal sigma combination from a
  \code{robust.yourcast} or \code{optim.yourcast} output object and then generates
  predictions with those sigma values by calling \code{yourcast}. Other
  arguments to \code{yourcast}, including the original data, are also
  extracted from the output object.

  If \code{plot=TRUE}, the function will also produce a plot of the resulting
  forecasts by sending the \code{yourcast} output object to
  \code{plot.yourcast}. 

}

\value{\code{yourcast} object called with same arguments supplied to
  \code{robust.yourcast} or \code{optim.yourcast} and most desireable sigma
  combination identified by the respective function. If \code{plot=TRUE}
  will also create a plot of the forecast using plot.yourcast printed to
  the device window or to a \file{.pdf} file. If
  \code{create.main=TRUE}, the function will create an informative main
  title for the plots the lists the optimal sigma combination used.}

\author{Jon Bischof \email{jbischof@fas.harvard.edu}}

\references{\url{http://gking.harvard.edu/yourcast}}

\seealso{\code{\link{robust.yourcast}}, \code{\link{optim.yourcast}},
  \code{\link{plot.yourcast}}}
