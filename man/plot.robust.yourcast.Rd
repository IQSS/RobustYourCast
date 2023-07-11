\name{plot.robust.yourcast}
\alias{plot.robust.yourcast}

\title{Plot generation tool for RobustYourCast}

\description{}

\usage{plot.robust.yourcast(x,nparam=2,
                                 screen1=list(z =-30,x=-60),
                                 screen2=list(z =-30,x=-60),
                                 screen3=list(z =-30,x=-60),
                                 screen4=list(z =-30,x=-60),
                                 print="device",
                                 filename="diagplots.pdf",...)
}

\arguments{
  \item{x}{\sQuote{robust.yourcast} output object or equivalent}

  \item{nparam}{Integer. Number of parameters to be varied at a time in
    the diagnostic plots. If \code{nparam=1}, two dimensional plots
    using \code{plot} will be used; if \code{nparam=2}, three dimensional
    plots using \code{wireframe} will be used.}

  \item{screen1}{List. List with three elements \sQuote{x}, \sQuote{y},
    and \sQuote{z} that rotate the viewing angle for three dimensional
    plots (passed to \code{wireframe}). Optimal viewing angles can often
    by found by increasing the \sQuote{z} element by 90 or 180
    degrees. This argument pertains to the first plot (top left) only.}

  \item{screen2}{List. Same as \code{screen1}, but applies to the
    second plot (top right) only.}

  \item{screen3}{List. Same as \code{screen1}, but applies to the
    third plot (bottom left) only.}

  \item{screen4}{List. Same as \code{screen1}, but applies to the
    fourth plot (bottom right) only.}

  \item{print}{String. Specifies whether graphical output should be
    displayed on a device window (\code{"device"}) or saved
    directly to a '.pdf' file (\code{"pdf"}).}

  \item{filename}{String. If \code{"pdf"=TRUE}, the filename of the
    \file{.pdf} to be created.}

  \item{...}{Additional arugments to be passed to plotting method. If
  one parameter allowed to vary will be \code{plot}; if two parameters
  allowed to vary will be \code{wireframe}.}

}


\details{Uses the output of \code{robust.yourcast} to produce plots of
the objective function and its component diagnostics. Since the
function is four-dimensional if no sigma parameters are set to
\code{NA}, only a subset of the parameters can be allowed to vary in
each plot. Therefore if all parameters were varied in the grid search
from \code{robust.yourcast}, the function will produce three plots on
the same device showing different conditional responses.

Parameters not allowed to vary will be held at their optimum value as
identified by the grid search in \code{robust.yourcast}. Users who want
more flexibility in creating diagnostic plots should call the
\code{plot.diag} function directly; this function is intended to give
users a quick summary of the \code{robust.yourcast} output.

The function by default plots the value of the objective function
against the sigma parameters. However, by adding the \code{family}
argument to \code{plot.diag} in the function call, users can also see
graphs of its three components specified by strings \code{"rss"},
\code{"arc.age"}, or \code{"age.time"}.

If \code{nparam=1}, the three plots will each show the conditional
relationship of the diagnostic and one sigma parameter at a time, with
the other parameters held constant at their optimum. If \code{nparam=2},
the three plots will vary two of the parameters at a time with the third
held constant at its optimum. A fourth plot with the all three
parameters varied at once will also be displayed.

The three dimensional plots produced when \code{nparam=2} are sometimes
not shown at an ideal viewing angle. For that reason users are provided
with three \code{screen*} arguments to rotate each of the plots.

With the exception of the \code{screen*} argument, arguments passed to
\code{plot.diag} will be applied to all the plots produced by this
function.  
}

\value{Device windows with requested plots or \file{.pdf} files saved in the
     working directory.}

\author{Jon Bischof \email{jbischof@fas.harvard.edu}}

\references{\url{http://gking.harvard.edu/yourcast}}

\seealso{\code{\link{robust.yourcast}}, \code{\link{plot.diag}}}
