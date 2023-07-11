\name{plot.diag}
\alias{plot.diag}

\title{Plot objective function or component diagnostics}

\description{Uses output from robust.yourcast function to plot the
  surface of the objective function or its component diagnostics over
  different combinations of the three sigma parameters.}

\usage{plot.diag(x,fix.param=list("vary","vary","opt"),
                      family="obj.fun",
                      lattice.plot="wireframe",
                      screen=list(z=-30,x=-60),
                      print="device",
                      filename="objplot.pdf",
                      args.par=list(),
                      args.print.trellis=list(),...)
}

\arguments{
  \item{x}{\sQuote{robust.yourcast} or \sQuote{optim.yourcast} output
    object or equivalent}

  \item{fix.param}{List. A list of length three that specifies which of
    the sigma parameters will be varied and how the others will be
    fixed. The three elements of the list coorespond to Ha.sigma,
    Ht.sigma, and Hat.sigma, respectively. List elements may take value
    \code{"vary"} if the parameter is to be varied, \code{"opt"} if the
    parameter is to be held fixed at is optimum value (as evaluated by
    the objective function), or an arbitrary numeric value at which that
    parameter is to be fixed. If the element is a numeric value, the
    function will look for the closest value at which the objective
    function was evaluated to hold the parameter constant. Naturally, at
    least one parameter must be varied. If any of the parameters was
    left as \code{NA} in the grid search, the function will
    automatically hold it fixed at \code{NA} regardless of the value of
    its corresponding list element.}

  \item{family}{String. Indicates the surface to be
    plotted. The default, \code{"obj.fun"}, indicates the objective
    function, and its four components are specified by strings
    \code{"rss"}, \code{"arc.age"}, \code{"arc.time"}, or
    \code{"trend.deviate"}.}

  \item{lattice.plot}{String. Type of plot in \code{lattice} package to
    be used. The default is \code{wireframe}, but \code{levelplot},
    \code{cloud}, and \code{contourplot} will also work. Note: this only
    applies when exactly two of the parameters are not fixed.}

  \item{screen}{List. List with three elements \sQuote{x}, \sQuote{y},
    and \sQuote{z} that rotate the viewing angle for three dimensional
    plots (passed to \code{wireframe}). Optimal viewing angles can often
    by found by increasing the \sQuote{z} element by 90 or 180
    degrees.}

  \item{print}{String. Specifies whether graphical output should be
    displayed on a device window (\code{"device"}) or saved
    directly to a \file{.pdf} file (\code{"pdf"}).}

  \item{filename}{If \code{print="pdf"}, specifies the filename of the
    \file{.pdf} created.}

  \item{args.par}{List. If only one variable is allowed to vary (so that
    \code{plot} is the plotting method), a list of arguments (must be
    labeled) to be passed to \code{par} such as \code{col="blue"},
    \code{cex=0.8}, etc.}

  \item{args.print.trellis}{List. If two variables allowed to vary (so
  that \code{wireframe} is the plotting method), a list of arguments
  (must be labeled) to be passed to \code{print.trellis} function used
  to print \code{wireframe} plot to the device. Used by
  \code{plot.robust.yourcast} to print multiple plots to the same
  device.}
  
  \item{...}{Additional arugments to be passed to plotting method. If
  one parameter allowed to vary will be \code{plot}; if two parameters
  allowed to vary will be \code{wireframe}.}

}

\details{Function plots the surface of the objective function or any of
  its three component diagnostics using the grid search output from
  \code{robust.yourcast}. The \code{fix.param} argument specifies which
  of the three smoothing parameters should be allowed to vary and which
  should be fixed at a specific value (either the optimum or one chosen
  by the user). If the user requests that one parameter be varied, the
  function makes a call to the \code{plot} function; if two varied, it
  makes a call to the function specified in \code{lattice.plot}; if
  three are varied, it makes a call to \code{cloud}.

  \code{plot.diag} calls several functions in order to create a plot. To
  ensure maximum flexibility, the user can pass additional arguments to
  these functions through use of the \code{...} argument (for the
  plotting method) or with the \sQuote{args} lists. While
  \code{plot.diag} changes some of the defaults of these functions, the
  user can override these changes by specifying a value for that
  argument. For example, if the user fails to supply an argument to
  \code{main}, the function will create an informative title for the
  plot. If a value for \code{main} is supplied, that value will be
  used.

}

\value{None. Prints a plot either to the device window or to a
  \file{.pdf} file.}

\author{Jon Bischof \email{jbischof@fas.harvard.edu}}

\references{\url{http://gking.harvard.edu/yourcast}}

\seealso{\code{\link{robust.yourcast}}}
