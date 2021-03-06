% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{facloadpointplot}
\alias{facloadpointplot}
\title{Displays point estimates of the factor loadings posterior.}
\usage{
facloadpointplot(
  x,
  fsvsimobj = NULL,
  statistic = "median",
  cex = 6.5,
  alpha = 0.2,
  allpairs = FALSE,
  col = NULL
)
}
\arguments{
\item{x}{Object of class \code{'fsvdraws'}, usually resulting from a call
to \code{\link{fsvsample}}.}

\item{fsvsimobj}{To indicate data generating values in case of simulated
data, pass an object of type \code{fsvsim} (usually the result of a
call to \code{\link{fsvsim}}).}

\item{statistic}{Character string indicating which posterior statistic
should be displayed.}

\item{cex}{Controls the size of the dots.}

\item{alpha}{Controls the level of transparency.}

\item{allpairs}{Logical value; if set to TRUE, all possible
pairwise combinations will be plotted.}

\item{col}{Vector of length \code{m} (number of component series),
containing \code{\link[grDevices]{rgb}}-type color codes used for
plotting. Will be recycled if necessary.}
}
\value{
Returns \code{x} invisibly, throws a warning if there aren't any
factors to plot.
}
\description{
\code{facloadpointplot} illustrates point estimates (mean, median, ...)
of the estimated factor loadings matrix.
}
\seealso{
Other plotting: 
\code{\link{comtimeplot}()},
\code{\link{corimageplot}()},
\code{\link{corplot}()},
\code{\link{cortimeplot}()},
\code{\link{evdiag}()},
\code{\link{facloadcredplot}()},
\code{\link{facloaddensplot}()},
\code{\link{facloadpairplot}()},
\code{\link{facloadtraceplot}()},
\code{\link{logvartimeplot}()},
\code{\link{paratraceplot}()},
\code{\link{plot.fsvdraws}()},
\code{\link{plotalot}()},
\code{\link{voltimeplot}()}
}
\concept{plotting}
