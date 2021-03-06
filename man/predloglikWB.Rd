% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_fsvdraws.R
\name{predloglikWB}
\alias{predloglikWB}
\title{Evaluates the predictive log likelihood using the Woodbury identity}
\usage{
predloglikWB(x, y, ahead = 1, each = 1, alldraws = FALSE)
}
\arguments{
\item{x}{Object of class \code{'fsvdraws'}, usually resulting from a call
to \code{\link{fsvsample}}.}

\item{y}{Matrix of dimension \code{length(ahead)} times \code{m} where the
predictive density should be evaluated.}

\item{ahead}{Vector of timepoints, indicating how many steps
to predict ahead.}

\item{each}{Single integer (or coercible to such) indicating how
often should be drawn from the posterior predictive distribution
for each draw that has been stored during MCMC sampling.}

\item{alldraws}{Should all the draws be returned or just the final results?
(Can be useful to assess convergence.)}
}
\value{
Vector of length \code{length(ahead)} with log predictive
likelihoods.
}
\description{
\code{predloglikWB} approximates the predictive log likelihood exploiting
the factor structure and using the Woodbury idenitity and the
corresponding matrix determinant lemma. This is recommended only
if many series and few factors are present.
}
\note{
Currently crudely implemented as a triple loop in pure R,
may be slow.
}
\examples{
\donttest{
set.seed(1)

# Simulate a time series of length 1100:
sim <- fsvsim(n = 1100, series = 3, factors = 1)
y <- sim$y

# Estimate using only 1000 days:
res <- fsvsample(y[seq_len(1000),], factors = 1)

# Evaluate the 1, 10, and 100 days ahead predictive log
# likelihood:
ahead <- c(1, 10, 100)
scores <- predloglikWB(res, y[1000+ahead,], ahead = ahead, each = 10)
print(scores)
}

}
\seealso{
Uses \code{\link{predprecWB}}. If \code{m} is small
or many factors are used, consider also using
\code{\link{predcov}}.

Other predictors: 
\code{\link{predcond}()},
\code{\link{predcor}()},
\code{\link{predcov}()},
\code{\link{predh}()},
\code{\link{predloglik}()},
\code{\link{predprecWB}()}
}
\concept{predictors}
