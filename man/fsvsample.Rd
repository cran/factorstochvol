% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{fsvsample}
\alias{fsvsample}
\title{Markov Chain Monte Carlo (MCMC) Sampling for the Factor Stochastic
Volatility Model.}
\usage{
fsvsample(
  y,
  factors = 1,
  draws = 1000,
  thin = 1,
  burnin = 1000,
  restrict = "none",
  zeromean = TRUE,
  priorfacloadtype = "rowwiseng",
  priorfacload = 0.1,
  facloadtol = 1e-18,
  priorng = c(1, 1),
  priormu = c(0, 10),
  priorphiidi = c(10, 3),
  priorphifac = c(10, 3),
  priorsigmaidi = 1,
  priorsigmafac = 1,
  priorh0idi = "stationary",
  priorh0fac = "stationary",
  priorbeta = c(0, 10000),
  keeptime = "last",
  heteroskedastic = TRUE,
  priorhomoskedastic = NA,
  runningstore = 6,
  runningstorethin = 10,
  runningstoremoments = 2,
  signident = TRUE,
  signswitch = FALSE,
  interweaving = 4,
  quiet = FALSE,
  samplefac = TRUE,
  startfac,
  startpara,
  startlogvar,
  startlatent,
  startlogvar0,
  startlatent0,
  startfacload,
  startfacloadvar,
  expert
)
}
\arguments{
\item{y}{Data matrix. Each of \code{m} columns is assumed to contain a single
(univariate) series of length \code{n}.}

\item{factors}{Number of latent factors to be estimated.}

\item{draws}{Number of MCMC draws kept after burn-in.}

\item{thin}{Single number greater or equal to 1, coercible to integer.
Every \code{thin}th MCMC draw is kept and returned. The default value
is 1, corresponding to no thinning of the draws, i.e. every draw is
stored.}

\item{burnin}{Number of initial MCMC draws to be discarded.}

\item{restrict}{Either "upper", "none", or "auto", indicating whether
the factor loadings
matrix should be restricted to have zeros above the diagonal ("upper"),
whether all elements should be estimated from the data ("none"), or
whether the function \code{\link{findrestrict}} should be invoked for a
priori finding suitable zeros. Setting
\code{restrict} to "upper" or "auto" often stabilizes MCMC
estimation and can be important for identifying the factor loadings matrix,
however, it generally is a strong prior assumption. Setting
\code{restrict} to "none" is usually the preferred option if identification
of the factor loadings matrix is of less concern but covariance estimation
or prediction is the goal. Alternatively, \code{restrict} can be a
logical matrix
of dimension \code{c(m, r)} indicating which elements should be unrestricted
(where \code{restrict} is \code{FALSE}) or zero (where \code{restrict} is
\code{TRUE}).}

\item{zeromean}{Logical. If \code{FALSE}, a constant mean is included in
the model for each of the \code{m} univariate series.
If \code{TRUE}, the mean is not modeled. Defaults to \code{TRUE}.}

\item{priorfacloadtype}{Can be \code{"normal"}, \code{"rowwiseng"},
\code{"colwiseng"}.
\describe{
 \item{\code{"normal"}: }{Normal prior. The value of \code{priorfacload}
                          is interpreted as the standard deviations of the
                          Gaussian prior distributions for the factor loadings.}
 \item{\code{"rowwiseng"}: }{Row-wise Normal-Gamma prior. The value of \code{priorfacload}
                             is interpreted as the shrinkage parameter \code{a}.}
 \item{\code{"colwiseng"}: }{Column-wise Normal-Gamma prior. The value of \code{priorfacload}
                             is interpreted as the shrinkage parameter \code{a}.}
}
For details please see Kastner (2019).}

\item{priorfacload}{Either a matrix of dimensions \code{m} times \code{factors}
with positive elements or a single number (which will be recycled accordingly).
The meaning of \code{priorfacload} depends on the setting of \code{priorfacloadtype}
and is explained there.}

\item{facloadtol}{Minimum number that the absolute value of a factor loadings draw can take.
Prevents numerical issues that can appear when strong shrinkage is enforced if chosen
to be greater than zero.}

\item{priorng}{Two-element vector with positive entries indicating the Normal-Gamma
prior's hyperhyperparameters \code{c} and \code{d}.}

\item{priormu}{Vector of length 2 denoting prior mean and standard deviation
for unconditional levels of the idiosyncratic log variance processes.}

\item{priorphiidi}{Vector of length 2, indicating the shape parameters for the
Beta prior distributions of the transformed parameters \code{(phi+1)/2}, where
\code{phi} denotes the persistence of the idiosyncratic log variances.}

\item{priorphifac}{Vector of length 2, indicating the shape parameters for the
Beta prior distributions of the transformed parameters \code{(phi+1)/2}, where
\code{phi} denotes the persistence of the factor log variances.}

\item{priorsigmaidi}{Vector of length \code{m} containing the
prior volatilities of log variances. If \code{priorsigmaidi} has exactly
one element, it will be recycled for all idiosyncratic log variances.}

\item{priorsigmafac}{Vector of length \code{factors} containing the
prior volatilities of log variances. If \code{priorsigmafac} has exactly
one element, it will be recycled for all factor log variances.}

\item{priorh0idi}{Vector of length 1 or \code{m}, containing
information about the Gaussian prior for the initial idiosyncratic
log variances.
If an element of \code{priorh0idi} is a nonnegative number, the conditional
prior of the corresponding initial log variance h0 is assumed to be Gaussian
with mean 0 and standard deviation \code{priorh0idi} times $sigma$.
If an element of
\code{priorh0idi} is the string 'stationary', the prior of the corresponding
initial log volatility is taken to be from the stationary distribution, i.e.
h0 is assumed to be Gaussian with mean 0 and variance $sigma^2/(1-phi^2)$.}

\item{priorh0fac}{Vector of length 1 or \code{factors}, containing
information about the Gaussian prior for the initial factor
log variances.
If an element of \code{priorh0fac} is a nonnegative number, the conditional
prior of the corresponding initial log variance h0 is assumed to be Gaussian
with mean 0 and standard deviation \code{priorh0fac} times $sigma$.
If an element of
\code{priorh0fac} is the string 'stationary', the prior of the corresponding
initial log volatility is taken to be from the stationary distribution, i.e.
h0 is assumed to be Gaussian with mean 0 and variance $sigma^2/(1-phi^2)$.}

\item{priorbeta}{numeric vector of length 2, indicating the mean and
standard deviation of the Gaussian prior for the regression parameters. The
default value is \code{c(0, 10000)}, which constitutes a very vague prior
for many common datasets. Not used if \code{zeromean} is \code{TRUE}.}

\item{keeptime}{Either a number coercible to a positive integer, or a string
equal to "all" or "last". If a number different from 1 is provided, only every
\code{keeptime}th latent log-volatility is being monitored. If, e.g.,
\code{keeptime = 3}, draws for the latent log variances
\code{h_1,h_4,h_7,...} will
be kept. If \code{keeptime} is set to "all", this is equivalent to setting it
to 1. If
\code{keeptime} is set to "last" (the default), only draws for the very last
latent log variances h_n are kept.}

\item{heteroskedastic}{Vector of length 1, 2, or \code{m + factors},
containing logical values indicating whether time-varying
(\code{heteroskedastic = TRUE}) or constant (\code{heteroskedastic = FALSE})
variance should be estimated.
If \code{heteroskedastic} is of length 2 it will be recycled accordingly,
whereby the first element is used for all idiosyncratic variances and
the second element is used for all factor variances.}

\item{priorhomoskedastic}{Only used if at least one element of
\code{heteroskedastic} is set to \code{FALSE}. In that case,
\code{priorhomoskedastic} must be a matrix with positive entries
and dimension c(m, 2). Values in column 1 will be interpreted as the
shape and values in column 2 will be interpreted as the rate parameter
of the corresponding inverse gamma prior distribution of the idisyncratic
variances.}

\item{runningstore}{Because most machines these days do not have enough memory
to store all draws for all points in time, setting \code{runningstore} to an
integer greater than 0 will cause \code{fsvsample} to store the first
\code{runningstoremoments}
ergodic moments of certain variables of interest. More specifically, mean,
variance, skewness, etc. will be stored for certain variables
if \code{runningstore} is set to a value...
\describe{
 \item{\code{>= 1}: }{Latent log variances \code{h_1,h_2,...,h_(n+r)}.}
 \item{\code{>= 2}: }{Latent factors \code{f_1,...,f_r}.}
 \item{\code{>= 3}: }{Latent volatilities \code{sqrt(exp(h_1,h_2,...,h_(n+r)))}.}
 \item{\code{>= 4}: }{Conditional covariance matrix and the square roots of its
                     diagonal elements.}
 \item{\code{>= 5}: }{Conditional correlation matrix.}
 \item{\code{>= 6}: }{Communalities, i.e. proportions of variances explained
                     through the common factors.}
}}

\item{runningstorethin}{How often should the calculation of running moments be
conducted? Set to a value > 1 if you want to avoid time consuming calculations
at every MCMC iteration.}

\item{runningstoremoments}{Selects how many running moments (up to 4) should be
calculated.}

\item{signident}{If set to \code{FALSE}, no ex-post sign-identification is
performed. Defaults to \code{TRUE}.}

\item{signswitch}{Set to \code{TRUE} to turn on a random sign switch of
factors and loadings. Note that the signs of each factor loadings matrix column
and the corresponding factor cannot be identified from the likelihood.}

\item{interweaving}{The following values for interweaving the factor loadings
are accepted:
\describe{
 \item{0: }{No interweaving.}
 \item{1: }{Shallow interweaving through the diagonal entries.}
 \item{2: }{Deep interweaving through the diagonal entries.}
 \item{3: }{Shallow interweaving through the largest absolute entries in each column.}
 \item{4: }{Deep interweaving through the largest absolute entries in each column.}
}
For details please see Kastner et al. (2017).
A value of 4 is the highly recommended default.}

\item{quiet}{Logical value indicating whether the progress bar and other
informative output during sampling should be omitted. The default
value is \code{FALSE}, implying verbose output.}

\item{samplefac}{If set to \code{FALSE}, the factors are not sampled (but
remain at their starting values forever). This might be useful if one
wants to include observed factors instead of latent ones.}

\item{startfac}{\emph{optional} numeric matrix of dimension
\code{c(factors, n)}, containing the starting values of the
latent factors.
In case of a single factor model, a numeric vector of length \code{n} is also accepted.}

\item{startpara}{\emph{optional} numeric matrix of dimension
\code{c(3, m + factors)}, containing the starting values
for the parameter draws. The first \code{m} columns must contain
parameters values corresponding to the idiosyncratic volatilities,
the subsequent \code{factor} columns must contain parameter values
corresponding to the factor volatilities. The first row of \code{startpara}
corresponds to \code{mu}, the level of the log variances (can be arbitrary
numerical values), the second row corresponds to \code{phi}, the persistence
parameters of the log variances (numeric values between \code{-1} and \code{1}),
and the third row corresponds to \code{sigma} (positive numeric values).}

\item{startlogvar}{\emph{optional} numeric matrix of dimension
\code{c(n, m + factors)}, containing the starting values of the
latent log variances.
The first \code{m} rows correspond to the idiosyncratic log variances,
the subsequent \code{factor} rows correspond to the factor log variances.
Was previously called startlatent.}

\item{startlatent}{\emph{Deprecated.} Please use \code{startlogvar} instead.}

\item{startlogvar0}{\emph{optional} numeric vector of length
\code{m + factors}, containing the starting values of the initial latent
log variances.
The first \code{m} elements correspond to the idiosyncratic log variances,
the subsequent \code{factor} elements correspond to the factor log variances.
Was previously called startlatent0.}

\item{startlatent0}{\emph{Deprecated.} Please use \code{startlogvar0} instead.}

\item{startfacload}{\emph{optional} numeric matrix of dimension
\code{c(m, factors)}, containing the starting values of the
factor loadings.
In case of a single factor model, a numeric vector of length \code{n} is also accepted.}

\item{startfacloadvar}{\emph{optional} numeric matrix of dimension
\code{c(m, factors)}, containing the starting values of the
factor loadings variances \eqn{\tau_{ij}^2}. Used only when the normal-gamma
prior is employed (priorfacloadtype != "normal") while ignored when static
loadings variances are used (priorfacloadtype == "normal").}

\item{expert}{\emph{optional} named list of expert parameters for the
univariate SV models (will be transformed and passed to the \code{stochvol} package). For most
applications, the default values probably work best. Interested users are
referred to Kastner and Frühwirth-Schnatter (2014), the package vignette, and Kastner (2016). If
\code{expert} is provided, it may contain the following named elements:
\describe{
 \item{\code{parameterization}: }{Character string equal to \code{"centered"},
       \code{"noncentered"}, \code{"GIS_C"}, or \code{"GIS_NC"}. Defaults to
       \code{"GIS_C"}.}
 \item{\code{mhcontrol}: }{Single numeric value controlling the proposal
       density of a Metropolis-Hastings (MH) update step when sampling
       \code{sigma}. If \code{mhcontrol} is smaller than 0, an independence
       proposal will be used, while values greater than zero control the
       stepsize of a log-random-walk proposal. Defaults to \code{-1}.}
 \item{\code{gammaprior}: }{Single logical value indicating whether a Gamma
       prior for \code{sigma^2} should be used. If set to \code{FALSE},
       an Inverse Gamma prior is employed. Defaults to \code{TRUE}.}
 \item{\code{truncnormal}: }{Single logical value indicating whether a
       truncated Gaussian distribution should be used as proposal for
       draws of \code{phi}. If set to \code{FALSE}, a regular Gaussian
       prior is employed and the draw is immediately discarded when values
       outside the unit ball happen to be drawn. Defaults to \code{FALSE}.}
 \item{\code{mhsteps}: }{Either \code{1}, \code{2}, or \code{3}. Indicates
       the number of blocks used for drawing from the posterior of the
       parameters. Defaults to \code{2}.}
 \item{\code{proposalvar4sigmaphi}: }{Single positive number indicating the
       conditional prior variance of \code{sigma*phi} in the ridge
       \emph{proposal} density for sampling \code{(mu, phi)}.
       Defaults to \code{10^8}.}
 \item{\code{proposalvar4sigmatheta}: }{Single positive number indicating
       the conditional prior variance of \code{sigma*theta} in the ridge
       \emph{proposal} density for sampling \code{(mu, phi)}.
       Defaults to \code{10^12}.}
}}
}
\value{
The value returned is a list object of class \code{fsvdraws} holding
 \describe{
 \item{\code{facload}: }{Array containing draws from the posterior distribution of the
                factor loadings matrix.}
 \item{\code{fac}: }{Array containing factor draws from the posterior distribution.}
 \item{\code{logvar}: }{Array containing idiosyncratic and factor initial log variance draws.}
 \item{\code{logvar0}: }{Array containing idiosyncratic and factor log variance draws.}
 \item{\code{para}: }{Array containing parameter draws form the posterior distribution.}
 \item{\code{y}: }{Matrix containing the data supplied.}
 \item{\code{latestauxiliary}: }{List containing the latest draws of auxiliary quantities used for
                     sampling the factor loadings matrix.}
 \item{\code{runningstore}: }{List whose elements contain ergodic moments of certain
                     variables of interest. See argument
                     \code{runningstore} for details about what
                     is being stored here.}
 \item{\code{config}: }{List containing information on configuration parameters.}
 \item{\code{priors}: }{List containing prior hyperparameter values.}
 \item{\code{identifier}: }{Matrix containing the indices of the series used for
   ex-post sign-identification along with the corresponding minimum distances to zero.
   See \code{\link{signident}} for details.}
}
To display the output, use \code{print}, \code{plot}, and in particular specialized
extractors and printing functions.
The \code{print} method prints a high-level overview; specialized extractors such as
\code{\link{covmat}} or \code{\link{runningcovmat}} are also available.
The \code{plot} method invokes a simple covariance matrix plot; specialized plotting
functions are linked in the documentation of \code{\link{plot.fsvdraws}}.
}
\description{
\code{fsvsample} simulates from the joint posterior distribution and returns
the MCMC draws. It is the main workhorse to conduct inference for factor
stochastic volatility models in this package.
}
\details{
For details concerning the factor SV algorithm please see
Kastner et al. (2017), details about the univariate SV estimation
can be found in Kastner and Frühwirth-Schnatter (2014).
}
\examples{
\donttest{
# Load exchange rate data (ships with stochvol):
data(exrates, package = "stochvol")
exrates$date <- NULL

# Compute the percentage log returns:
dat <- 100 * logret(exrates)

# We are going to fit a one-factor model so the ordering is irrelevant
# NOTE that these are very few draws, you probably want more...
res <- fsvsample(dat, factors = 2, draws = 2000, burnin = 1000,
  runningstore = 6, zeromean = FALSE)

voltimeplot(res)

corimageplot(res, nrow(dat), plotCI = 'circle')

oldpar <- par(ask = TRUE)
plot(res)
par(oldpar)
pairs(t(res$beta[1:4, ]))
}

}
\references{
Kastner, G., Frühwirth-Schnatter, S., and Lopes, H.F. (2017).
Efficient Bayesian Inference for Multivariate Factor Stochastic Volatility Models.
\emph{Journal of Computational and Graphical Statistics}, \bold{26}(4), 905--917,
\doi{10.1080/10618600.2017.1322091}.

Kastner, G. (2019).
Sparse Bayesian Time-Varying Covariance Estimation in Many Dimensions
\emph{Journal of Econometrics}, \bold{210}(1), 98--115,
\doi{10.1016/j.jeconom.2018.11.007}

Kastner, G. (2016).
Dealing with stochastic volatility in time series using the R package
stochvol.
\emph{Journal of Statistical Software}, \bold{69}(5), 1--30,
\doi{10.18637/jss.v069.i05}.

Kastner, G. and Frühwirth-Schnatter, S. (2014).
Ancillarity-Sufficiency Interweaving Strategy (ASIS) for Boosting MCMC
Estimation of Stochastic Volatility Models.
\emph{Computational Statistics & Data Analysis}, \bold{76}, 408--423,
\doi{10.1016/j.csda.2013.01.002}.
}
\concept{wrappers}
