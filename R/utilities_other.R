#' @describeIn logret (De-meaned) log returns
#' @export

logret.matrix <- function(dat, demean = FALSE, ...) {
 tmp <- dat[,colSums(is.na(dat)) <= 0.5]
 tmp <- diff(log(as.matrix(tmp)))
 if (all(isTRUE(demean))) tmp <- tmp - rep(colMeans(tmp), each = nrow(tmp))
 tmp
}


#' @describeIn logret (De-meaned) log returns
#' @export

logret.data.frame <- function(dat, demean = FALSE, ...) {
 dat <- data.matrix(dat)
 logret(dat, ...)
}


#' Ledermann bound for the number of factors
#' 
#' In the static factor case, the Ledermann bound is the largest
#' integer rank for which a unique decomposition of the covariance
#' matrix is possible. (This is the largest possible number of
#' factors which can be used for \code{\link[stats]{factanal}}.
#' 
#' @param m Number of component series.
#'
#' @return The Ledermann bound, a nonnegative integer.
#'
#' @seealso preorder
#'
#' @export

ledermann <- function(m) {
 as.integer(floor((2*m+1)/2 - sqrt((2*m+1)^2/4 - m^2 + m)))
}


#' Ad-hoc methods for determining the order of variables
#'
#' In factor SV models, the ordering of variables is often
#' chosen through a preliminary static factor analysis. These
#' methods are implemented in \code{preorder}.
#' After a maximum likelihood factor model fit to the data,
#' factor loadings are ordered as follows: The variable with the
#' highest loading on factor 1 is placed first, the variable with
#' the highest loading on factor 2 second (unless this variable
#' is already placed first, in which case the variable with the
#' second highest loading is taken).
#' 
#' @param dat Matrix containing the data, with \code{n} rows
#' (points in time) and \code{m} columns (component series).
#' @param factors Number of factors to be used, defaults to the
#' Ledermann bound.
#' @param type Can be "fixed" or "dynamic". The option "fixed"
#' means that that a \code{factors}-factor model is fit once and
#' the entire ordering is determined according to this fit
#' (the default). The option "dynamic" means that 
#' the model is re-fit \code{factors} times with the number of
#' factors going from 1 to
#' \code{factors} and in each round the correspondingly largest
#' loading is chosen.
#' @param transload Function for transforming the estimated
#' factor loadings before ordering. Defaults to the identity
#' function.
#'
#' @return A vector of length \code{m} with the ordering found.
#'
#' @seealso ledermann 
#'
#' @export

preorder <- function(dat, factors = ledermann(ncol(dat)), type = "fixed", transload = identity) {
 m <- ncol(dat)
 control <- list(opt = list(maxit = 100000)) 
 ordering <- rep(NA_integer_, m)
 
 if (type == "fixed") {
  fa <- factanal(dat, factors, control = control)
  for (i in 1:factors) {
   tmp <- order(transload(fa$loadings[,i]), decreasing = TRUE)
   ordering[i] <- tmp[!(tmp %in% ordering)][1]
  }
 } else if (type == "dynamic") {
  for (i in 1:factors) {
   fa <- factanal(dat, i, control = control)
   tmp <- order(transload(fa$loadings[,i]), decreasing = TRUE)
   ordering[i] <- tmp[!(tmp %in% ordering)][1]
  }
 } else stop("Unknown type")
 ordering[(factors + 1) : m] <- (1:m)[!((1:m) %in% ordering)]
 ordering
}

