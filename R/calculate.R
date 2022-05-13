ageEquation <- function(x, an.obs) {
  # Constants initialization
  L232 <- 4.9334*10^(-5)
  L235 <- 9.8485*10^(-4)
  L238 <- 1.55125*10^(-4)
  est.pb <- (an.obs[2]/232.04*(exp(L232*x)-1)+
               an.obs[1]/238.03*0.9928*(exp(L238*x)-1)+
               an.obs[1]/238.03*0.0072*(exp(L235*x)-1))-an.obs[3]/207.2
  
  return(est.pb)
}

ageEqDerivative <- function(x, an.obs) {
  # Constants initialization
  L232 <- 4.9334*10^(-5)
  L235 <- 9.8485*10^(-4)
  L238 <- 1.55125*10^(-4)
  est.pb <- (an.obs[2]/232.04*L232*exp(L232*x)+
               an.obs[1]/238.03*0.9928*L232*exp(L238*x)+
               an.obs[1]/238.03*0.0072*L235*exp(L235*x))
  
  return(as.matrix(est.pb))
}

#' @import nleqslv
calculateOneAge <- function(an.obs) {
  age.init <- an.obs[3]/208/(an.obs[2]*2.1325E-07+an.obs[1]*6.7724E-07)
  solution <- try(nleqslv(age.init, function(x) ageEquation(x,an.obs),
                          function(x) ageEqDerivative(x,an.obs)),
                  silent = TRUE)
  if (inherits(solution, "try-error")) {
    solution <- NA
  } else solution <- round(solution$x)

  return(solution)
}

#' @import stats
calculateOneDistribution <- function(d, nloops) {
  random.data <- cbind(rnorm(nloops, d[1], d[2]/2),
                       rnorm(nloops, d[3], d[4]/2),
                       rnorm(nloops, d[5], d[6]/2))
  t.random <- apply(random.data,1,calculateOneAge)
  
  return(t.random)
}

#' @name calculateAges
#' @aliases calculateAges
#' @title Calculate the ages from electron microprobe measurements.
#' 
#' @usage calculateAges(measures, nloops = 1000, level = 0.05, verbose = TRUE, seed = NULL)
#'
#' @param measures a data.frame object with one electron microprobe measurement
#' by row and with columns U, corresponding error for U, Th, corresponding error
#' for Th, Pb, corresponding error for Pb, all expressed in ppm. See an example
#' with \command{data(srilanka)}.
#' 
#' @param nloops the number of Monte Carlo (MC) simulations used to estimate the
#' confidence intervals for the ages. Default is \code{1000}.
#' 
#' @param level the level of significance of the confidence intervals for the
#' ages. Default is \code{0.05}.
#' 
#' @param verbose logical; activates the verbose mode. Default is \code{TRUE}.
#' 
#' @param seed if supplied, initialize the random seed. Default is \code{NULL}
#' (the random seed is not initialized).
#' 
#' @return An object of class \code{\linkS4class{ages}}.
#' 
#' @description This function calculates the ages, confidence intervals and standard
#' deviations from triplets (U,Th,Ph), obtained by electron microprobe, given
#' together with corresponding errors.
#' 
#' @details The ages are calculated by solving the Equation (1) of Montel \emph{et al.}
#' (1996). The equation is solved by the Broyden method implemented in the
#' \code{\link[nleqslv:nleqslv]{nleqslv}()} function.
#' 
#' The standard deviations and the confidence intervals are calculated using a MC
#' approach: randomized observations of the triplets are generated from normal
#' distributions with standard deviations equal to
#' \eqn{\frac{\textrm{error}}{2}}{error/2} where 'error' denotes the error in the
#' measurement of Th, U or Pb, passed in \option{measures}. Standard deviations
#' are estimated by the empirical standard deviations and confidence intervals by
#' quantiles for probabilities \eqn{\frac{\textrm{\texttt{level}}}{2}}{level/2}
#' and \eqn{1-\frac{\textrm{\texttt{level}}}{2}}{1-level/2}, respectively.
#' 
#' @note You should use at least 1000 MC simulations otherwise the estimated confidence
#' intervals and standard deviations will not be reliable. Such simulations can
#' take a few seconds/minutes for fifty or so triplets and corresponding errors.
#' 
#' @references 
#' Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
#' microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.
#' 
#' @author Jean-Marc Montel, \email{jean-marc.montel@@ensg.inpl-nancy.fr}
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\linkS4class{ages}} \code{\link{tests}}
#' 
#' @examples 
#' ## Load the data
#' data(srilanka)
#' 
#' ## Calculate the ages
#' calculateAges(srilanka, nloops=10)
#' 
#' @keywords htest
#' 
#' @export calculateAges
#' 
#' @import stats

calculateAges <- function(measures, nloops=1000, level=0.05, verbose=TRUE,
                          seed=NULL) {
  
  # Input validity tests for measures
  if (!inherits(measures, "data.frame") | ncol(measures) != 6) {
    stop("The input measures' class/number of columns is not correct. It must be a 'data.frame' containing exactly 6 columns. 
         Check the dataset.")
  }
  # Input validity tests for nloops
  else if (!is.numeric(nloops) | nloops < 2) {
    stop("Invalid entry for the nloops parameter. It must be a numeric strictly larger than 1.")
  }
  # Input validity tests for level
  else if (!is.numeric(level) | level <= 0 | level >= 1) {
    stop("Invalid entry for the level parameter. It must be a numeric between 0 and 1.")
  }
  
  # If all validity tests are successfully passed
  else {
    d <- as.matrix(measures)
    # Calculate the ages
    if (verbose) cat("Age estimation...\n")
    t.init <- apply(d[,c(1,3,5)], 1, calculateOneAge)
    names(t.init) <- rownames(measures)
    
    # Estimate confidence interval by MC simulations
    if (verbose) cat("MC simulations...\n",
                     "(it might take a while if 'nloops' is large...)\n\n")
    if (!is.null(seed)) set.seed(seed)
    all.rand.t <- t(apply(d, 1, calculateOneDistribution, nloops=nloops))
    if (sum(is.na(all.rand.t)) > 0)
      warning(paste("Number of invalid estimations is", sum(is.na(all.rand.t))))
    ci.bounds <- apply(all.rand.t, 1, quantile, probs=c(level/2,1-level/2),
                       na.rm = TRUE)
    colnames(ci.bounds) <- rownames(measures)
    ages.sd <- apply(all.rand.t, 1, sd, na.rm = TRUE)
    names(ages.sd) <- rownames(measures)
    
    res <- new(Class = "ages", data = measures, ages = t.init, ci = ci.bounds, 
               sd = ages.sd, nloops = nloops, level = level)
    
    if (verbose) print(res)
    
    invisible(res)
  }
  
}
