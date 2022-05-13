#' @import methods
NULL

setGeneric("tests", function(object, ...) standardGeneric("tests"))

#' @name tests-methods
#' @docType methods
#' @aliases tests-methods tests,ages-method tests
#' @title Method \code{"tests"} for Class \code{"\linkS4class{ages}"}
#' 
#' @description Test if a set of ages belongs to a given number of populations or
#' alternatively find out the most probable number of populations within a range.
#' 
#' @section Usage:
#' \code{tests(object, nbmin=1, nbmax=NULL, level=object@@level, verbose=TRUE)}
#' @param object an object of type \code{\linkS4class{ages}} as produced by the function \code{\link{calculateAges}()}
#' @param nbmin a numeric. Minimum tested number of populations.
#' @param nbmax a numeric. Maximum tested number of populations.
#' @param level a numeric. Confidence interval level. Default is equal to \code{object@@level}, 
#'              i.e., to the level of confidence used to calculate the ages.
#' @param verbose logical. Activates the verbose mode.
#' 
#' @section Methods:
#' \itemize{
#'    \item \code{signature(object = "ages")}: an object of type
#'          \code{\linkS4class{ages}} as produced by the function
#'          \code{\link{calculateAges}()}.
#'    \item ...: other arguments passed to the method's function (see section Usage)
#' }
#' 
#' @details 
#' The function successively tests all values from \code{nbmin} to
#' \code{nbmax} and keeps the smallest accepted one (returns an error if all
#' supplied values are rejected). In this case, the output value is an object of
#' class \code{\linkS4class{ageTests}}.
#'
#' If \code{nbmax} is \code{NULL}, the results are kept even if the test is
#' rejected. In this case, the output value is an object of class
#' \code{\linkS4class{oneAgeTest}}.
#'
#' The performed tests are those described in Montel \emph{et al.} (1996).
#' 
#' @return An object of class \code{\linkS4class{ageTests}} or
#' \code{\linkS4class{oneAgeTest}} depending on the value of \code{nbmax} (see section "Details").
#' 
#' @references Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
#' microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.
#' 
#' @author Jean-Marc Montel, \email{jean-marc.montel@@ensg.inpl-nancy.fr}
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\link{calculateAges}()}, \code{\linkS4class{oneAgeTest}}, \code{\linkS4class{ageTests}}
#' 
#' @examples 
#' data(srilanka)
#' calculated.ages <- calculateAges(srilanka, nloops = 10)
#' tests(calculated.ages)
#' tests(calculated.ages, 1, 3)
#' 
#' @keywords methods
#' 
#' @exportMethod tests

setMethod("tests", "ages", function(object, nbmin=1, nbmax=NULL,
                                    level=object@level, verbose=TRUE) {
  
  # Input validity tests for object
  if (!inherits(object, "ages")) {
    stop("Invalid input for object. It must be an object of 'ages' class.")
  }
  # Input validity tests for nbmin
  if (!is.numeric(nbmin) | nbmin < 1) stop("Invalid input for nbmin. Must be a strictly positive numeric value.")
  
  # When testing for 1 population only
  if (is.null(nbmax)) {
    res.test <- testPop(object, nbmin, level)
  } 
  # When testing for multiple populations
  else {
    # Input validity test
    if (nbmax <= nbmin)
      stop("'nbmax' must be strictly larger than 'nbmin'.")
    # If passed successfully
    res.test <- testAges(object, nbmin, nbmax, level)
    # If inputs (nbmin,nbmax) tested higher than the true number of populations found
    if (sum(is.na(res.test@best.res@ages)) >= 1)
      warning("The population numbers tested are too high. The test's final result is lower than 'nbmin' input.")
  }
  
  if (verbose) print(res.test)
  invisible(res.test)
})