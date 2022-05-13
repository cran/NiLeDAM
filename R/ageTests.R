#' @import methods
NULL

#' @name ageTests-class
#' @docType class
#' @aliases ageTests-class ageTests print,ageTests-method print.ageTests show,ageTests-method show.ageTests summary,ageTests-method summary.ageTests
#' @title Class \code{"ageTests"}
#' 
#' @description Results of \eqn{\chi^2}{chi2}-tests testing to which number of populations the
#' ages calculated by the function \code{\link{calculateAges}()} are the most likely
#' to come from. The test is the one described in the article Montel \emph{et al.}
#' (1996).
#' 
#' @section Objects from the Class:
#' Objects from this class are created by the function \code{\link{tests}()} applied
#' to an object of class \code{\link{ages}} when the option \option{nbmax} is not
#' set to \code{NULL} but to an integer larger than \option{nbmin}.
#' 
#' @field nb.pop Object of class \code{"vector"} that contains the
#'        tested numbers of populations.
#' @field best.nb Object of class \code{"numeric"} that contains the
#'        most probable number of populations, which is the smallest number for which the
#'        \eqn{\chi^2}{chi2}-test is accepted.
#' @field best.res Object of class \code{"\linkS4class{oneAgeTest}"}
#'        that contains the result of the test for \code{best.nb} populations.
#' 
#' @section Methods:
#' \itemize{
#'    \item print(\code{signature(x = "ageTests")}): displays a summary of the test's results.
#'    \item show(\code{signature(object = "ageTests")}): displays a summary of the test's results.
#'    \item summary(\code{signature(object = "ageTests")}): displays a summary of the test's results.
#' }
#'
#' @references Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
#' microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.
#' 
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\linkS4class{ages}}, \code{\link{tests}()}, \code{\link{plot.ageTests}}
#' 
#' @examples 
#' showClass("ageTests")
#' 
#' ## Example
#' data(srilanka)
#' calculated.ages <- calculateAges(srilanka, nloops = 10)
#' tests(calculated.ages, nbmax = 3)
#' 
#' @keywords classes
#' 
#' @exportClass ageTests
setClass("ageTests", representation(nb.pop="vector",
                                    best.nb="numeric",
                                    best.res="oneAgeTest"))

setMethod("print", "ageTests", function(x, ...) {
  if (sum(is.na(x@best.res@ages)) >= 1) {
    cat("Test if the",length(x@best.res@data@ages),
        "estimated ages are coming from\n",x@nb.pop,"\npopulation(s).\n\n")
    cat("    No valid number of populations in the sequence tested. (",
        min(x@nb.pop)," to ",max(x@nb.pop),")\n\n")
    cat("    The ages are not found to be likely coming from",x@best.nb,
        "population(s) at level",x@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",x@best.res@S," ~ df:",x@best.res@df,"\n\n")
    cat("    Estimated ages:", x@best.res@ages, "\n\n")
    cat("    Population numbers:\n", x@best.res@which.pop, "\n\n")
  }
  else {
    cat("Test if the",length(x@best.res@data@ages),
        "estimated ages are coming from\n",x@nb.pop,"\npopulation(s).\n\n")
    cat("    The ages are found to be likely coming from",x@best.nb,
        "population(s) at level",x@best.res@level*100,"%.\n",
        "   Chi2 test statistic:",x@best.res@S," ~ df:",x@best.res@df,"\n\n")
    cat("    Estimated ages:", x@best.res@ages, "\n\n")
    cat("    Population numbers:\n", x@best.res@which.pop)
  }
})
#' @exportMethod print

setMethod("show", "ageTests", function(object) {
  print(object)
})
#' @exportMethod show

setMethod("summary", "ageTests", function(object, ...) {
  print(object)
})
#' @exportMethod summary

setMethod("plot", "ageTests", function(x, y = NULL, ...) {
  
  if (!inherits(x, "ageTests")) stop("Invalid input, must be of class 'ageTests'")
  
  args <- list(...)
  args$x <- x@best.res
  do.call("plot",args)
  
})
#' @exportMethod plot

setMethod("popline", "ageTests", function(x, main2 = "", ...) {
  
  if (!inherits(x, "ageTests")) stop("Invalid input, must be of class 'ageTests'")
  
  args <- list(...)
  args$x <- x@best.res
  do.call("popline",args)
  
})
#' @exportMethod popline