#' @import methods
NULL

#' @name ages-class
#' @docType class
#' @aliases ages-class ages print,ages-method print.ages show,ages-method show.ages summary,ages-method summary.ages
#' @title Class \code{"ages"}
#' 
#' @description A class for the results of the \code{\link{calculateAges}} function. It contains
#' the ages calculated for each analytic point from triplets (U,Th,Pb) and
#' corresponding errors.
#' 
#' @section Objects from the Class:
#' Objects can be created by calls of the function \code{\link{calculateAges}()}.
#' 
#' @field data Object of class \code{"data.frame"} that contains the
#'        original triplets (U,Th,Pb) and corresponding errors.
#' @field ages Object of class \code{"vector"} that contains the
#'        ages calculated for each analytic point.
#' @field ci Object of class \code{"matrix"}, which is a two-column
#'        matrix that contains the bounds of the confidence intervals for each age of the
#'        slot \code{ages}. These are estimated from a MC simulation method as described
#'        in \code{\link{calculateAges}()}.
#' @field sd Object of class \code{"vector"} that contains the standard
#'        deviations for each age of the slot \code{ages}. These are estimated from a MC
#'        simulation method as described in \code{\link{calculateAges}()}.
#' @field nloops Object of class \code{"numeric"} that stores the
#'        number of MC simulations used to estimate the confidence intervals and the
#'        standard deviations.
#' @field level Object of class \code{"numeric"} that stores the level
#'        of the confidence intervals.
#' 
#' @section Methods:
#' \itemize{
#'    \item print(\code{signature(x = "ages")}): display a summary of the results
#'          of the ages calculation.
#'    \item show(\code{signature(object = "ages")}): display a summary of the
#'          results of the ages calculation (same as \code{print.ages}).
#'    \item summary(\code{signature(object = "ages")}): display a summary of the
#'          results of the ages calculation (same as \code{print.ages}).
#' }
#' 
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\link{calculateAges}()}, \code{\link{tests}()}
#' 
#' @examples 
#' showClass("ages")
#' 
#' ## Example on the srilanka dataset
#' 
#' data(srilanka)
#' calculated.ages <- calculateAges(srilanka, nloops = 10)
#' calculated.ages
#' 
#' @keywords classes
#' 
#' @exportClass ages
setClass("ages", representation(data="data.frame",
                                ages="vector",
                                ci="matrix",
                                sd="vector",
                                nloops="numeric",
                                level="numeric"))

setMethod("print","ages", function(x, ...) {
  cat(nrow(x@data),"ages and confidence intervals estimated from",
      x@nloops,"bootstrap samples.\n","Summary:\n")
  print(summary(x@ages))
})
#' @exportMethod print

setMethod("show", "ages", function(object) {
  cat(nrow(object@data),"ages and confidence intervals estimated from",
      object@nloops,"bootstrap samples.\n","Summary:\n")
  print(summary(object@ages))
})
#' @exportMethod show

setMethod("summary", "ages", function(object, ...) {
  cat(nrow(object@data),"ages and confidence intervals estimated from",
      object@nloops,"bootstrap samples.\n","Summary:\n")
  print(summary(object@ages))
})
#' @exportMethod summary