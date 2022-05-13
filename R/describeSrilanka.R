#' @name srilanka
#' @aliases srilanka
#' @docType data
#' @title An example data set: electron microprobe data.
#' 
#' @description This dataset is kindly provided by Anne-Magali Seydoux-Guillaume
#' \email{anne.magali.seydoux@univ-st-etienne.fr} and has been published in 
#' Seydoux-Guillaume \emph{et al.} (2012).
#' 
#' @usage data(srilanka)
#' 
#' @format 
#' A data frame with 32 observations on the following 6 variables:
#' \itemize{
#'    \item \strong{U}: U concentration (ppm)
#'    \item \strong{errU}: error on the measurement of U concentration
#'    \item \strong{Th}: Th concentration (ppm)
#'    \item \strong{ErrTh}: error on the measurement of Th concentration
#'    \item \strong{Pb}: Pb concentration (ppm)
#'    \item \strong{ErrPb}: error on the measurement of Pb concentration
#' }
#' 
#' @details The first 8 observations are group control data (more precisely, they correspond
#' to standard reference analyses). When testing if all the observations are issued
#' from a single population, the assumption is thus rejected. Removing the first 8
#' observations leads to obtain a positive answer when testing if the observations
#' come from the same population. See \code{demo(srilanka)}.
#' 
#' @references Seydoux-Guillaume A.M., Montel J.M., Bingen B., Bosse V., de Parseval P.,
#' Paquette J.L., Janots E., Wirth R. (2012) Low-temperature alteration of
#' monazite: fluid mediated coupled dissolution-precipitation, irradiation damage
#' and disturbance of the U-Pb and Th-Pb chronometers. \emph{Chemical Geology}, 
#' \bold{330--331}, 140--158.
#' 
#' @examples 
#' data(srilanka)
#'
#' # With control group data
#' summary(srilanka)
#'
#' # Without control group data
#' summary(srilanka[9:32,])
#' 
#' @keywords datasets
NULL