#' @name liveNiLeDAM
#' @aliases liveNiLeDAM
#' 
#' @title Graphical Web User Interface for 'NiLeDAM'
#' @description This function starts the graphical user interface with the default 
#' system browser. This interface is more likely to work properly with Firefox
#' \url{https://www.mozilla.org/fr/firefox/new/}. In case Firefox is not your 
#' default browser, copy/paste http://localhost:8100 into the address bar.
#' 
#' @usage liveNiLeDAM()
#' 
#' @return Starts the 'NiLeDAM' GUI.
#' 
#' @author Aurélie Mercadié, \email{aurelie.mercadie@@inrae.fr}
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @export liveNiLeDAM
#' 
#' @importFrom shiny runApp
#' @import shinythemes
#' @importFrom shinyjs reset useShinyjs
#' @importFrom thematic thematic_shiny

liveNiLeDAM <- function() {
  runApp(system.file('shiny', package='NiLeDAM'))
}