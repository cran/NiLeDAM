#' @import methods
NULL

#' @name oneAgeTest-class
#' @docType class
#' @aliases oneAgeTest-class oneAgeTest print,oneAgeTest-method print.oneAgeTest show,oneAgeTest-method show.oneAgeTest summary,oneAgeTest-method summary.oneAgeTest
#' @title Class \code{"oneAgeTest"}
#' 
#' @description Results of a \eqn{\chi^2}{chi2}-test testing if the ages calculated by the 
#' function \code{\link{calculateAges}()} come from a given number of populations.
#' The test is the one described in the article Montel \emph{et al.} (1996).
#' 
#' @section Objects from the Class:
#' Objects from this class are created by the function \code{\link{tests}()} applied
#' to an object of class \code{\link{ages}} when the option \option{nbmax} is set
#' to \code{NULL}.
#' 
#' @field data Object of class \code{"\link{ages}"} as created by the
#'        function \code{\link{calculateAges}()}. This is the object passed as an argument
#'        to the function \code{\link{tests}()}.
#' @field ages Object of class \code{"vector"} that contains the
#'        estimated common age(s) of the population(s). Its size is equal to the argument
#'        \option{nbmin} passed to the function \code{\link{tests}()}.
#' @field sd Object of class \code{"vector"} that contains the
#'        estimated standard deviations of the common age(s) of the population(s). Its
#'        size is equal to the argument \option{nbmin} passed to the function
#'        \code{\link{tests}()}.
#' @field ic Object of class \code{"matrix"} having \option{nbmin} rows
#'        and two columns corresponding to the confidence intervals at level
#'        \option{level} as passed as an argument to the function \code{\link{tests}()}.
#' @field S Object of class \code{"numeric"} \eqn{\chi^2}{chi2} test statistic of the test.
#' @field thres Object of class \code{"numeric"} probability threshold of the test.
#' @field df Object of class \code{"numeric"} degree of freedom of the test.
#' @field level Object of class \code{"numeric"} confidence level of
#'        the test. Passed as an argument to the function \code{\link{tests}()}.
#' @field h0 Object of class \code{"logical"} result of the test: if
#'        \code{h0} is \code{TRUE}, the test is accepted at level \option{level} and if
#'        \code{h0} is \code{FALSE}, the test is rejected at level \option{level}.
#' @field which.pop Object of class \code{"vector"} that contains the
#'        population number for each analytic point. \code{1} corresponds to the
#'        population with the first age in \code{ages}, \code{2} to the population with
#'        the second age in \code{ages}...
#' 
#' @section Methods:
#' \itemize{
#'    \item print(\code{signature(x = "oneAgeTest")}): displays a summary of the test's result.
#'    \item show(\code{signature(object = "oneAgeTest")}): displays a summary of the test's result.
#'    \item summary(\code{signature(object = "oneAgeTest")}): displays a summary of the test' results.
#' }
#' 
#' @references Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
#' microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.
#' 
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\linkS4class{ages}}, \code{\link{tests}()}, \code{\link{plot.oneAgeTest}}
#' 
#' @examples 
#' showClass("oneAgeTest")
#' 
#' ## Example
#' 
#' data(srilanka)
#' calculated.ages <- calculateAges(srilanka, nloops=10)
#' tests(calculated.ages)
#' 
#' @keywords classes
#' 
#' @exportClass oneAgeTest
setClass("oneAgeTest", representation(data="ages",
                                      ages="vector",
                                      sd="vector",
                                      ic="matrix",
                                      S="numeric",
                                      thres="numeric",
                                      df="numeric",
                                      level="numeric",
                                      h0="logical",
                                      which.pop="vector"))

setMethod("print", "oneAgeTest", function(x, ...) {
  cat("Test if the",length(x@which.pop),"estimated ages are coming from",
      length(x@ages),"population(s).\n\n")
  if (x@h0) {
    cat("    The ages are not found to be unlikely coming from",length(x@ages),
        "population(s) at level",x@level*100,"%.\n",
        "   Chi2 test statistic:",x@S," ~ df:",x@df,"\n\n")
    cat("    Estimated ages:", x@ages, "\n\n")
    cat("    Population numbers:\n", x@which.pop)
  } else {
    cat("    The ages are found to be unlikely coming from",length(x@ages),
        "population(s) at level",x@level*100,"%.\n",
        "   Chi2 test statistic:",x@S," ~ df:",x@df,"\n\n")
  }
})
#' @exportMethod print

setMethod("show", "oneAgeTest", function(object) {
  print(object)
})
#' @exportMethod show

setMethod("summary", "oneAgeTest", function(object, ...) {
  print(object)
})
#' @exportMethod summary

#' @name plot-methods
#' @docType methods
#' @aliases plot-methods plot,oneAgeTest-method plot.oneAgeTest plot,ageTests-method plot.ageTests
#' @title Method \code{"plot"} for Classes \code{"\linkS4class{oneAgeTest}"} and \code{"\linkS4class{ageTests}"}
#' 
#' @description Display densities of the ages calculated by the function
#' \code{\link{calculateAges}()}, as well as the common age(s) density(ies) as 
#' returned by the function \code{\link{tests}()}.
#' 
#' @section Usage: plot(x, ...)
#' @param x an object of class "oneAgeTest" or "ageTests" (see section Methods)
#' @param y NUll
#' @param main a character string, title of the plot
#' @param col a character string, color of the common age(s) density(ies)
#' 
#' @section Methods:
#' \itemize{
#'    \item \code{signature(object = "oneAgeTest")}: an object of type
#'          \code{\linkS4class{oneAgeTest}} as produced by the function
#'          \code{\link{tests}()}.
#'    \item \code{...}: further arguments passed to the function \code{\link{plot}()}.
#' }
#' 
#' \itemize{
#'    \item \code{signature(object = "ageTests")}: an object of type
#'          \code{\linkS4class{ageTests}} as produced by the function
#'          \code{\link{tests}()}.
#'    \item \code{...}: further arguments passed to the function \code{\link{plot}()}.
#' }
#' 
#' @details The estimated densities are Gaussian, as supposed by the model 
#' described in Montel \emph{et al.} (1996). Each density is referenced by its 
#' number to help identify outliers or invalid measurements.
#' 
#' Also note that if an object of class \code{\linkS4class{oneAgeTest}} is passed
#' to the function, the common age(s) densities is displayed even if the test of
#' the function \code{\link{tests}()} is rejected.
#' 
#' If argument \option{col} is supplied, it is used to display 
#' the density(ies) of the common age(s).
#' 
#' @references Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
#' microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.
#' 
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\link{tests}()}, \code{\linkS4class{oneAgeTest}}, \code{\linkS4class{ageTests}}
#' 
#' @examples
#' data(srilanka)
#' calculated.ages <- calculateAges(srilanka, nloops = 10)
#' res.tests <- tests(calculated.ages, 1, 3)
#' plot(res.tests, main = "Densities", col = "lightseagreen")
#' 
#' @keywords methods
#'
#' @importFrom tidyr pivot_longer everything 
#' @import stats
#' @import ggplot2
#' 
#' @exportMethod plot
setMethod("plot", "oneAgeTest", function(x, y=NULL, main = "", col = "red") {
  
  if (!inherits(x, "oneAgeTest")) stop("Invalid input, must be of class 'oneAgeTest'")
  
  # densities for the estimated ages
  dens.x <- seq(min(x@data@ages)-5*x@data@sd[which.min(x@data@ages)],
                max(x@data@ages)+5*x@data@sd[which.max(x@data@ages)],
                1)
  
  dens.y <- pivot_longer(
    as.data.frame(
      apply(cbind(x@data@ages,x@data@sd),1,
            function(one.obs) dnorm(dens.x,mean=one.obs[1],sd=one.obs[2]))
    ), everything(), names_to = "Ind", values_to = "Dens")
  dens.y <- dens.y[order(dens.y$Ind),]
  
  y <- apply(cbind(x@ages, x@sd), 1,
             function(one.obs) dnorm(dens.x, mean=one.obs[1], sd=one.obs[2]))
  
  dens.label <- sapply(1:length(x@data@ages), function(obs.nb) {
    one.obs <- x@data@ages[obs.nb]
    dnorm(one.obs,one.obs,x@data@sd[obs.nb])*1.2
  })
  
  # densities for the common age estimates
  dens.label2.x <- sapply(1:length(x@ages), function(nb){
    x@ages[nb]*1.15
  })
  dens.label2.y <- sapply(1:length(x@ages), function(nb){
    max(y[,nb])*0.8
  })
  dens.label2 <- sapply(1:length(x@ages), function(nb){
    paste(x@ages[nb]," +/- ",round(x@sd[nb]*qnorm(1-x@level/2),0),sep="")
  })
  
  y <- pivot_longer(as.data.frame(y), everything(), names_to = "Ind", values_to = "Dens")
  y <- y[order(y$Ind),]
  
  # display all on a ggplot
  ggplot() +
    xlim(range(dens.x)) +
    ylim(c(0,max(y$Dens))) +
    geom_line(aes(x = rep(dens.x,length(unique(dens.y$Ind))), y = dens.y$Dens, group = dens.y$Ind), color = "black") +
    geom_text(aes(x = x@data@ages, y = dens.label, label = names(x@data@ages))) +
    geom_line(aes(x = rep(dens.x,length(unique(y$Ind))), y = y$Dens, group = y$Ind), linetype = "dashed", size = 1, color = col) +
    geom_text(aes(x = dens.label2.x, y = dens.label2.y, label = dens.label2), color = col, size = 5) +
    xlab("Ma") +
    ylab("") +
    ggtitle(main) +
    theme_bw()
  
})

setGeneric("popline", function(x, ...) standardGeneric("popline"))

#' @name popline-methods
#' @docType methods
#' @aliases popline popline-methods popline,oneAgeTest-method popline.oneAgeTest popline,ageTests-method popline.ageTests
#' @title Method \code{"popline"} for Classes \code{"\linkS4class{oneAgeTest}"} and \code{"\linkS4class{ageTests}"}
#' 
#' @description Display observations as well as their reference's population as calculated 
#' by the function \code{\link{tests}()} given their value of Pb (ppm) 
#' and Th*(ppm), where Th* is a function of Th and U variables.
#' 
#' @section Usage: popline(x, main2)
#' @param x an object of class "oneAgeTest" or "ageTests" (see section Methods)
#' @param main2 a character string, title of the plot
#' 
#' @section Methods:
#' \itemize{
#'    \item \code{signature(object = "oneAgeTest")}: an object of type
#'          \code{\linkS4class{oneAgeTest}} as produced by the function
#'          \code{\link{tests}()}.
#'    \item \code{main2}: a character string to entitle the plot
#' }
#' 
#' \itemize{
#'    \item \code{signature(object = "ageTests")}: an object of type
#'          \code{\linkS4class{ageTests}} as produced by the function
#'          \code{\link{tests}()}.
#'    \item \code{main2}: a character string to entitle the plot
#' }
#' 
#' @details 
#' Th* is computed thanks to the following formula: 
#' \eqn{Th + 232*U * [238(exp(4.9475*10^{-5}*Age) - 1)]^{-1} * [[exp(9.8485*10^{-4}*Age + 138*exp(1.55125*10^{-4}*Age))][139] - 1]}
#' 
#' Slopes, depending directly of the ages computed with the function \code{\link{tests}()}, 
#' are obtained through the following equation: \eqn{[exp(4.9475*10^{-5}*Age) - 1] * 207.3/232}.
#' 
#' @references Montel J.M., Foret S., Veschambre M., Nicollet C., Provost A. (1996) Electron
#' microprobe dating of monazite. \emph{Chemical Geology}, \bold{131}, 37--53.
#' 
#' @author Aurélie Mercadié, \email{aurelie.mercadie@@inrae.fr}
#' @author Jean-Marc Montel, \email{jean-marc.montel@@ensg.inpl-nancy.fr}
#' @author Nathalie Vialaneix, \email{nathalie.vialaneix@@inrae.fr}
#' 
#' @seealso \code{\link{tests}()}, \code{\linkS4class{oneAgeTest}}, \code{\linkS4class{ageTests}}
#' 
#' @examples 
#' data(srilanka)
#' calculated.ages <- calculateAges(srilanka, nloops = 10)
#' res.tests <- tests(calculated.ages, 1, 3)
#' popline(res.tests, main2 = "Populations")
#' 
#' @keywords methods
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom dplyr group_by mutate select distinct
#' @import ggplot2
#' @importFrom scales hue_pal
#' 
#' @exportMethod popline
setMethod("popline", "oneAgeTest", function(x, main2 = "") {
  
  if (!inherits(x, "oneAgeTest")) stop("Invalid input, must be of class 'oneAgeTest'")
  
  # compute TH*, depending on Th and U
  dataset <- x@data@data
  dataset$`Th*` <- dataset$Th + dataset$U * 232 / (238 * (exp(4.9475E-05*x@data@ages)-1)) * ((exp(9.8485E-04*x@data@ages)+138*exp(1.55125E-04*x@data@ages))/139-1)
  
  # compute lines' slopes (as a function of population ages estimated)
  # added condition to exclude NA from common ages vector
  slopes <- (exp(4.9475E-05*x@ages[!is.na(x@ages)])-1) * (207.3/232)
  
  # create labels (one for each population identified) + their future location on the graph
  # added condition to exclude NA from common ages vector
  labels <- as.character(paste("Population",order(unique(x@which.pop)),paste("(",x@ages[!is.na(x@ages)],"Ma)",sep=""),sep=" "))
  labels.loc <- data.frame(x@which.pop)
  names(labels.loc) <- "Pop"
  labels.loc <- cbind(labels.loc, dataset) %>%
    group_by(.data$Pop) %>%
    mutate(`Max.Th*` = max(.data$`Th*`), Max.Pb = max(.data$Pb)) %>%
    select(.data$Pop, .data$`Max.Th*`, .data$Max.Pb) %>%
    distinct()
  
  # display all on a ggplot
  ggplot() +
    geom_point(aes(x = dataset$`Th*`, y = dataset$Pb, color = as.factor(x@which.pop)), size = 3) +
    geom_abline(slope = slopes, intercept = rep(0,length(unique(x@which.pop))), 
                color = hue_pal()(length(unique(x@which.pop))), 
                linetype = rep("dashed",length(unique(x@which.pop)))) +
    scale_x_continuous(expand = c(0,0), limits = c(0,max(dataset$`Th*`)+0.15*max(dataset$`Th*`))) +
    scale_y_continuous(expand = c(0,0), limits = c(0,max(dataset$Pb)+0.15*max(dataset$Pb))) +
    geom_text(aes(x = labels.loc$`Max.Th*`, y = labels.loc$Max.Pb), label = labels, vjust = -2) +
    labs(col = "Population") +
    xlab("Th* (ppm)") +
    ylab("Pb (ppm)") +
    ggtitle(main2) +
    theme_bw()
  
})