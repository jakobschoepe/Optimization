#' @title Extracting the estimated model parameters of \code{bsw}
#' @description For objects of class \code{"bsw"}, \code{coef} extracts the estimated model parameters of \code{bsw}.
#' @usage coef(object)
#' @param object An object of class \code{"bsw"}.
#' @return A numeric vector giving the estimated model parameters.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

setMethod(f = "coef",
          signature = "bsw",
          definition = function(object) {
            return(object@coefficients)
          }
)
