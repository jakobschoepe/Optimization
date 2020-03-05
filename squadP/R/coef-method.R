#' @title Extracting the estimated model parameters of the fitted log-binomial model
#' @description For objects of class \code{"squadP"}, \code{coef} extracts the estimated model parameters of the fitted log-binomial model.
#' @param object An object of class \code{"squadP"}.
#' @return A real vector giving the estimated model parameters.
#' @references Work in progress.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

setMethod(f = "coef",
          signature = "squadP",
          definition = function(object) {
            return(object@coefficients)
          }
)
