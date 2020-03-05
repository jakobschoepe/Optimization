#' @title Estimating confidence intervals of the estimated model parameters of the log-binomial model
#' @description For objects of class \code{"squadP"}, \code{confint} estimates confidence intervals of the estimated model parameters of the log-binomial model.
#' @param object An object of class \code{"squadP"}.
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level A numeric value giving the level of confidence.
#' @details \code{confint} provides Wald confidence intervals of the estimated model parameter of the log-binomial model fitted by \code{squadP}.
#' @return A matrix with columns giving the lower and upper confidence limits of each estimated model parameter.
#' @references 
#' @author Adam Bekhit, Jakob Schöpe
#' @export

setMethod(f = "confint",
          signature = "squadP",
          definition = function(object, parm, level = .95) {
            if (!is.numeric(x = level)) {
              stop("\"level\" must be a numeric value")
            }
            
            else if (length(x = level) != 1L) {       
              stop("single numeric value for \"level\" expected")          
            }
            
            else {
              cf <- coef(object)
              pnames <- names(cf)
              if (missing(parm)) { 
                parm <- pnames
              }
              
              else if (is.numeric(parm)) { 
                parm <- pnames[parm]
              }
              y <- object@y
              x <- object@x
              alpha <- (1 - level) / 2
              p <- c(alpha, 1 - alpha)
              q <- qnorm(p = p)
              ci <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, paste(x = format(x = 100 * p, digits = 3, scientific = FALSE, trim = TRUE), "%", sep = "")))
              se <- sqrt(diag(solve(hess(cf, y, x))))[parm]
              ci[] <- cf[parm] + se %o% q
              return(ci)
            }
          }
)
