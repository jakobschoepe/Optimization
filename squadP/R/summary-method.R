#' @title Summarizing the estimated model parameters of the log-binomial model fitted by \code{squadP}
#' @description For objects of class \code{"squadP"}, \code{summary} summarizes the estimated model parameters of the log-binomial model fitted by \code{squadP}.
#' @param object An object of class \code{"squadP"}.
#' @details \code{summary} .
#' @return A list containing the following elements:
#' \item{coefficients}{A numeric vector containing the estimated coefficients of the log-binomial model.}
#' \item{std.err}{A numeric vector containing the estimated standard errors of the coefficients of the log-binomial model.}
#' \item{z.value}{A numeric vector containing the estimated z test statistic of the coefficients of the log-binomial model.}
#' \item{p.value}{A numeric vector containing the estimated p values of the coefficients of the log-binomial model.}
#' @author Adam Bekhit, Jakob Schöpe
#' @export

setMethod(f = "summary",
          signature = "squadP",
          definition = function(object) {
            cf <- coef(object)
            ci <- confint(object)
            x <- object@x
            y <- object@y
            se <- sqrt(diag(solve(hess(cf, y, x))))
            z <- cf / se
            p <- 2 * pnorm(abs(z), lower.tail = FALSE)
            coef.table <- cbind(as.matrix(cf), as.matrix(se), as.matrix(z), as.matrix(p), as.matrix(exp(cf)), exp(ci))
            colnames(coef.table) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)", "RR", colnames(ci))
            
            cat("Call:\n")
            print(object@call)
            cat("\nConvergence:", object@converged)
            cat("\nCoefficients:\n")
            print(coef.table)
            cat("\nIterations:", object@iter, "\n")
            return(invisible(list(coefficients = cf, std.err = se, z.value = z, p.value = p)))
          }
)
