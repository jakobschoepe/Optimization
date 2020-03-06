#' @title Deriving the first derivative of the likelihood function in \code{squadP}
#' @description \code{gradF} is a helper function for \code{squadP} to derive the first derivative of the likelihood function.
#' @usage gradF(theta, y, x)
#' @param theta A numeric vector containing .
#' @param y A numeric vector containing the dependent variable of the log-binomial model.
#' @param x The model matrix of the log-binomial model.
#' @return A numeric vector containing the first derivatives of the likelihood function.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

gradF <- function(theta, y, x) {
  p <- exp(x %*% theta)
  p[p >= 1] <- 1 - 1e-5
  s <- (y - p) / (1 - p)
  deriv1 <- as.vector(t(x) %*% s)
  return(deriv1)
}
