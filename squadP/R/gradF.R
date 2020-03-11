#' @title Deriving the first derivative of the likelihood function in \code{bsw}
#' @description \code{gradF} is a helper function for \code{bsw} to derive the first derivative of the likelihood function.
#' @usage gradF(theta, y, x)
#' @param theta A numeric vector containing .
#' @param y A numeric vector containing the dependent variable of the model.
#' @param x The model matrix.
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
