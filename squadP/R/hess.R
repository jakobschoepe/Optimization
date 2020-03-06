#' @title Computing the Hessian matrix of the log-binomial model fitted by \code{squadP}
#' @description \code{hess} is a helper function for \code{squadP} to compute the Hessian matrix of the log-binomial model.
#' @usage hess(theta, y, x)
#' @param theta 
#' @param y A numeric vector containing the dependent variable of the log-binomial model.
#' @param x The model matrix of the log-binomial model.
#' @return The Hessian matrix of the log-binomial model fitted by \code{squadP}.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

hess <- function(theta, y, x) {
  p <- exp(x %*% theta)
  p[p >= 1] <- 1 - 1e-5
  s <- p * (y - 1) / (1 - p)^2
  im <- 0
  for(i in 1:nrow(x)){
    im <- im + x[i,] %*% t(x[i,]) * s[i]
  }
  colnames(im) <- names(theta)
  rownames(im) <- names(theta)
  return(-im)
}
