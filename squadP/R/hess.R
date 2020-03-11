#' @title Computing the Hessian matrix in \code{bsw}
#' @description \code{hess} is a helper function for \code{bsw} to compute the Hessian matrix.
#' @usage hess(theta, y, x)
#' @param theta A numeric vector containing the model parameters.
#' @param y A numeric vector containing the dependent variable of the model.
#' @param x The model matrix.
#' @return The Hessian matrix in \code{bsw}.
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
