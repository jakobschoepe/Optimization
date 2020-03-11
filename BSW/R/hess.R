#' @title Deriving the second partial derivatives of the likelihood function in \code{bsw} (Hessian matrix)
#' @description \code{hess} is a helper function for \code{bsw} to derive the second partial derivatives of the likelihood function.
#' @usage hess(theta, y, x)
#' @param theta A numeric vector containing the model parameters.
#' @param y A numeric vector containing the dependent variable of the model.
#' @param x The model matrix.
#' @return A numeric matrix containing the second partial derivatives of the likelihood function (Hessian matrix).
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
