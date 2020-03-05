#' @title .
#' @description \code{hess} .
#' @usage hess(theta, y, x)
#' @param theta 
#' @param y 
#' @param x 
#' @details 
#' @return 
#' @references Work in progress.
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
