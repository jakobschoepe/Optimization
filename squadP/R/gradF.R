#' @title .
#' @description \code{gradF} .
#' @usage gradF(theta, y, x)
#' @param theta 
#' @param y 
#' @param x 
#' @details 
#' @return 
#' @references Work in progress.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

gradF <- function(theta, y, x) {
  p <- exp(x %*% theta)
  p[p >= 1] <- 1 - 1e-5
  s <- (y - p) / (1 - p)
  deriv1 <- as.vector(t(x) %*% s)
  return(deriv1)
}
