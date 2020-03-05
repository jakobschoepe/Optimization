#' @title .
#' @description \code{constr} .
#' @usage constr(x)
#' @param x 
#' @details 
#' @return 
#' @references Work in progress.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

constr <- function(x) {
  colMax <- matrixStats::colMaxs(x)
  colMin <- matrixStats::colMins(x)
  const <- expand.grid(lapply(2:length(colMax), function(i) {c(colMin[i], colMax[i])}))
  Amat <- unname(as.matrix(cbind(rep(1, times = nrow(const)), const)))
  return(-Amat)
}
