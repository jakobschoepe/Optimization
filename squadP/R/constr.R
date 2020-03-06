#' @title Setting the constraints for constrained non-linear optimization in \code{squadP}
#' @description \code{constr} sets the constraints for constrained non-linear optimization in \code{squadP}.
#' @usage constr(x)
#' @param x A model matrix.
#' @return A matrix containing the constraints for constrained non-linear optimization in \code{squadP}.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

constr <- function(x) {
  colMax <- matrixStats::colMaxs(x)
  colMin <- matrixStats::colMins(x)
  const <- expand.grid(lapply(2:length(colMax), function(i) {c(colMin[i], colMax[i])}))
  Amat <- unname(as.matrix(cbind(rep(1, times = nrow(const)), const)))
  return(-Amat)
}
