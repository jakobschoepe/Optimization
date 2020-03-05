#' @title S4 Class \code{"squadP"}
#' @slot call .
#' @slot coefficients A numerical vector containing the estimated coefficients of the log-binomial model.
#' @slot iter A positive integer indicating the number of iterations.
#' @slot converged A logical constant that indicates whether the log-binomial model has converged.
#' @slot y A numerical vector containing the dependent variable of the log-binomial model.
#' @slot x The model matrix of the log-binomial model.
#' @author Adam Bekhit, Jakob Schöpe
#' @import Matrix matrixStats quadprog
#' @export

setClass(Class = "squadP", slots = c(call = "language", coefficients = "numeric", iter = "numeric", converged = "logical", y = "numeric", x = "matrix"))
