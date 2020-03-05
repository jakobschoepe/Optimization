#' @title Fitting a log-binomial model using constrained non-linear optimization
#' @description \code{squadP} fits a log-binomial model using constrained non-linear optimization.
#' @usage squadP(formula, data)
#' @param formula An object of class \code{"formula"} (or one that can be coerced to that class): a symbolic description of the model to be fitted. 
#' @param data A data frame containing the variables in the model.
#' @details 
#' @return An object of S4 class \code{"squadP"} containing the following slots:
#' \item{call}{}
#' \item{coefficients}{A numeric vector containing the estimated coefficients of the log-binomial model.}
#' \item{iter}{A positive integer indicating the number of iterations.}
#' \item{converged}{A logical constant that indicates whether the log-binomial model has converged.}
#' \item{y}{A numerical vector containing the dependent variable of the log-binomial model.}
#' \item{x}{The model matrix of the log-binomial model.}
#' @references Work in progress.
#' @author Adam Bekhit, Jakob Schöpe
#' @export

squadP <- function(formula, data) {
  call <- match.call()
  if (!inherits(x = formula, what = "formula")) {
    stop("\"formula\" must be of class \"formula\"")
  }
  
  else {
    data <- model.frame(formula = formula, data = data)
    y <- unname(model.matrix(as.formula(paste("~", all.vars(formula)[1])), data = data)[,-1])
    x <- model.matrix(object = formula, data = data)
    ymean <- ifelse(mean(y) <= 0, 0.01, mean(y))
    theta <- c(log(ymean), rep(0, times = ncol(x) - 1))
    Amat <- constr(x)
    bvec <- rep(0, times = nrow(Amat))
    converged <- FALSE
    iter <- 0
  
    while(isFALSE(converged)) {
      iter <- iter + 1
      Dmat <- Matrix::nearPD(hess(theta, y, x))$mat
      dvec <- gradF(theta, y, x) + t(theta) %*% Dmat
      fit <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec, Amat = t(Amat), bvec = bvec)
      converged <- all(abs(fit$solution - theta) < 1e-4)
      theta <- fit$solution
      names(theta) <- colnames(x)
    }
    return(new(Class = "squadP", call = call, coefficients = theta, iter = iter, converged = converged, y = y, x = x))
  }
}

