# File: MLE.R
# Author: Jakob Schöpe
# Date: March 7, 2018


# Normal model
# Specify the log-likelihood function
logLL <- function(theta, X, y) {
  -sum(x = dnorm(x = y, mean = X %*% theta, sd = sd(x = y), log = TRUE))
}

# Binomial model
# Specify the log-likelihood function
logLL <- function(theta, X, y) {
  -sum(x = dbinom(x = y, size = 1, prob = exp(X %*% theta)/(1 + exp(X %*% theta)), log = TRUE))
}

# Poisson model
# Specify the log-liklihood function
logLL <- function(theta, X, y) {
  -sum(x = dpois(x = y, lambda = X %*% theta, log = TRUE))
}

mle <- optim(par = c(0,0), fn = logLL, x = model.matrix(y ~ x1, data = data), y = data$y, method = "BFGS", hessian = TRUE)
se <- sqrt(x = diag(x = solve(a = mle$hessian)))
p <- pchisq(q = (mle$par/se)^2, df = 1, lower.tail = FALSE)
aic <- 2 * mle$value + 2 * 3
