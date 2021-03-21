rm(list=ls(all=TRUE))



nPred <- parm$pred
sigma <- matrix(parm$cov, parm$pred, parm$pred)
diag(sigma) <- 1.0

#Generate data
X <- rmvnorm(n = parm$n, mean = rep(0, nPred), sigma = sigma)

Y <- rnorm(n = parm$n, mean = 0, sd = 1)

data <- data.frame(X,Y)

data

typeof(data)
