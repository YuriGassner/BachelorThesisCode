#Parameters of the simulation:
parm <- list()


parm$mec        <- c("MCAR", "MAR")               #Missingness mechanism
parm$pm         <- c(0.1, 0.25, 0.5, 0.75, 0.9)   #Percent Missing
parm$m          <- c(5, 50, 100, 250, 500)        #Number of imputations
parm$iter       <- c(1)                           #Iterations
parm$n          <- c(100)                         #Sample size
parm$Nfmi       <- c(1000000)                     #Large N to approximate infinity
parm$rsq        <- c(0.4)                         #RSquared
parm$cov        <- c(0)                           #Covariance
parm$snr        <- c(1)                           #Signal to Noise ratio
parm$pred       <- c(3)                           #Number of Predictors




# ### Stores values in a vector as for loops progress
# 
# vector <- vector("numeric")
# u <- 1:4
# 
# for (i in u)
# {
#   f <- rnorm(n = parm$n, mean = 0, sd = 1)
#   vector <- append(vector, f)
# }
# vector
# 
# ### Store values in a list 
# 
# list <- list()
# 
# for (i in u)
# {
#   g <- rnorm(n = parm$n, mean = 0, sd = 1)
#   list[[i]] <- g
#   
# }
# list
# 
# 
# ### Rmvnorm (bastian)
# 
# covariance <- 0
# nrpred <- 3
# sigma <- matrix(covariance, nrpred, nrpred)
# diag(sigma) <- 1.0
# 
# X <- rmvnorm(n = 100, mean = rep(0, nrpred), sigma = sigma)
