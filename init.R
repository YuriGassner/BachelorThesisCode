#Parameters of the simulation:
parm <- list()

parm$iter       <- c(5)                           #Iterations, WILL CHANGE TO 1000 IF CODE IS FULLY READY
parm$n          <- c(100)                         #Sample size
parm$Nfmi       <- c(500000)                      #Large N to approximate infinity
parm$rsq        <- c(0.4)                         #RSquared
parm$cov        <- c(0.3)                         #Covariance (with infinite N, is equal to correlation)
parm$snr        <- c(1)                           #Signal to Noise ratio
parm$pred       <- c(2)                           #Number of Predictors: X1, X2
parm$Vecpred    <- c(1,1)                         #Vector of N pred, otherwise simLinearMissingness doesn't work
parm$Ncond      <- c(50)                          #Number of conditions: mec x pm x m

mec             <- c("MCAR", "MAR")               #Missingness mechanism
pm              <- c(0.9, 0.75, 0.5, 0.25, 0.1)   #Percent Missing
m               <- c(500, 250, 100, 50, 5)        #Number of imputations

conds <- expand.grid(m = m, pm = pm, mec = mec)   #Condition Matrix, specified so that m changes first while other par remain constant

store <- vector("list", length = nrow(conds))     #List of matrices for temporary storage of each iteration
storage <- vector("list", length = nrow(conds))   #List of matrices for temporary storage of the trueFMI values                                         #Counter for storing the files doRep
