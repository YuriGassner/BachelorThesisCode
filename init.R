#Parameters of the simulation:
parm <- list()


parm$mec        <- c("MCAR", "MAR")               #Missingness mechanism
parm$pm         <- c(0.1, 0.25, 0.5, 0.75, 0.9)   #Percent Missing
parm$m          <- c(5, 50, 100, 250, 500)        #Number of imputations
parm$iter       <- c(1)                           #Iterations
parm$n          <- c(100)                         #Sample size
parm$Nfmi       <- c(1000000)                     #Large N to approximate infinity
parm$rsq        <- c(0.4)                         #RSquared
parm$cov        <- c(0.3)                         #Covariance (with infinite N, is equal to correlation)
parm$snr        <- c(1)                           #Signal to Noise ratio
parm$pred       <- c(2)                           #Number of Predictors: X1, X2
parm$Vecpred    <- c(1,1)                         #Vector of N pred, otherwise simLinearMissingness doesn't work
parm$Ncond      <- c(50)                          #Number of conditions: mec x pm x m