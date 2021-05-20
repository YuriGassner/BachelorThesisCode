#Parameters of the simulation:
parm <- list()

parm$iter       <- c(240)                                        #Iterations, WILL CHANGE TO 1000 IF CODE IS FULLY READY
parm$n          <- c(100)                                      #Sample size
parm$Nfmi       <- c(5000000)                                   #Large N to approximate infinity
parm$rsq        <- c(0.4)                                      #RSquared
parm$cov        <- c(0.3)                                      #Covariance (with infinite N, is equal to correlation)
parm$pred       <- c(2)                                        #Number of Predictors: X1, X2
parm$Vecpred    <- c(2)                                        #Columnnames of predictors
parm$Ncond      <- c(50)                                       #Number of conditions: mec x pm x m
parm$seed       <- c(507669)                                   #Starting seed
parm$nStreams   <- c(500)                                      #How many iterations is my Simulation running, thats how many seed values I need
snr             <- c(1)                                        #Defining the signal-to-noise ratio


directory <- "results/"                                        #Directory for all the output


mec             <- c("MCAR", "MAR")                            #Missingness mechanism
pm              <- c(0.9, 0.75, 0.5, 0.25, 0.1)                #Percent Missing
m               <- c(500, 250, 200, 100, 50, 25, 10, 5)        #Number of imputations

conds <- expand.grid(m = m, pm = pm, mec = mec)                #Condition Matrix, specified so that m changes first while other par remain constant

condsFMI <- expand.grid(pm = pm, mec = mec)                    #Condition Matrix for the true FMI approximation

store <- vector("list", length = nrow(conds))                  #List of matrices for temporary storage of each iteration
storage <- vector("list", length = nrow(conds))                #List of matrices for temporary storage of the trueFMI values              
