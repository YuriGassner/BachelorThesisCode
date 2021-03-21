#Parameters of the simulation:
parm <- list()



#Variable
#covariances <- c(0, 3.33, 10)       # Covariances
#r.squared   <- c(0, .1, .5)         # R-square
#mechanism   <- c("MCAR", "MAR")     # Missing data mechanism
#snr         <- c(.65, .75)          # SNR
#pm          <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#m           <- c(5, 50, 100, 250, 500)
#iter        <- 1                    # iterations

#Fixed
#parameters$n    <- 100              #sample size
#parameters$c    <- list(c(1, 1.5),  #Coefficients for study 1
#                        c(0, 2))    #and 2 respectively
parm$mec    <- c("MCAR", "MAR")
parm$pm     <- c(0.1, 0.25, 0.5, 0.75, 0.9)
parm$m      <- c(5, 50, 100, 250, 500)
parm$iter   <- c(1)
parm$n      <- c(100)
parm$rsq    <- c(0.5)
parm$cov    <- c(5)
parm$snr    <- c(0.65) #Signal to Noise ratio
