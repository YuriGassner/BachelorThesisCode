# Functions
##-------------------------------------------------------------------------------------------------------------------##

simData <- function (parm, infinity)
{
  if (infinity == "no")
  {
    sigma <- matrix(parm$cov, parm$pred, parm$pred)
    diag(sigma) <- 1.0
    
    #Generate data
    #X1 will act as Y and X2 will 
    X <- rmvnorm(n = parm$n, mean = rep(0, parm$pred), sigma = sigma)
    
    data <- data.frame(X)
    
    data
  }
  
  else if (infinity == "yes")  #Approximate an infinite N to approximate a true FMI
    
  {
    sigma <- matrix(parm$cov, parm$pred, parm$pred)
    diag(sigma) <- 1.0
    
    #Generate data
    X <- rmvnorm(n = parm$Nfmi, mean = rep(0, parm$pred), sigma = sigma)
    
    data <- data.frame(X)
    
    data
  }
  
  else
  {
    stop("Undefined or unsupported Sample Size")
  }
}

##-------------------------------------------------------------------------------------------------------------------##
# data       - the data frame which should get missing observations
# mechanism  - the mechanism of missing data, by default MCAR
# percent    - the proportion of observations that should be set to missing (NA)
# indices    - A vector of indices indicating which columns should contain missing values
makeMissing <- function(data, 
                        mechanism="MCAR", 
                        pm, 
                        preds, 
                        snr=NULL)
{
  #MAR missing data mechanism
  if(mechanism=="MAR")
  {
    out <- simLinearMissingness(pm       = pm,
                                data     = data,
                                snr      = parm$snr,
                                preds    = preds,
                                type     = "high",
                                optimize = FALSE)
    
    out
  }
  
  #MCAR missing data mechanism
  else if(mechanism=="MCAR")
  {
    r <- sample(1:nrow(data), nrow(data)*pm)
    tmp <- rep(FALSE, nrow(data))
    tmp[r] <- TRUE
    r <- tmp
    out <- list(r   = r)#,
    #eta = eta2,
    #auc = auc,
    #snr = sd(eta) / sqrt(var(eta2) - var(eta)))
    #return
    out
  }
  else
  {
    stop("Undefined or unsupported missing data mechanism.")
  }
}


###----------------------------------------------------------###
## Only works for 3 Predictors! 
## Only for the "true value" of the FMI, delete infinity=no since it is made with MI & SEMTOOLS
getFimlFmi <- function (data)
{
  dataX1 <- data.frame(data[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
  #column data.frames automatically into numeric vectors
  
  colnames(dataX1) <- c("X1") #Name column
  
  #Set up Model
  
  data.cfa <- 'X2 =~ X1' 
  step1.cfa <- cfa(data.cfa, data = dataX1, missing = "fiml", std.lv = TRUE) 
  
  se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa - Fit - se ; are the same values
  cov.cfa <- step1.cfa@implied[["cov"]][[1]]
  means.cfa <- step1.cfa@implied[["mean"]][[1]]
  
  #Multiple model-implied cov by N/N-1, only worth doing with small N
  
  cov.cfa <- cov.cfa*(parm$Nfmi/(parm$Nfmi-1))
  
  #Sepcify row and columnnames according to model
  
  rownames(cov.cfa) <- c("X1")
  colnames(cov.cfa) <- c("X1")
  
  #run the model with model-implied cov matrix and means as input
  step2.cfa <- cfa(data.cfa,
                   sample.cov = cov.cfa,
                   sample.mean = means.cfa,
                   sample.nobs = parm$Nfmi, 
                   std.lv = TRUE,
                   meanstructure = TRUE,
                   information = "observed")
  
  se.step2.cfa <- parameterEstimates(step2.cfa)$se
  
  #Compute vector of fraction of missing information estimates
  
  fmi <- 1-(se.step2.cfa^2/se.cfa^2)
  fmi 
  
  
  # ########################################
  # ##Calculate "TRUE" FMI
  # datainf <- simData(parm = parm,
  #                  infinity = "yes")
  # sd(datax$X1) #Approximately 1
  # s <- sd(datax$X1)/sqrt(parm$Nfmi) #std. error approximately 0
  # 
  # se.real <- c(0,NA,NA,0,NA) #NA are also in the normal file, I dont know why
  # 
  # fmi <- 1 - (se.real^2/se.cfa^2)
  # fmi
}

