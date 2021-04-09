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
getFMI <- function (data, infinity) 
{
  
  ## NOT DONE YET !!!
  if (infinity == "yes")
  {
    dataX2 <- data.frame(data[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
    #column data.frames automatically into numeric vectors
    
    colnames(dataX2) <- c("X2") #Name column
    
    #Set up Model
    
    data.cfa <- 'X1 =~ X2' 
    step1.cfa <- cfa(data.cfa, data = dataX2, missing = "fiml", std.lv = TRUE) 
    
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
  }
  else if (infinity == "no")
  {
    
    dataX2 <- data.frame(data[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
    #column data.frames automatically into numeric vectors
    
    colnames(dataX2) <- c("X2") #Name column
    
    #Set up Model
    
    data.cfa <- 'X1 =~ X2' 
    step1.cfa <- cfa(data.cfa, data = dataX2, missing = "fiml", std.lv = TRUE) 
    
    se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa -> Fit -> se ; are the same values
    cov.cfa <- step1.cfa@implied[["cov"]][[1]]
    means.cfa <- step1.cfa@implied[["mean"]][[1]]
    
    #Multiple model-implied cov by N/N-1, only worth doing with small N
    
    cov.cfa <- cov.cfa*(parm$n/(parm$n-1))
    
    #Sepcify row and columnnames according to model
    
    rownames(cov.cfa) <- c("X2")
    colnames(cov.cfa) <- c("X2")
    
    #run the model with model-implied cov matrix and means as input
    step2.cfa <- cfa(data.cfa,
                     sample.cov = cov.cfa,
                     sample.mean = means.cfa,
                     sample.nobs = parm$n, 
                     std.lv = TRUE,
                     meanstructure = TRUE,
                     information = "observed")
    
    se.step2.cfa <- parameterEstimates(step2.cfa)$se
    
    #Compute vector of fraction of missing information estimates
    
    fmi <- 1-(se.step2.cfa^2/se.cfa^2)
    fmi
    
  }
  
  else 
  {
    stop("Unknown or no Sample size specified")
  }
  
}

######################################################################################################

## Simulate a nonresponse vector via a linear probability model:
  simLinearMissingness <- function(pm,
                                   data,
                                   auc      = NULL,
                                   snr      = NULL,
                                   optimize = TRUE,
                                   preds    = colnames(data),
                                   type     = "high",
                                   beta     = rep(1.0, length(preds)),
                                   stdData  = TRUE,
                                   ...)
  {
    if(is.null(snr) & is.null(auc))
      stop("You must define a value of either 'snr' or 'auc'")
    
    if(optimize & !type %in% c("high", "low"))
      stop("'type' must be either 'high' or 'low'")
    
    ## Standardize the missing data predictors:
    if(stdData) data <- scale(data)
    
    ## Define the (centered) linear predictor:
    eta <- as.numeric(as.matrix(data) %*% matrix(beta))
    
    ## Find the optimal proportion of noise:
    if(optimize) {
      noise    <- rnorm(length(eta), 0, sd(eta))
      noiseFit <- .optNoise(auc      = auc,
                            eta      = eta,
                            noise    = noise,
                            type     = type,
                            pm       = pm,
                            logistic = FALSE,
                            ...)
      
      ## Add noise to the linear predictor:
      eta2 <- eta + (noiseFit$minimum * noise)
    }
    else
      ## Define the noisy linear predictor in terms of the specified SNR:
      eta2 <- eta + (1 / snr) * rnorm(length(eta), 0, sd(eta))
    
    r <- .linProbMissingness(eta = eta2, pm = pm, type = type)
    
    ## Compute the achieved AUC:
    auc <- ifelse(type %in% c("high", "low"),
                  as.numeric(auc(roc(r, eta, quiet = TRUE, ...))),
                  NA)
    
    list(r   = r,
         eta = eta2,
         auc = auc,
         snr = sd(eta) / sqrt(var(eta2) - var(eta))
    )
  }
