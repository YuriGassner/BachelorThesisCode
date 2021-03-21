##Libraries
library("SimDesign")

# Functions
##-------------------------------------------------------------------------------------------------------------------##
simData <- function (parm$n, parm$coef, parm$cov, parm$rsq)
{
  #Construct covariance matrix
  nPred <- length(parm$coef) - 1
  sigma <- matrix(parm$cov, parm$n, parm$n)
  diag(sigma) <- 1.0
  
  #Generate data
  X <- rmvnorm(n = parm$n, mean = rep(0, nPred), sigma = sigma)
  
  #Generate error termn
  #if r.sqrd = 0, we need to fix this stuff.
  
  
  beta <- coefficients[-1]
  if(parm$rsq > 0)
  {
    var.model <- t(beta) %*% cov(X) %*% beta
    var.residual <- (var.model/parm$rsq) - var.model
    U = rnorm(parm$n, mean = 0, sd = sqrt(var.residual))
    #compute Y
    Y <- coefficients[1] + X  %*% beta + U 
  }
  else
  {
    Y <- rnorm(n=500, mean=0, sd=1)
  }
  
  
  
  data <- data.frame(X)
  
  #Add to data frame
  data$Y <- as.vector(Y)
  
  #Return data
  data
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
                                snr      = snr,
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


calcFMI <- function (data)
{
  

}


###----------------------------------------------------------###


impData <- function(data)


    
###--------------------------------------------------------------------------###
  
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
