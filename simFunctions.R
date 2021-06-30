### Title:    Functions for the Bachelor Thesis
### Author:   Yuri T.C.A. Gaßner
### Created:  2021-06-30

##-------------------------------------------------------------------------------------------------------------------##
# Simulate Data Function

simData <- function (parm, N)
{
  
    sigma <- matrix(parm$cov, parm$pred, parm$pred)
    diag(sigma) <- 1.0
    
    #Generate data
    X <- rmvnorm(n = N, mean = rep(0, parm$pred), sigma = sigma)
    
    data <- data.frame(X)
    
    data

}


##-------------------------------------------------------------------------------------------------------------------##
# Poke Holes Function

makeMissing <- function(data, 
                        mechanism="MCAR", 
                        pm, 
                        preds, 
                        snr=snr)
{
  #MAR missing data mechanism
  if(mechanism=="MAR")
  {
    
    
    #Specify where holes will be poked into the data sets
    out <- simLinearMissingness(pm       = pm,
                                data     = data,
                                snr      = snr, 
                                preds    = preds,
                                type     = "high",
                                optimize = FALSE)
    
    
    #Poke Holes
    missingdf <- data
    missingdf[out$r , 1] <- NA
    missingdf
    
  }
  
  #MCAR missing data mechanism
  else if(mechanism=="MCAR")
  {
    
    #Random sampling pm*N elements from the df
    r <- sample(1:nrow(data), nrow(data)*pm)
    
    
    #Creating a vector of 500 FALSE elements and replacing previously sampled elements with TRUE
    tmp <- rep(FALSE, nrow(data))
    tmp[r] <- TRUE
    r <- tmp
    out <- list(r   = r)
    
    
    #Poke holes
    missingdf <- data
    missingdf[out$r , 1] <- NA
    missingdf
    
    
  }
  
  else
    
  {
    stop("Undefined or unsupported missing data mechanism.")
  }
  
}



##-------------------------------------------------------------------------------------------------------------------##
# doRep Function 

doIter <- function(rp, conds, parm)
{
  data <- try(simData(parm = parm,
                      N = parm$n))
  c <- rp
  
  setSeed(parm = parm,
          rp = c)
     
  for (i in 1 : nrow(conds))
  {
    #Save current values of pm and mec to check if new imputed data sets need to be created
    pm <- parm$pm
    mec <- parm$mec
    m <- parm$m
    
    #Save current values of the varying values
    parm$m <- conds[i, "m"]
    parm$mec <- conds[i, "mec"]
    parm$pm <- conds[i, "pm"]
    
    
    check <- (is.null(pm) | is.null(mec)) || (pm != parm$pm | mec != parm$mec)
    
    if(check) 
    {
      MissingData <- try(makeMissing(data = data,
                                     mechanism = parm$mec,
                                     pm = parm$pm,
                                     preds = parm$Vecpred,
                                     snr = snr
      ))
      
      
      #Impute missing values
      impData <- try(mice(data = MissingData,
                          m = parm$m,
                          method = "norm",
                          print = FALSE,
                          maxit = 1          #as only one variable has missing values
      ))
      
      
      #Save a list of imputed data sets
      impList <- try(complete(data = impData,
                              action = "all"
      ))
      
      
    }
    
     else if(!check)
    {
     impList <- adjustImpList(impList = impList,
                              parm = parm)
     
    }

      length(impList)
      
    
    
    
    #Calculate FMI
    fmi <- try(fmi(data = impList,
                   method = "sat",
                   fewImps = TRUE
    ))
    
    
    #Save FMIs to list
    store[[i]] <- fmi
    
  }
  
  #Write list to disc
  saveRDS(store, 
          file = paste0(directory,"Rep",c,".rds")) #c is the current iteration
  
  
}




##-------------------------------------------------------------------------------------------------------------------##
# Function to approximate true FMI values

getTrueFMI <- function(condsFMI, parm)
{
  
  #Create one dataset with N = 500.000
  data <- simData(parm = parm, N = parm$Nfmi)
  
  
  for(i in 1 : nrow(condsFMI))
  {
    
    #Save current values of the varying values
    parm$mec <- condsFMI[i, "mec"]
    parm$pm <- condsFMI[i, "pm"]
    
    
    #Poke holes into the data set
    MissingData <- try(makeMissing(data = data,
                                   mechanism = parm$mec,
                                   pm = parm$pm,
                                   preds = parm$Vecpred,
                                   snr = snr
    ))
    
    
    #Impute missing values via FIML and calculate FMI
    fmi <- try(fmi(data = MissingData,
                   method = "sat",
    ))
    
    
    #Save FMIs to list
    storage[[i]] <- fmi
    
    
  }
  
  
  saveRDS(storage,
          file = paste0(directory,"data_trueFMI.rds"))
  
  
}


##-------------------------------------------------------------------------------------------------------------------##
# Reusing the big impSet to save computational cost
# Adjusts the number of m to the newly specified m while maintaining the big impList of m = 500

adjustImpList <- function(impList, parm)
{

  
  impList[sample(1:length(impList), parm$m)]
  
}
SE <- function(x){
  sd(x)/sqrt(500)
}



##-------------------------------------------------------------------------------------------------------------------##
# Seedfunction to have independent samples when parallelising the process

setSeed <- function(parm = parm, rp = rp)
{
  .lec.SetPackageSeed(rep(parm$seed, 6))
  if(!rp %in% .lec.GetStreams())
    .lec.CreateStream(c(1:parm$nStreams))
  .lec.CurrentStream(rp)
}