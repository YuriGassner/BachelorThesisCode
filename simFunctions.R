# Functions
##-------------------------------------------------------------------------------------------------------------------##

### KML: This is just a function for me to use for testing.
                                        #toyData <- function(n) {
                                        #    sigma       <- matrix(0.3, 2, 2)
                                        #    diag(sigma) <- 1.0
                                        #    
                                        #    dat1           <- rmvnorm(n, c(0, 0), sigma)
                                        #    colnames(dat1) <- paste0("X", 1 : 2)
                                        #    
                                        #    r             <- as.logical(rbinom(n, 1, 0.3))
                                        #    dat1[r, "X1"] <- NA
                                        #    as.data.frame(dat1)
                                        #}

##-------------------------------------------------------------------------------------------------------------------##

simData <- function (parm, N)
{
  
    n <- N                                   
    sigma <- matrix(parm$cov, parm$pred, parm$pred)
    diag(sigma) <- 1.0
    
    #Generate data
    X <- rmvnorm(n = n, mean = rep(0, parm$pred), sigma = sigma)
    
    data <- data.frame(X)
    
    data

}


##-------------------------------------------------------------------------------------------------------------------##

##-------------------------------------------------------------------------------------------------------------------##

makeMissing <- function(data, 
                        mechanism="MCAR", 
                        pm, 
                        preds, 
                        snr=NULL)
{
  #MAR missing data mechanism
  if(mechanism=="MAR")
  {
    
    
    #Specify where holes will be poked into the data sets
    out <- simLinearMissingness(pm       = pm,
                                data     = data,
                                snr      = parm$snr,
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
    out <- list(r   = r)#,
    
    
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
#New doRep function saving the impList 

doIter <- function(conds, parm, counter)
{
  data_main <- try(simData(parm = parm, N = parm$n))
  
  c <- counter
  
  
  for (i in 1 : nrow(conds))
  {
    
    #Create a seperate data matrix to avoid possible problems
    data <- data_main
    
    
    #Save current values of pm and mec to check if new imputed data sets need to be created
    pm <- parm$pm
    mec <- parm$mec
    m <- parm$m
    
    #Save current values of the varying values
    parm$m <- conds[i, "m"]
    parm$mec <- conds[i, "mec"]
    parm$pm <- conds[i, "pm"]
    
    
    check <- (is.null(pm) | is.null(mec)) || (pm != parm$pm | mec != parm$mec)
    #Is TRUE when either pm/mec equals NULL OR when either pm/mec are not the same as parm$pm/mec
    #When TRUE: new imputation list needs to be generated
    #Is FALSE when either pm/mec is not null OR when either pm/mec are the same as parm$pm/mec
    #When FALSE: no new imputation list is needed, list needs to be adjusted to new m!
    #Check does what it is supposed to do, only 10 imp sets are created per iteration
    
    if(check == "TRUE")
    {
      MissingData <- try(makeMissing(data = data,
                                     mechanism = parm$mec,
                                     pm = parm$pm,
                                     preds = parm$Vecpred,
                                     snr = NULL
      ))
      
      
      #Impute missing values
      impData <- try(mice(data = MissingData,
                          m = parm$m,
                          method = "norm",
                          print = FALSE
      ))
      
      
      #Save a list of imputed data sets
      impListdf <- try(complete(data = impData,
                                action = "all"
      ))
      
      impList <- impListdf
      
    }
    
    else if(check ==  "FALSE")
    {
     impList <- adjustImpList(impListdf = impListdf,
                              parm = parm)
     
    }
    
    else print("something went wrong")     #Very necessary
    
    
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
          file = paste0("results/doRep2_",c,".rds")) #c is the current iteration
  
  
}




##-------------------------------------------------------------------------------------------------------------------##


getTrueFMI <- function(conds, parm)
{
  
  #Create one dataset with N = 500.000
  data_main <- simData(parm = parm, N = parm$Nfmi)
  
  
  for(i in 1 : nrow(conds))
  {
    
    #Save current values of the varying values
    parm$m <- conds[i, "m"]
    parm$mec <- conds[i, "mec"]
    parm$pm <- conds[i, "pm"]
    
    
    #Keep reusing the same previously created dataset
    data <- data_main
    
    
    #Poke holes into the data set
    MissingData <- try(makeMissing(data = data,
                                   mechanism = parm$mec,
                                   pm = parm$pm,
                                   preds = parm$Vecpred,
                                   snr = NULL
    ))
    
    
    #Impute missing values via FIML and calculate FMI
    fmi <- try(fmi(data = MissingData,
                   method = "sat",
                   ### POTENTIALLY EXCLUDE X2 AS THERE ARE NO MISSING VALUES? but also maybe not
    ))
    
    
    #Save FMIs to list
    storage[[i]] <- fmi
    
    
  }
  
  
  saveRDS(storage,
          file = paste0("results/data_trueFMI",i,".rds"))
  
  
}


##-------------------------------------------------------------------------------------------------------------------##
#Reusing the big impSet to save computational cost
#Adjusts the number of m to the newly specified m while maintaining the big impList of m = 500

adjustImpList <- function(impListdf, parm)
{
  
  #Copy the m = 500 imputation list
  out <- impListdf
  
  
  #Compute 
  parm$mcomp <- parm$m/length(out)
  #Sampling pm*500 observations
  r <- sample(1:length(out), length(out)-parm$m)
  
  
  #Creating a vector of 500 FALSE elements and replacing previously sampled elements with TRUE
  tmp <- rep(FALSE, length(out))
  tmp[r] <- TRUE
  r <- tmp
  
  
  #As the imputation list is a list, this also has to be a list
  list <- list(r = r)
  
  
  #Remove the 'TRUE' values from the imputation list
  impList2 <- out
  impList2[list$r] <- NULL
  impList2
  
}



##-------------------------------------------------------------------------------------------------------------------##
