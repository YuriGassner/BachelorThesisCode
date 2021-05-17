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
#test

##-------------------------------------------------------------------------------------------------------------------##

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

##-------------------------------------------------------------------------------------------------------------------##

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
#New doRep function saving the impList 

doIter <- function(rp, conds, parm)
{
  #data_main <- try(simData(parm = parm, N = parm$n))
  data <- try(simData(parm = parm,
                      N = parm$n))
  c <- rp
  
  setSeed(parm = parm,
          rp = c)
     
  for (i in 1 : nrow(conds))
  {
    
    #Create a seperate data matrix to avoid possible problems
    #data <- data_main
    
    
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
          file = paste0("results/Rep",c,".rds")) #c is the current iteration
  
 # variable <- "directory"
 # paste0(variable,"doRep2_",c,".rds")
  
### KML: Pass your output directory/filename as a variable
}




##-------------------------------------------------------------------------------------------------------------------##


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
### POTENTIALLY EXCLUDE X2 AS THERE ARE NO MISSING VALUES? but also maybe not
### KML: Don't exclude X2. The FMI will only be optimal when estimated from a fully saturated model.
    ))
    
    
    #Save FMIs to list
    storage[[i]] <- fmi
    
    
  }
  
  
  saveRDS(storage,
          file = paste0("results/data_trueFMI",i,".rds"))  ## save as variable
  
  
}


##-------------------------------------------------------------------------------------------------------------------##
#Reusing the big impSet to save computational cost
#Adjusts the number of m to the newly specified m while maintaining the big impList of m = 500

adjustImpList <- function(impList, parm)
{
  
  # #Copy the m = 500 imputation list
  # out <- impListdf ### KML: Why copy?
  # 
  # 
  # #Compute 
  # # parm$mcomp <- parm$m/length(out)
  # #Sampling m imputed data sets from the current 
  # r <- sample(1:length(out), parm$m)
  
  impList[sample(1:length(impList), parm$m)]

### KML: Don't you want to be sampling m imputations, not length(out) - m? This
### code will return 45 imputed datasets in the m = 5 condition.
### Oh, I see what you've done below. So, this function will work, but it's much
### more complicated than necessary (as noted below).
  

### KML: The 'r' vector you create above will contain m randomly sampled indices
### (after you fix the issue noted above). So, you can directly subset your list
### of imputated datsets with 'r'. You don't need to create any logical vectors.
    
  #Creating a vector of 500 FALSE elements and replacing previously sampled elements with TRUE
  # tmp <- rep(FALSE, length(out))
  # tmp[r] <- TRUE
  # r <- tmp
  # 
  # #As the imputation list is a list, this also has to be a list
  # list <- list(r = r)
  # 
  # 
  # #Remove the 'TRUE' values from the imputation list
  # impList2 <- out
  # impList2[list$r] <- NULL
  # impList2
  
}



##-------------------------------------------------------------------------------------------------------------------##

setSeed <- function(parm = parm, rp = rp)
{
  .lec.SetPackageSeed(rep(parm$seed, 6))
  if(!rp %in% .lec.GetStreams())
    .lec.CreateStream(c(1:parm$nStreams))
  .lec.CurrentStream(rp)
}