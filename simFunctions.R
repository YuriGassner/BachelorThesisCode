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

simData <- function (parm)
{
                                       
    sigma <- matrix(parm$cov, parm$pred, parm$pred)
    diag(sigma) <- 1.0
    
    #Generate data
    X <- rmvnorm(n = parm$n, mean = rep(0, parm$pred), sigma = sigma)
    
    data <- data.frame(X)
    
    data

}


##-------------------------------------------------------------------------------------------------------------------##

simDataInf <- function (parm)
{
  
  sigma <- matrix(parm$cov, parm$pred, parm$pred)
  diag(sigma) <- 1.0
  
  #Generate data
  X <- rmvnorm(n = parm$Nfmi, mean = rep(0, parm$pred), sigma = sigma)
  
  data <- data.frame(X)
  
  data
  
}

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
    r <- sample(1:nrow(data), nrow(data)*pm)
    tmp <- rep(FALSE, nrow(data))
    tmp[r] <- TRUE
    r <- tmp
    out <- list(r   = r)#,
    #eta = eta2,
    #auc = auc,
    #snr = sd(eta) / sqrt(var(eta2) - var(eta)))
    #return
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

                                        #data <- toyData(1e7)

### KML: I've edited the following code, so it works as expected, but it turns
### out that you don't even need to specify your own function. The fmi()
### function from semTools will do this calculation for you.

## Only works for 3 Predictors! 
## Only for the "true value" of the FMI, delete infinity=no since it is made with MI & SEMTOOLS
getFimlFmi <- function (data)
{
                                        #dataX1 <- data.frame(data[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
                                        #column data.frames automatically into numeric vectors
  
                                        #colnames(dataX1) <- c("X1") #Name column
    
    ## Set up Model
    
                                        #data.cfa <- 'X2 =~ X1' 
                                        #step1.cfa <- cfa(data.cfa, data = dataX1, missing = "fiml", std.lv = TRUE) 

    data.cfa <- "X1 ~~ X2" 
    step1.cfa <- cfa(data.cfa, data = data, missing = "fiml") 
       
    se.cfa    <- parameterEstimates(step1.cfa)$se #step1.cfa - Fit - se ; are the same values
    cov.cfa   <- step1.cfa@implied[["cov"]][[1]]
    means.cfa <- step1.cfa@implied[["mean"]][[1]]
      
    ## Multiple model-implied cov by N/N-1, only worth doing with small N
    cov.cfa <- cov.cfa * (nrow(data) / (nrow(data) - 1))
    
                                        #Sepcify row and columnnames according to model
  
    rownames(cov.cfa) <- colnames(cov.cfa) <- c("X1", "X2")
  
    ## run the model with model-implied cov matrix and means as input
                                        #step2.cfa <- cfa(data.cfa,
                                        #                 sample.cov = cov.cfa,
                                        #                 sample.mean = means.cfa,
                                        #                 sample.nobs = parm$Nfmi, 
                                        #                 std.lv = TRUE,
                                        #                 meanstructure = TRUE,
                                        #                 information = "observed")
    
    step2.cfa <- cfa(data.cfa,
                     sample.cov    = cov.cfa,
                     sample.mean   = means.cfa,
                     sample.nobs   = nrow(data), 
                     meanstructure = TRUE,
                     information   = "observed")
    
    se.step2.cfa <- parameterEstimates(step2.cfa)$se
    
    ## Compute vector of fraction of missing information estimates
    fmi <- 1 - (se.step2.cfa^2 / se.cfa^2)
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



##-------------------------------------------------------------------------------------------------------------------##
# Calculate each cell of the crossed-condition matrix/One repetition of the simulation
doRep <- function(conds, parm)
{
  #Simulate data only once per iteration as data-generation parameters are not changing
  data_main <- try(simData(parm = parm))
  
  
  for (i in 1 : nrow(conds))
  {
    
    #Create a seperate data matrix to avoid possible problems
    data <- data_main
    
    
    #Save current values of the varying values
    parm$m <- conds[i, "m"]
    parm$mec <- conds[i, "mec"]
    parm$pm <- conds[i, "pm"]
    
    
    #Poke holes into the data
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
    
    
    #Compute FMIs 
    fmi <- try(fmi(data = impListdf,
                   method = "sat",
                   fewImps = TRUE
    ))
    
    
    #Save FMIs to list
    store[[i]] <- fmi
    
  }
  
  
  #Write list to disc
  saveRDS(store, 
          file = paste0("results/data_it_",a,".rds"))
  
  
  #Increase counter by 1
  a <- a + 1
  
}

##-------------------------------------------------------------------------------------------------------------------##


getTrueFMI <- function(conds, parm)
{
  
  #Create one dataset with N = 500.000
  data_main <- simDataInf(parm = parm)
  
  
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

adjustImpList <- function(impList, parm)
{
  out <- impList
  r <- sample(1:length(out), length(out)*parm$pm)
  tmp <- rep(FALSE, length(out))
  tmp[r] <- TRUE
  r <- tmp
  list <- list(r = r)
  impList2 <- out
  impList2[list$r] <- NULL
  impList2
}



##-------------------------------------------------------------------------------------------------------------------##
# # Calculate each cell of the crossed-condition matrix/One repetition of the simulation
# doRepMultipleData <- function(conds, parm)
# {
# 
#   
#   
#   for (i in 1 : nrow(conds))
#   {
#     
#     #Simulate data only once per iteration as data-generation parameters are not changing
#     data <- try(simData(parm = parm))
#     
#     
#     #Save current values of the varying values
#     parm$m <- conds[i, "m"]
#     parm$mec <- conds[i, "mec"]
#     parm$pm <- conds[i, "pm"]
#     
#     
#     #Create missingdata matrix
#     missingMatrix <- try(makeMissing(data = data,
#                                      mechanism = parm$mec,
#                                      pm = parm$pm,
#                                      preds = parm$Vecpred,
#                                      snr = NULL
#     ))
#     
#     
#     #Impute missing values
#     impData <- try(mice(data = missingMatrix,
#                         m = parm$m,
#                         method = "norm",
#                         print = FALSE
#     ))
#     
#     
#     #Save a list of imputed data sets
#     impListdf <- try(complete(data = impData,
#                               action = "all"
#     ))
#     
#     
#     #Compute FMIs 
#     fmi <- try(fmi(data = impListdf,
#                    method = "sat",
#                    fewImps = TRUE
#     ))
#     
#     
#     #Save FMIs to list
#     store[[i]] <- fmi
#     
#   }
#   
#   
#   #Write list to disc
#   saveRDS(store, 
#           file = paste0("results/data_it_",b,".rds"))
#   
#   
#   #Increase counter by 1
#   b <- b + 1
#   
# }