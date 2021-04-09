## Clean up the workspace

rm(list=ls(all=TRUE))


## Libraries, Sources & Seed

library("SimDesign")
library("mice")
library("lavaan")
library("semTools")

source("init.R")
source("simFunctions.R")
source("simMissingness.R")

set.seed(541491)


## Storing the data as a list since the output of the analyses are multiple matrices
## Conditions will be sorted 1:20

store <- vector("list", length = parm$Ncond)


# store[[7]] <- n5

# Name them

# Reference them each, e.g. in conditions varying pm  from 5:10 




## Store resulting data frame of each iteration

store_i <- vector("list", parm$iter)

###-----------------------------------------------------------------###
# Simulation Loop

for (a in 1:parm$iter)
{
  #Generate Data
  
  data <- try(simData(parm = parm,
                      infinity = "no"))
  
  #Generate Missing Values
  for (b in 1:length(parm$mec))  #Loop for Mechanism
  {
    for (c in 1:length(parm$pm))  #Loop for Percent-Missing
    {
      #Create MissingnessMatrix with TRUE/FALSE values
      MissingMatrix <- try(makeMissing(data = data,
                                mechanism = parm$mec[[b]],
                                pm = parm$pm[[c]],
                                preds = parm$pred,
                                snr = NULL
                                ))
      #Impose Missingness
      MissingDf <- data
      MissingDf[MissingMatrix$r,1] <- NA 
      
      #Impute Missing Values
      for (d in 1:length(parm$m))
      {
        ImpData <- try(mice(data = MissingDf,
                            m = parm$m[[d]],
                            method = "norm",
                            print = FALSE
                            ))
        
        #Save a list of imputed data sets
        ImpListDf <- try(complete(data = ImpData,
                                  action = "all"
                                  ))
        
        #Calculate FMIs 
        fmi <- try(fmi(data = ImpListDf,
                       method = "sat",
                       fewImps = TRUE
                       ))
        
        #Save FMIs to list
        
      }
    }
  
  }
  
  #Save list to Location
  X=X
}

# End Simulation
###-----------------------------------------------------------------###