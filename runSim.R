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
## Conditions will be sorted 1:50

store <- vector("list", length = parm$Ncond)

# Create Counter for FMI



# class(store)

# store[[7]] <- n5

# Name them

# Reference them each, e.g. in conditions varying pm  from 5:10 




## Store resulting data frame of each iteration

store_i <- vector("list", parm$iter)

###-----------------------------------------------------------------###
# Simulation Loop
start_time <- Sys.time()

for (a in 1:parm$iter)
{
  
  #Generate Data
  data <- try(simData(parm = parm,
                      infinity = "no"))
  
  #Set/Reset counter
  i = 1
  
  #Generate Missing Values
  for (b in 1:length(parm$mec))  #Loop for Mechanism mec
  {
    for (c in 1:length(parm$pm))  #Loop for Percent-Missing pm
    {
      #Create MissingnessMatrix with TRUE/FALSE values
      MissingMatrix <- try(makeMissing(data = data,
                                mechanism = parm$mec[[b]],
                                pm = parm$pm[[c]],
                                preds = parm$Vecpred,
                                snr = NULL
                                ))
      #Impose Missingness
      MissingDf <- data
      MissingDf[MissingMatrix$r,1] <- NA 
      
      #Impute Missing Values
      for (d in 1:length(parm$m))  #Loop for Missing Values m
      {
        #Impute Missing Data Sets
        ImpData <- try(mice(data = MissingDf,
                            m = parm$m[[d]],
                            method = "norm",
                            print = FALSE
                            ))
        
        #Save a List of Imputed Data Sets
        ImpListDf <- try(complete(data = ImpData,
                                  action = "all"
                                  ))
        
        #Calculate FMIs 
        fmi <- try(fmi(data = ImpListDf,
                       method = "sat",
                       fewImps = TRUE
                       ))
        
        #Save FMIs to list
        store[[i]] <- fmi
        i <- i + 1
      }
    }
  
  }
  
  #Save list to Location
  store_i[[a]] <- store
  write.table(data.frame(store), 
              file = paste0("/home/BachelorThesisCode/results/results_i",a,".md"))
  
}

end_time <- Sys.time()


# End Simulation
###-----------------------------------------------------------------###