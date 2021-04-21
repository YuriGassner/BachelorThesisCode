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

##################################################

for (i in 1 : nrow(conds))
{
  
  #Save current values of the varying values
  parm$m <- conds[i, "m"]
  parm$mec <- conds[i, "mec"]
  parm$pm <- conds[i, "pm"]
  
  
  #Simulate Data
  data <- try(simData(parm = parm,))
  
  
  #Create Missingdata Matrix
  missingMatrix <- try(makeMissing(data = data,
                                   mechanism = parm$mec,
                                   pm = parm$pm,
                                   preds = parm$Vecpred,
                                   snr = NULL
  ))
  
  
  # #Impose Missingness
  # missingdf <- data
  # missingdf[missingMatrix$r , 1] <- NA
  
  
  #Impute Missing Values
  impData <- try(mice(data = missingdf,
                      m = parm$m,
                      method = "norm",
                      print = FALSE
  ))
  
  
  #Save a List of Imputed Data Sets
  impListdf <- try(complete(data = impData,
                            action = "all"
  ))
  
  
  #Compute FMIs 
  fmi <- try(fmi(data = impListdf,
                 method = "sat",
                 fewImps = TRUE
  ))
  
  
  #Save FMIs to List
  store[[i]] <- fmi

}


#Write List to disc
saveRDS(store, 
        file = paste0("results/data_it_",a,".rds"))






#######
# Notes: Rewrite Loopstructure, Create "Condition Matrix" -> flattens out the loops
#        Label the conditions somehow?? 
#        
######
#  expand.grid function (vectors that are named. e.g. imp= imp) creates a data frame
#  rows of this condition matrix key out the repetitions
#  take the values from the condition matrix x <- conds[i, "rsq"]
#  cbind the condition matrix on the output
######
#  FMI with large N, poke holes into it and run semTools function
#  Create 10 mega datasets for Mechanism X PM conditions 
#  
######
# class(store)

# store[[7]] <- n5

# Name them

# Reference them each, e.g. in conditions varying pm  from 5:10 




## Store resulting data frame of each iteration

store_i <- vector("list", parm$iter)

###-----------------------------------------------------------------###
# Simulation Loop
start_time <- Sys.time()

for (a in 1:parm$iter) ### KML: See comment in Bastian's code about main-style
                       ### function.
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

                             
        #Calculate FMIs with semTools as 'lavaan' only does ML fmi
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

    ### KML: Save your output as RDS files.
  write.table(data.frame(store), 
              file     = paste0("results/data_iteration_",a,".csv")) ## SAVE AS RDS
  
}

end_time <- Sys.time()

# Current time per Iteration ~10min30sec

# End Simulation
###-----------------------------------------------------------------###
# Analysis
# Read in CSV files
# Average FMI estimate per condition
# Calculate SE and CI of FMI estimates per condition

