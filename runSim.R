### Title:    Main Simulation for the Bachelor Thesis
### Author:   Yuri T.C.A. Gaßner
### Created:  2021-06-30

##-------------------------------------------------------------------------------------------------------------------##

rm(list=ls(all=TRUE))
setwd("/home/itsme/BachelorThesisCode/")

## Libraries & Sources 

library("mice")
library("semTools")
library("mvtnorm") 
library("parallel")
library("pROC") 
library("rlecuyer")

source("init.R")
source("simFunctions.R")
source("simMissingness.R")

#Detects the amount of cores to later specify the maximum amount of cores to be used for parallelisation
cores <- detectCores()

##-------------------------------------------------------------------------------------------------------------------##
# Start Simulation
# Time it

time[[1]] <- Sys.time()

mclapply(X = 1:parm$iter,
         FUN = doIter,
         conds = conds,
         parm = parm,
         mc.cores = cores)

time[[2]] <- Sys.time()

saveRDS(time,
        file = paste0(directory,"Time.rds"))

# End Simulation
##-------------------------------------------------------------------------------------------------------------------##


