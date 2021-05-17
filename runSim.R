## Clean up the workspace

rm(list=ls(all=TRUE))
setwd("/home/itsme/BachelorThesisCode")

## Libraries, Sources & Seed

install.packages("mice","semTools","mvtnorm","parallel","pROC","rlecuyer")

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
time <- list()

time[[1]] <- Sys.time()

#Parallelise the doIter function over multiple cores, potentially include the time stamps


mclapply(X = 1:parm$iter,
         FUN = doIter,
         conds = conds,
         parm = parm,
         mc.cores = cores)
#it uses one core per iteration!! so do these things in multiples of 4
#Need about 30hours on average
#Do 168-168-164 iterations -> 500
#Current time is about 14min
#168iterations would need about 10hours

time[[2]] <- Sys.time()

saveRDS(time,
        file = paste0(directory,a,"Time.rds"))
# total sample size = Nconds * iterations so iter = 500 instead of 1000 should still hold sufficient results, don't have to argue for it in the paper just mention it


# getTrueFMI(conds, parm)
# start_time <- 13.01
# end_time <- Sys.time()
# #5min for one run

##-------------------------------------------------------------------------------------------------------------------##
#Notes & Testruns, STILL HAS TO BE CLEANED UP





#Notes
#  Rewrite Loopstructure, Create "Condition Matrix" -> flattens out the loops
#  Label the conditions somehow?? 
#        
######
#  expand.grid function (vectors that are named. e.g. imp= imp) creates a data frame
#  rows of this condition matrix key out the repetitions
#  take the values from the condition matrix x <- conds[i, "rsq"]
#  cbind the condition matrix on the output
######
#  FMI with large N, poke holes into it and run semTools function on empty data set
#  Create 10 mega datasets for Mechanism X PM conditions 
#  Does the a + 1 counter still work for parallelisation
##-------------------------------------------------------------------------------------------------------------------##
#Main Simulation


#Rep2 best time: 9.9min
#Current best times for one iteration on one core:

#21th of April - doRep - 13min ; new doRep function, same time as with multiple for loops
#21th of April - doRep - 11.099min ; simulating the data only once, saved about 2min per iteration
#21th of April - doRepMultipleData - 11.62618min ; doRep function but generating data each time
#21th of April - both functions - 10.8min ; no other open programs aside from Terminal, doRep slightly faster, slightly
#                                           different results -> use only one simulated file per iteration
#21th of April - doRep 10iterations - slightly below 11min ; COUNTER DOES NOT WORK, OVERWRITES FILE 1


# End Simulation
##-------------------------------------------------------------------------------------------------------------------##


