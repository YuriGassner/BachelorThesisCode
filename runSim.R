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


##-------------------------------------------------------------------------------------------------------------------##
# Start Simulation

start_time <- Sys.time()
time <- list()

for (f in 1:parm$iter)
{
  doRep(conds = conds,
        parm = parm,
        counter = f)
  
  time[[f]] <- Sys.time()
}




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


