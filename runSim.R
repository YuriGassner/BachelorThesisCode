## Clean up the workspace

rm(list=ls(all=TRUE))


## Libraries, Sources & Seed

library("mice")
# library("lavaan") ## Don't need that anymore as I am using the semTools package
library("semTools")
library("mvtnorm") ### KML: Missing dependency
library("parallel")

source("init.R")
source("simFunctions.R")
source("simMissingness.R")

set.seed(541491)  #Potentially still change this 


##-------------------------------------------------------------------------------------------------------------------##
# Start Simulation

start_time <- Sys.time()
time <- list()

#Parallelise the doIter function over multiple cores, potentially include the time stamps

mclapply(X = 1:parm$iter,
         FUN = doIter,
         conds = conds,
         parm = parm,
         mc.cores = 3)
  
  time[[f]] <- Sys.time()



# Can run Rscript without GUI with function Rscript (probably)
# parallelise with mclapply function (parallel package)
# if I run 1-100 iter and then 200-300 the seed stays the same -> results are the same

# total sample size = Nconds * iterations so iter = 500 instead of 1000 should still hold sufficient results, don't have to argue for it in the paper just mention it


#######
#Name the columns and rows instead of referring to it by numerical indices





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


