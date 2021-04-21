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


#### STILL NEEDS: 
# CBIND the condition matrix on the output
# Fix counter for doRep 
# Test trueFMI calculation


##-------------------------------------------------------------------------------------------------------------------##
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

start_time <- Sys.time()

time <- vector("list", length = 10)
#Currently does one rep
for(i in 1:10)
{
  doRep(conds, parm)
  time[[i]] <- Sys.time()
}

time0_1 <- time[[1]] - start_time
time1_2 <- time[[2]] - time[[1]]
time2_3 <- time[[3]] - time[[2]]
time3_4 <- time[[4]] - time[[3]]
time4_5 <- time[[5]] - time[[4]]
time5_6 <- time[[6]] - time[[5]]
time6_7 <- time[[7]] - time[[6]]
time7_8 <- time[[8]] - time[[7]]
time8_9 <- time[[9]] - time[[8]]
time9_10 <- time[[10]] - time[[9]]
total_time <- time[[10]] - start_time

mean1_10 <- mean(time0_1, time1_2, time2_3, time3_4, time4_5, time5_6, time6_7, time7_8, time8_9, time9_10)
sd1_10 <- sd(time0_1, time1_2, time2_3, time3_4, time4_5, time5_6, time6_7, time7_8, time8_9, time9_10)


mean(time)
time

end_time <- Sys.time()

time <- end_time - start_time
time 
#Current best times for one iteration on one core:

#21th of April - doRep - 13min ; new doRep function, same time as with multiple for loops
#21th of April - doRep - 11.099min ; simulating the data only once, saved about 2min per iteration
#21th of April - doRepMultipleData - 11.62618min ; doRep function but generating data each time
#21th of April - both functions - 10.8min ; no other open programs aside from Terminal, doRep slightly faster, slightly
#                                           different results -> use only one simulated file per iteration
#21th of April - doRep 10iterations - slightly below 11min ; COUNTER DOES NOT WORK, OVERWRITES FILE 1


#Time needed for 1000 iterations on 4 cores (slightly more as not all CPUs can be utilised fully simultaneously)
total_time <- (time*1000)/4
total_time

# End Simulation
##-------------------------------------------------------------------------------------------------------------------##


