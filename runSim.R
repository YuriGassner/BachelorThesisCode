## Clean up the workspace
rm(list=ls(all=TRUE))

library("SimDesign")
library("lavaan")
set.seed(400)
source("init.R")
source("simFunctions.R")

###--------------------------###
#Calculate "True" FMI

simData(parm)
calcFMI


###--------------------------###
#Run Simulation

for (i in parm$iter)
{
  table <- list() #Create a data table for each iteration containing all relevant information
  
  simData(parms)
}