## Clean up the workspace
rm(list=ls(all=TRUE))


set.seed(400)


###--------------------------###
#Calculate "True" FMI

simData(parms)
calcFMI


###--------------------------###
#Run Simulation

for (i in parm$iter)
{
  table <- list() #Create a data table for each iteration containing all relevant information
  
  simData(parms)
}