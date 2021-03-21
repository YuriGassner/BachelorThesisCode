## Clean up the workspace
rm(list=ls(all=TRUE))

library("SimDesign")
library("lavaan")
set.seed(400)
source("init.R")
source("simFunctions.R")

data <- simData(parm, infinity = "no")
datainfinite <- simData(parm, infinity = "yes")

mean(data$X1)
mean(data$X2)
mean(data$X3)
mean(data$Y)
mean(datainfinite$X1)
mean(datainfinite$X2)
mean(datainfinite$X3)
mean(datainfinite$Y)

sd(data$X1)
sd(data$X2)
sd(data$X3)
sd(data$Y)
sd(datainfinite$X1)
sd(datainfinite$X2)
sd(datainfinite$X3)
sd(datainfinite$Y)

###--------------------------###
#Calculate "True" FMI

# data <- simData(parm)
# calcFMI


###--------------------------###
#Run Simulation
# 
# for (i in parm$iter)
# {
#   table <- list() #Create a data table for each iteration containing all relevant information
#   
#   simData(parms)
# }