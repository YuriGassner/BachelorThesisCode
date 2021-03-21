## Clean up the workspace
rm(list=ls(all=TRUE))

library("SimDesign")
library("lavaan")
set.seed(400)
source("init.R")
source("simFunctions.R")

# data <- simData(parm, infinity = "no")
# datainfinite <- simData(parm, infinity = "yes")
# 
# mean(data$X1)
# mean(data$X2)
# mean(data$X3)
# mean(data$Y)
# mean(datainfinite$X1)
# mean(datainfinite$X2)
# mean(datainfinite$X3)
# mean(datainfinite$Y)
# 
# sd(data$X1)
# sd(data$X2)
# sd(data$X3)
# sd(data$Y)
# sd(datainfinite$X1)
# sd(datainfinite$X2)
# sd(datainfinite$X3)
# sd(datainfinite$Y)

#FMI Test
dataFinite <- simData(parm, infinity = "no")
fmiFinite <- calcFMI(data = data, infinity = "no")

###--------------------------###
#Calculate "True" FMI

dataInfinite <- simData(parm, infinity = "yes")
fmiInfinite <- calcFMI(data = datainfinite, infinity = "yes")
 

###----------------------------------------###

data1 <- data[,1:3] #Exclude Y

#Set up Model

data.cfa <- 'Y =~ X1 + X2 + X3' 
step1.cfa <- cfa(data.cfa, data = data1, missing = "fiml", std.lv = TRUE) 

se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa - Fit - se ; are the same values
cov.cfa <- step1.cfa@implied[["cov"]][[1]]
means.cfa <- step1.cfa@implied[["mean"]][[1]]

#Multiple model-implied cov by N/N-1, only worth doing with small N

cov.cfa <- cov.cfa*(parm$n/(parm$n-1))

###--------------------------###
#Run Simulation

for (i in parm$iter)
{
  table <- list() #Create a data table for each iteration containing all relevant information

  simData(parms)
}