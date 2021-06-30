### Title:    Analysis of the main Simulation
### Author:   Yuri T.C.A. Gaßner
### Created:  2021-06-30


##-------------------------------------------------------------------------------------------------------------------##
rm(list=ls(all=TRUE))
# Analysis
# Read in RDS files
setwd("/home/itsme/BachelorThesisCode/Final_Results/")

source("../init.R")
source("../simFunctions.R")
source("../simMissingness.R")

temp <- list.files(pattern = "Rep")
results <- lapply(temp, readRDS)


avgMeanFmi  <- list(1:80)
avgVarFmi   <- list(1:80)
avgCovFmi   <- list(1:80)
devMeanFmi  <- list(1:80)
devVarFmi   <- list(1:80)
devCovFmi   <- list(1:80)
trueMeanFmi <- list(1:10)
trueVarFmi  <- list(1:10)
trueCovFmi  <- list(1:10)
trueMeanFmibig <- list(1:10)
trueVarFmibig  <- list(1:10)
trueCovFmibig  <- list(1:10)
#Random Vector
Benoni <- c(1:320)

#More Things for plots and graphs
AvgMeanFmiList <- list(c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500))


#MeanFMI
for(i in 1:length(avgMeanFmi[[1]])){
  
  for(u in 1:length(results)){
    
    Benoni[[u]] <- results[[u]][[i]][["Means"]][["fmi"]][[1]]
  }
  
  avgMeanFmi[[1]][[i]] <- mean(Benoni)
  devMeanFmi[[1]][[i]] <- SE(Benoni)
  
}




#VarFmi
for(i in 1:length(avgVarFmi[[1]])){
  
  for(u in 1:length(results)){
    
    Benoni[[u]] <- results[[u]][[i]][["Covariances"]][["fmi"]]["X1", "X1"]
  }
  
  avgVarFmi[[1]][[i]] <- mean(Benoni)
  devVarFmi[[1]][[i]] <- SE(Benoni)
  
}


#CovFmi
for(i in 1:length(avgCovFmi[[1]])){
  
  for(u in 1:length(results)){
    
    Benoni[[u]] <- results[[u]][[i]][["Covariances"]][["fmi"]]["X2", "X1"]
  }
  
  avgCovFmi[[1]][[i]] <- mean(Benoni)
  devCovFmi[[1]][[i]] <- SE(Benoni)
  
}



RuyLopez <- do.call(rbind, Map(data.frame,
                             avgMeanFmi = avgMeanFmi, avgVarFmi = avgVarFmi, avgCovFmi = avgCovFmi,
                             devMeanFmi = devMeanFmi, devVarFmi = devVarFmi, devCovFmi = devCovFmi))

FinalResults <- cbind(RuyLopez, conds)

saveRDS(FinalResults,
       file = paste0("../Results_without_TrueFmiwithrealSE.rds"))

##-------------------------------------------------------------------------------------------------------------------##

test <- getTrueFMI(condsFMI = condsFMI,
                   parm = parm)
trueFmiResultsbig <- readRDS("../truefmi_5mil.rds")


for(i in 1:length(trueCovFmi)){
  trueMeanFmibig[[i]] <- trueFmiResultsbig[[i]][["Means"]][["fmi"]][[1]]
  trueCovFmibig[[i]]  <- trueFmiResultsbig[[i]][["Covariances"]][["fmi"]]["X1", "X2"]
  trueVarFmibig[[i]]  <- trueFmiResultsbig[[i]][["Covariances"]][["fmi"]]["X1", "X1"]

}

ViennaGamebig <- do.call(rbind, Map(data.frame, trueMeanFmi = trueMeanFmibig,
                                 trueVarFmi = trueVarFmibig, trueCovFmi = trueCovFmibig))

TrueFmiResultsBig <- cbind(ViennaGamebig, condsFMI)

saveRDS(TrueFmiResultsBig,
        file = paste0("../TrueFmiResultsbig.rds"))


##-------------------------------------------------------------------------------------------------------------------##
# Relative Bias calculation

setwd("/home/itsme/BachelorThesisCode/Final_Results/")
results <- readRDS("Results_without_TrueFmiwithrealSE.rds")
trueFmi <- readRDS("TrueFmiResultsbig.rds")

difference <- results
  
difference <- difference[,-c(4:6)]
for(a in 1:3){
    
  #Difference for pm 90 MCAR
  for(i in 1:8){
    difference[i,a] <- (difference[i,a]-trueFmi[1,a])/trueFmi[1,a]
    }
  #Difference for pm 75 MCAR
  for(i in 9:16){
    difference[i,a] <- (difference[i,a]-trueFmi[2,a])/trueFmi[2,a]
  }
  #Diff pm 50 MCAR
  for(i in 17:24){
    difference[i,a] <- (difference[i,a]-trueFmi[3,a])/trueFmi[3,a]
  }
  #Diff pm 25 MCAR
  for(i in 25:32){
    difference[i,a] <- (difference[i,a]-trueFmi[4,a])/trueFmi[4,a]
  }
  #Diff pm 10 MCAR
  for(i in 33:40){
    difference[i,a] <- (difference[i,a]-trueFmi[5,a])/trueFmi[5,a]
  }
  
  #Diff pm 90 MAR
  for(i in 41:48){
    difference[i,a] <- (difference[i,a]-trueFmi[6,a])/trueFmi[6,a]
  }
  #Diff pm 75 MAR
  for(i in 49:56){
    difference[i,a] <- (difference[i,a]-trueFmi[7,a])/trueFmi[7,a]
  }
  #Diff pm 50 MAR
  for(i in 57:64){
    difference[i,a] <- (difference[i,a]-trueFmi[8,a])/trueFmi[8,a]
  }
  #Diff pm 25 MAR
  for(i in 65:72){
    difference[i,a] <- (difference[i,a]-trueFmi[9,a])/trueFmi[9,a]
  }
  #Diff pm 10 MAR
  for(i in 73:80){
    difference[i,a] <- (difference[i,a]-trueFmi[10,a])/trueFmi[10,a]
  }
}

diff <- cbind(format(round(difference[,c(1:3)], 2), nsmall = 2), difference[,c(4:5)])


saveRDS(difference,
        file = paste0("RelativeBiasFMI.rds"))

##-------------------------------------------------------------------------------------------------------------------##