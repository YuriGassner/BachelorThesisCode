###-----------------------------------------------------------------###
# Analysis
# Read in RDS files
rm(list=ls(all=TRUE))
setwd("/home/itsme/BachelorThesisCode/results")
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
Benoni <- c(1:length(results))

#More Things for plots and graphs
AvgMeanFmiList <- list(c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),
                       c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500),c(1:500))

#Things for plots and graphs
for(b in 1:length(AvgMeanFmiList)){
  
  for(a in 1:length(results)){
    AvgMeanFmiList[[b]][[a]] <- results[[a]][[b]][["Means"]][["fmi"]][[1]]
  }
}


saveRDS(AvgMeanFmiList,
        file = paste0("../AvgMeanFmiList.rds"))

#MeanFMI
for(i in 1:length(avgMeanFmi)){
  
  for(u in 1:length(results)){
    
    new[[u]] <- results[[u]][[i]][["Means"]][["fmi"]][[1]]
  }
  
  avgMeanFmi[[i]] <- mean(new)
  devMeanFmi[[i]] <- sd(new)
  
}


#VarFmi
for(i in 1:length(avgVarFmi)){
  
  for(u in 1:length(results)){
    
    new[[u]] <- results[[u]][[i]][["Covariances"]][["fmi"]]["X1", "X1"]
  }
  
  avgVarFmi[[i]] <- mean(new)
  devVarFmi[[i]] <- sd(new)
  
}


#CovFmi
for(i in 1:length(avgCovFmi)){
  
  for(u in 1:length(results)){
    
    new[[u]] <- results[[u]][[i]][["Covariances"]][["fmi"]]["X2", "X1"]
  }
  
  avgCovFmi[[i]] <- mean(new)
  devCovFmi[[i]] <- sd(new)
  
}



RuyLopez <- do.call(rbind, Map(data.frame,
                             avgMeanFmi = avgMeanFmi, avgVarFmi = avgVarFmi, avgCovFmi = avgCovFmi,
                             devMeanFmi = devMeanFmi, devVarFmi = devVarFmi, devCovFmi = devCovFmi))

FinalResults <- cbind(RuyLopez, conds)

saveRDS(FinalResults,
       file = paste0("../Results_without_TrueFmiThiloYuri.rds"))

########################################################################################################


test <- getTrueFMI(condsFMI = condsFMI,
                   parm = parm)
trueFmiResultsbig <- readRDS("../truefmi_5mil.rds")
trueFmiResults <- readRDS("../data_trueFMI.rds")


for(i in 1:length(trueCovFmi)){
  trueMeanFmibig[[i]] <- trueFmiResultsbig[[i]][["Means"]][["fmi"]][[1]]
  trueCovFmibig[[i]]  <- trueFmiResultsbig[[i]][["Covariances"]][["fmi"]]["X1", "X2"]
  trueVarFmibig[[i]]  <- trueFmiResultsbig[[i]][["Covariances"]][["fmi"]]["X1", "X1"]

}

for(i in 1:length(trueCovFmi)){
  trueMeanFmi[[i]] <- trueFmiResults[[i]][["Means"]][["fmi"]][[1]]
  trueCovFmi[[i]]  <- trueFmiResults[[i]][["Covariances"]][["fmi"]]["X1", "X2"]
  trueVarFmi[[i]]  <- trueFmiResults[[i]][["Covariances"]][["fmi"]]["X1", "X1"]

}


ViennaGame <- do.call(rbind, Map(data.frame, trueMeanFmi = trueMeanFmi,
                                 trueVarFmi = trueVarFmi, trueCovFmi = trueCovFmi))
ViennaGamebig <- do.call(rbind, Map(data.frame, trueMeanFmi = trueMeanFmibig,
                                 trueVarFmi = trueVarFmibig, trueCovFmi = trueCovFmibig))

TrueFmiResultsBig <- cbind(ViennaGamebig, condsFMI)
TrueFmiResults <- cbind(ViennaGame, condsFMI)
view(TrueFmiResults)

saveRDS(TrueFmiResultsBig,
        file = paste0("../TrueFmiResultsbig.rds"))




