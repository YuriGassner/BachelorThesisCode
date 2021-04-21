rm(list=ls(all=TRUE))



###-------------------------------------###

library(lavaan)
math <- read.table("math.dat", na.strings = "NA")
colnames(math) <- c("MATH1", "MATH2", "MATH3", "MATH4")
#set up model
math.cfa <- ’math =~ MATH1 + MATH2 + MATH3 + MATH4’
step1.cfa <- cfa(math.cfa, data = math, missing = "fiml", std.lv = TRUE)
#save model standard errors
SE.cfa <- parameterEstimates(fit.cfa)$se
#get model-implied covariance matrix and means
cov.cfa <- fitted.values(step1.cfa)\$cov
means.cfa <- fitted.values(step1.cfa)\$mean
#multiply model-implied cov by N/N-1
#note that this is optional and perhaps only worth doing with small N
cov.cfa <- cov.cfa*(494/493)
#run the model using model-implied cov. matrix and means as input
step2.cfa <- cfa(math.cfa, sample.cov = cov.cfa, sample.mean =
                   means.cfa, sample.nobs = 494, std.lv = TRUE,
                 meanstructure = TRUE, information = "observed")
#get standard errors
SE.step2.cfa <- parameterEstimates(step2.cfa)$se
#compute vector of fraction of missing information estimates
FMI <- 1-(SE.step2.cfa^2/SE.cfa^2)









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

#Sepcify row and columnnames according to model

rownames(cov.cfa) <- c("X1","X2","X3")
colnames(cov.cfa) <- c("X1","X2","X3")

#run the model with model-implied cov matrix and means as input
step2.cfa <- cfa(data.cfa,
                 sample.cov = cov.cfa,
                 sample.mean = means.cfa,
                 sample.nobs = parm$n, 
                 std.lv = TRUE,
                 meanstructure = TRUE,
                 information = "observed")

se.step2.cfa <- parameterEstimates(step2.cfa)$se

#Compute vector of fraction of missing information estimates

fmi <- 1-(se.step2.cfa^2/se.cfa^2)
fmi




##############################################################################################





nPred <- parm$pred
sigma <- matrix(parm$cov, parm$pred, parm$pred)
diag(sigma) <- 1.0

#Generate data
X <- rmvnorm(n = parm$n, mean = rep(0, parm$pred), sigma = sigma)

data <- data.frame(X)

data





###########################################################################################
a <- c(1,1)

# Make Missing
testmar <- makeMissing(data = data,
                        mechanism = "MAR",
                        pm = 0.2,
                        preds = a,
                        snr = parm$snr)

datamar <- data
datamar[testmar$r,1] <- NA

# Impute Data with mice

impsets <- mice(data = datamar,
                m = 5,
                method = "norm",
                print = FALSE)

# Save a list of imputed data sets
dfimp <- complete(data = impsets,
                  action = "all")
# Calculate fmi with those data sets
impFmi <- fmi(data = dfimp,
              fewImps = TRUE)

n500 <- impFmi
n5 <- impFmi
n500pm02 <- impFmi
n5pm02 <- impFmi
###########################################################################################







res <- cor(data)
data.cfa <- data



###########################################Test fmi


dataX2 <- data.frame(data[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
#column data.frames automatically into numeric vectors

colnames(dataX2) <- c("X2") #Name column

#Set up Model

data.cfa <- 'X1 =~ X2' 
step1.cfa <- cfa(data.cfa, data = dataX2, missing = "fiml", std.lv = TRUE) 

se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa -> Fit -> se ; are the same values
cov.cfa <- step1.cfa@implied[["cov"]][[1]]
means.cfa <- step1.cfa@implied[["mean"]][[1]]

#Multiple model-implied cov by N/N-1, only worth doing with small N

cov.cfa <- cov.cfa*(parm$n/(parm$n-1))

#Sepcify row and columnnames according to model

rownames(cov.cfa) <- c("X2")
colnames(cov.cfa) <- c("X2")

#run the model with model-implied cov matrix and means as input
step2.cfa <- cfa(data.cfa,
                 sample.cov = cov.cfa,
                 sample.mean = means.cfa,
                 sample.nobs = parm$n, 
                 std.lv = TRUE,
                 meanstructure = TRUE,
                 information = "observed")

se.step2.cfa <- parameterEstimates(step2.cfa)$se

#Compute vector of fraction of missing information estimates

fmi <- 1-(se.step2.cfa^2/se.cfa^2)
fmi




library("Amelia")



#############################################################

#FMI Test
dataFinite <- simData(parm, infinity = "no")
fmiFinite <- calcFMI(data = data, infinity = "no")

###--------------------------###
#Calculate "True" FMI

dataInfinite <- simData(parm, infinity = "yes")
fmiInfinite <- calcFMI(data = datainfinite, infinity = "yes")


###----------------------------------------###




# Calculate true FMI

dataInMcar <- simData(parm = parm,
                  infinity = "yes")
dataInMar <- dataInMcar

#Make Missing MCAR
dataholesMcar <- makeMissing(data = dataInMcar,
                             mechanism = "MCAR",
                             pm = 0.5,
                             preds = parm$Vecpred,
                             snr = parm$snr
                             )
MisInDataMcar <- dataInMcar
MisInDataMcar[dataholesMcar$r,1] <- NA

#Make Missing MAR
dataholesMar <- makeMissing(data = dataInMar,
                            mechanism = "MAR",
                            pm = 0.5,
                            preds = parm$Vecpred,
                            snr = parm$snr
                            )

MisInDataMar <- dataInMar
MisInDataMar[dataholesMar$r,1] <- NA

data4 <- simData(parm = parm, infinity = "no")
ve <- sd(data4$X1)
data4holes <- makeMissing(data = data4, mechanism = "MCAR", pm = 0.5, preds = parm$Vecpred, snr = parm$snr)
data4mis <- data4
data4mis[data4holes$r,1] <- NA

dataX1 <- data.frame(data[,1])
colnames(dataX1) <- c("X2")

data.cfa <- 'X1 =~ X2' 
step1.cfa <- cfa(data.cfa, data = dataX1, missing = "fiml", std.lv = TRUE) 


se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa - Fit - se ; are the same values

se.real <- c(ve,ve,ve,ve,ve)

#Calculate FMI
fmi <- 1-(se.real^2/se.cfa^2)
fmi

cov.cfa <- step1.cfa@implied[["cov"]][[1]]
means.cfa <- step1.cfa@implied[["mean"]][[1]]

#Multiple model-implied cov by N/N-1, only worth doing with small N

cov.cfa <- cov.cfa*(parm$Nfmi/(parm$Nfmi-1))

#Sepcify row and columnnames according to model

rownames(cov.cfa) <- c("X2")
colnames(cov.cfa) <- c("X2")

#run the model with model-implied cov matrix and means as input
step2.cfa <- cfa(data.cfa,
                 sample.cov = cov.cfa,
                 sample.mean = means.cfa,
                 sample.nobs = parm$Nfmi, 
                 std.lv = TRUE,
                 meanstructure = TRUE,
                 information = "observed")

se.step2.cfa <- parameterEstimates(step2.cfa)$se

#Compute vector of fraction of missing information estimates

fmi <- 1-(se.step2.cfa^2/se.cfa^2)
fmi

###------------------------###
#FMI

#X1 is the object of interest here, as missingness only occurs in X1
dataX1 <- data.frame(data4mis[,1])

colnames(dataX1) <- c("X2")

#Set up Model
data.cfa <- 'X1 =~ X2'
step1.cfa <- cfa(data.cfa,
                 data = dataX1,
                 missing = "fiml",
                 std.lv = TRUE,
                 estimator = "ML")
se.cfa <- parameterEstimates(step1.cfa)$se

#"Calculate" real SE
se.real <- c(ve,ve,ve,ve,ve)

#Calculate FMI
fmi <- 1-(se.real^2/se.cfa^2)
fmi
  





datax <- simData(parm = parm,
                 infinity = "yes")
sd(datax$X1)
dataX2 <- data.frame(data[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
#column data.frames automatically into numeric vectors

colnames(dataX2) <- c("X2") #Name column

#Set up Model

data.cfa <- 'X1 =~ X2' 
step1.cfa <- cfa(data.cfa, data = dataX2, missing = "ml.x", std.lv = TRUE) 

se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa - Fit - se ; are the same values

#
se.real <- c(1,NA,NA,1,NA)  
#
# 
# cov.cfa <- step1.cfa@implied[["cov"]][[1]]

#

#

#Multiple model-implied cov by N/N-1, only worth doing with small N
# 
# cov.cfa <- cov.cfa*(parm$Nfmi/(parm$Nfmi-1))

#Sepcify row and columnnames according to model
# 
rownames(cov.cfa) <- c("X2")
colnames(cov.cfa) <- c("X2")




# #run the model with model-implied cov matrix and means as input
# step2.cfa <- cfa(data.cfa,
#                  sample.cov = cov.cfa,
#                  sample.mean = means.real,
#                  sample.nobs = parm$Nfmi, 
#                  std.lv = TRUE,
#                  meanstructure = TRUE,
#                  information = "observed")
# 
# se.step2.cfa <- parameterEstimates(step2.cfa)$se

#Compute vector of fraction of missing information estimates

fmi <- 1-(se.real^2/se.cfa^2)
fmi

#####################################################################################################################################################################################
Test <- complete(data = ImpData,
                 method ="broad")

dataX1 <- data.frame(Test[,1]) #X2 as a predictor of the missingness in X1; Has to be a data.frame, R converts single
#column data.frames automatically into numeric vectors

colnames(dataX1) <- c("X1") #Name column

#Set up Model

data.cfa <- 'X2 =~ X1' 
step1.cfa <- cfa(data.cfa, data = dataX1, missing = "fiml", std.lv = TRUE) 

se.cfa <- parameterEstimates(step1.cfa)$se #step1.cfa - Fit - se ; are the same values
cov.cfa <- step1.cfa@implied[["cov"]][[1]]
means.cfa <- step1.cfa@implied[["mean"]][[1]]

#Multiple model-implied cov by N/N-1, only worth doing with small N

cov.cfa <- cov.cfa*(parm$n/(parm$n-1))

#Sepcify row and columnnames according to model

rownames(cov.cfa) <- c("X1")
colnames(cov.cfa) <- c("X1")


#run the model with model-implied cov matrix and means as input
step2.cfa <- cfa(data.cfa,
                 sample.cov = cov.cfa,
                 sample.mean = means.cfa,
                 sample.nobs = parm$n, 
                 std.lv = TRUE,
                 meanstructure = TRUE,
                 information = "observed")

se.step2.cfa <- parameterEstimates(step2.cfa)$se

#Compute vector of fraction of missing information estimates

fmi <- 1 - (se.step2.cfa^2/se.cfa^2)
fmi























##################################################

##################################################

for (i in 1 : nrow(conds))
{
  
  #Save current values of the varying values
  parm$m <- conds[i, "m"]
  parm$mec <- conds[i, "mec"]
  parm$pm <- conds[i, "pm"]
  
  
  #Simulate Data
  data <- try(simData(parm = parm,))
  
  
  #Create Missingdata Matrix
  missingMatrix <- try(makeMissing(data = data,
                                   mechanism = parm$mec,
                                   pm = parm$pm,
                                   preds = parm$Vecpred,
                                   snr = NULL
  ))
  
  
  # #Impose Missingness
  # missingdf <- data
  # missingdf[missingMatrix$r , 1] <- NA
  
  
  #Impute Missing Values
  impData <- try(mice(data = missingdf,
                      m = parm$m,
                      method = "norm",
                      print = FALSE
  ))
  
  
  #Save a List of Imputed Data Sets
  impListdf <- try(complete(data = impData,
                            action = "all"
  ))
  
  
  #Compute FMIs 
  fmi <- try(fmi(data = impListdf,
                 method = "sat",
                 fewImps = TRUE
  ))
  
  
  #Save FMIs to List
  store[[i]] <- fmi
  
}


#Write List to disc
saveRDS(store, 
        file = paste0("results/data_it_",a,".rds"))









data <-  simData(parm = parm)

missingdf <- makeMissing(data = data,
                         mechanism = "MCAR",
                         pm = 0.5,
                         preds = parm$Vecpred,
                         snr = NULL)







start_time <- Sys.time()
doRep(conds = conds, parm = parm)
end_time <- Sys.time()




##-------------------------------------------------------------------------------------------------------------------##
#True FMI calculation due to asymptotic approximation

#Create one dataset with N = 500.000
data_main <- simDataInf(parm = parm)
storage <- vector("list", length = nrow(conds))

for(f in 1 : nrow(conds))
{
  
  #Save current values of the varying values
  parm$m <- conds[i, "m"]
  parm$mec <- conds[i, "mec"]
  parm$pm <- conds[i, "pm"]
  
  
  #Keep reusing the same previously created dataset
  data <- data_main
  
  
  #Poke holes into the data set
  MissingData <- try(makeMissing(data = data,
                                 mechanism = parm$mec,
                                 pm = parm$pm,
                                 preds = parm$Vecpred,
                                 snr = NULL
                                 ))
  
  
  #Impute missing values via FIML and calculate FMI
  fmi <- try(fmi(data = MissingData,
                 method = "sat",
                 ### POTENTIALLY EXCLUDE X2 AS THERE ARE NO MISSING VALUES? but also maybe not
                 ))
  
  
  #Save FMIs to list
  storage[[f]] <- fmi
  
}

saveRDS(storage,
        file = paste0("results/data_trueFMI",f,".rds"))



getTrueFMI <- function(conds, parm)
{
  
  #Create one dataset with N = 500.000
  data_main <- simDataInf(parm = parm)
  storage <- vector("list", length = nrow(conds))
  
  
  for(f in 1 : nrow(conds))
  {
    
    #Save current values of the varying values
    parm$m <- conds[i, "m"]
    parm$mec <- conds[i, "mec"]
    parm$pm <- conds[i, "pm"]
    
    
    #Keep reusing the same previously created dataset
    data <- data_main
    
    
    #Poke holes into the data set
    MissingData <- try(makeMissing(data = data,
                                   mechanism = parm$mec,
                                   pm = parm$pm,
                                   preds = parm$Vecpred,
                                   snr = NULL
    ))
    
    
    #Impute missing values via FIML and calculate FMI
    fmi <- try(fmi(data = MissingData,
                   method = "sat",
                   ### POTENTIALLY EXCLUDE X2 AS THERE ARE NO MISSING VALUES? but also maybe not
    ))
    
    
    #Save FMIs to list
    storage[[f]] <- fmi
    
    
  }
  
  
  saveRDS(storage,
          file = paste0("results/data_trueFMI",f,".rds"))
  
  
}

