### Title:    Tables & Graphs depicting the Results of the Main Simulation
### Author:   Yuri T.C.A. Gaßner
### Created:  2021-06-30


##-------------------------------------------------------------------------------------------------------------------##
rm(list=ls(all=TRUE))
setwd("/home/itsme/BachelorThesisCode/Final_Results")
relbias <- readRDS("RelativeBiasFMI.rds")

source("../init.R")
library("plot3D")
library("formattable")

x <- c(5, 10, 25, 50, 100, 200, 250, 500)
y <- c(0.1, 0.25, 0.5, 0.75, 0.9)
##-------------------------------------------------------------------------------------------------------------------##
res <- relbias

test <- cbind(abs(res[,c(1:3)]), res[,c(4:5)])
dd <- matrix(ncol = 5, nrow = 8)

rownames(dd) <- c("m = 5", "m = 10", "m = 25", "m = 50", "m = 100", "m = 200", "m = 250", "m = 500")
colnames(dd) <- c("pm = 0.1", "pm = 0.25", "pm = 0.5", "pm = 0.75", "pm = 0.9")

vec5   <- c(res[[1]][[40]], res[[1]][[32]], res[[1]][[24]], res[[1]][[16]], res[[1]][[8]])
vec10  <- c(res[[1]][[39]], res[[1]][[31]], res[[1]][[23]], res[[1]][[15]], res[[1]][[7]])
vec25  <- c(res[[1]][[38]], res[[1]][[30]], res[[1]][[22]], res[[1]][[14]], res[[1]][[6]])
vec50  <- c(res[[1]][[37]], res[[1]][[29]], res[[1]][[21]], res[[1]][[13]], res[[1]][[5]])
vec100 <- c(res[[1]][[36]], res[[1]][[28]], res[[1]][[20]], res[[1]][[12]], res[[1]][[4]])
vec200 <- c(res[[1]][[35]], res[[1]][[27]], res[[1]][[19]], res[[1]][[11]], res[[1]][[3]])
vec250 <- c(res[[1]][[34]], res[[1]][[26]], res[[1]][[18]], res[[1]][[10]], res[[1]][[2]])
vec500 <- c(res[[1]][[33]], res[[1]][[25]], res[[1]][[17]], res[[1]][[9]], res[[1]][[1]])

dd[1, ] <- vec5
dd[2, ] <- vec10
dd[3, ] <- vec25
dd[4, ] <- vec50
dd[5, ] <- vec100
dd[6, ] <- vec200
dd[7, ] <- vec250
dd[8, ] <- vec500

z <- dd
# hist3D(x = x, y = y, z = z, zlim = c(0,50), theta = 40, phi = 40, axes = TRUE, nticks = 8, label = FALSE,
#        ticktype = "detailed", space = 0.5, lighting = TRUE, light = "diffuse", shade = 0.5, xlab = "deineMutter")

hist3D(z = z,
       axes = TRUE,
       label = TRUE,
       lighting = TRUE,
       space = 0.1,
       theta = 160,
       phi = 21,
       xlab = "m",
       ylab = "pm",
       zlab = "Relative Bias",
       expand = 0.5,
       colkey = list(side = 1, length = 0.5),
       zlim = c(0.0,0.20),
       ticktype = "simple",
       clab = "Relative Bias",
)

##-------------------------------------------------------------------------------------------------------------------##
###Diff VarFMI MCAR

dd2 <- matrix(ncol = 5, nrow = 8)
rownames(dd2) <- c("m = 5", "m = 10", "m = 25", "m = 50", "m = 100", "m = 200", "m = 250", "m = 500")
colnames(dd2) <- c("pm = 0.1", "pm = 0.25", "pm = 0.5", "pm = 0.75", "pm = 0.9")

vec5   <- c(res[[2]][[40]], res[[2]][[32]], res[[2]][[24]], res[[2]][[16]], res[[2]][[8]])
vec10  <- c(res[[2]][[39]], res[[2]][[31]], res[[2]][[23]], res[[2]][[15]], res[[2]][[7]])
vec25  <- c(res[[2]][[38]], res[[2]][[30]], res[[2]][[22]], res[[2]][[14]], res[[2]][[6]])
vec50  <- c(res[[2]][[37]], res[[2]][[29]], res[[2]][[21]], res[[2]][[13]], res[[2]][[5]])
vec100 <- c(res[[2]][[36]], res[[2]][[28]], res[[2]][[20]], res[[2]][[12]], res[[2]][[4]])
vec200 <- c(res[[2]][[35]], res[[2]][[27]], res[[2]][[19]], res[[2]][[11]], res[[2]][[3]])
vec250 <- c(res[[2]][[34]], res[[2]][[26]], res[[2]][[18]], res[[2]][[10]], res[[2]][[2]])
vec500 <- c(res[[2]][[33]], res[[2]][[25]], res[[2]][[17]], res[[2]][[9]], res[[2]][[1]])

dd2[1, ] <- vec5
dd2[2, ] <- vec10
dd2[3, ] <- vec25
dd2[4, ] <- vec50
dd2[5, ] <- vec100
dd2[6, ] <- vec200
dd2[7, ] <- vec250
dd2[8, ] <- vec500

z2 <- dd2

hist3D(z = z2,
       axes = TRUE,
       label = TRUE,
       lighting = TRUE,
       space = 0.1,
       theta = 160,
       phi = 21,
       xlab = "m",
       ylab = "pm",
       zlab = "Relative Bias",
       expand = 0.5,
       colkey = list(side = 1, length = 0.5),
       zlim = c(0.0,0.25),
       ticktype = "simple",
       clab = "Relative Bias",
)
##-------------------------------------------------------------------------------------------------------------------##
###Diff CoefFMI MCAR


dd3 <- matrix(ncol = 5, nrow = 8)
rownames(dd3) <- c("m = 5", "m = 10", "m = 25", "m = 50", "m = 100", "m = 200", "m = 250", "m = 500")
colnames(dd3) <- c("pm = 0.1", "pm = 0.25", "pm = 0.5", "pm = 0.75", "pm = 0.9")

vec5   <- c(res[[3]][[40]], res[[3]][[32]], res[[3]][[24]], res[[3]][[16]], res[[3]][[8]])
vec10  <- c(res[[3]][[39]], res[[3]][[31]], res[[3]][[23]], res[[3]][[15]], res[[3]][[7]])
vec25  <- c(res[[3]][[38]], res[[3]][[30]], res[[3]][[22]], res[[3]][[14]], res[[3]][[6]])
vec50  <- c(res[[3]][[37]], res[[3]][[29]], res[[3]][[21]], res[[3]][[13]], res[[3]][[5]])
vec100 <- c(res[[3]][[36]], res[[3]][[28]], res[[3]][[20]], res[[3]][[12]], res[[3]][[4]])
vec200 <- c(res[[3]][[35]], res[[3]][[27]], res[[3]][[19]], res[[3]][[11]], res[[3]][[3]])
vec250 <- c(res[[3]][[34]], res[[3]][[26]], res[[3]][[18]], res[[3]][[10]], res[[3]][[2]])
vec500 <- c(res[[3]][[33]], res[[3]][[25]], res[[3]][[17]], res[[3]][[9]], res[[3]][[1]])

dd3[1, ] <- vec5
dd3[2, ] <- vec10
dd3[3, ] <- vec25
dd3[4, ] <- vec50
dd3[5, ] <- vec100
dd3[6, ] <- vec200
dd3[7, ] <- vec250
dd3[8, ] <- vec500

z3 <- dd3

hist3D(z = z3,
       axes = TRUE,
       label = TRUE,
       lighting = TRUE,
       space = 0.1,
       theta = 160,
       phi = 21,
       xlab = "m",
       ylab = "pm",
       zlab = "Relative Bias",
       expand = 0.5,
       colkey = list(side = 1, length = 0.5),
       zlim = c(0.0,0.25),
       ticktype = "simple",
       clab = "Relative Bias",
)
##-------------------------------------------------------------------------------------------------------------------##
###Diff MeanFMI MAR

dd4 <- matrix(ncol = 5, nrow = 8)
rownames(dd4) <- c("m = 5", "m = 10", "m = 25", "m = 50", "m = 100", "m = 200", "m = 250", "m = 500")
colnames(dd4) <- c("pm = 0.1", "pm = 0.25", "pm = 0.5", "pm = 0.75", "pm = 0.9")

vec5   <- c(res[[1]][[80]], res[[1]][[72]], res[[1]][[64]], res[[1]][[56]], res[[1]][[48]])
vec10  <- c(res[[1]][[79]], res[[1]][[71]], res[[1]][[63]], res[[1]][[55]], res[[1]][[47]])
vec25  <- c(res[[1]][[78]], res[[1]][[70]], res[[1]][[62]], res[[1]][[54]], res[[1]][[46]])
vec50  <- c(res[[1]][[77]], res[[1]][[69]], res[[1]][[61]], res[[1]][[53]], res[[1]][[45]])
vec100 <- c(res[[1]][[76]], res[[1]][[68]], res[[1]][[60]], res[[1]][[52]], res[[1]][[44]])
vec200 <- c(res[[1]][[75]], res[[1]][[67]], res[[1]][[59]], res[[1]][[51]], res[[1]][[43]])
vec250 <- c(res[[1]][[74]], res[[1]][[66]], res[[1]][[58]], res[[1]][[50]], res[[1]][[42]])
vec500 <- c(res[[1]][[73]], res[[1]][[65]], res[[1]][[57]], res[[1]][[49]], res[[1]][[41]])

dd4[1, ] <- vec5
dd4[2, ] <- vec10
dd4[3, ] <- vec25
dd4[4, ] <- vec50
dd4[5, ] <- vec100
dd4[6, ] <- vec200
dd4[7, ] <- vec250
dd4[8, ] <- vec500

z4 <- dd4

hist3D(z = z4,
       axes = TRUE,
       label = TRUE,
       lighting = TRUE,
       space = 0.1,
       theta = 160,
       phi = 21,
       xlab = "m",
       ylab = "pm",
       zlab = "Relative Bias",
       expand = 0.5,
       colkey = list(side = 1, length = 0.5),
       zlim = c(0.0,0.15),
       ticktype = "simple",
       clab = "Relative Bias",
)

##-------------------------------------------------------------------------------------------------------------------##
###Diff VarFMI MAR

dd5 <- matrix(ncol = 5, nrow = 8)
rownames(dd5) <- c("m = 5", "m = 10", "m = 25", "m = 50", "m = 100", "m = 200", "m = 250", "m = 500")
colnames(dd5) <- c("pm = 0.1", "pm = 0.25", "pm = 0.5", "pm = 0.75", "pm = 0.9")

vec5   <- c(res[[2]][[80]], res[[2]][[72]], res[[2]][[64]], res[[2]][[56]], res[[2]][[48]])
vec10  <- c(res[[2]][[79]], res[[2]][[71]], res[[2]][[63]], res[[2]][[55]], res[[2]][[47]])
vec25  <- c(res[[2]][[78]], res[[2]][[70]], res[[2]][[62]], res[[2]][[54]], res[[2]][[46]])
vec50  <- c(res[[2]][[77]], res[[2]][[69]], res[[2]][[61]], res[[2]][[53]], res[[2]][[45]])
vec100 <- c(res[[2]][[76]], res[[2]][[68]], res[[2]][[60]], res[[2]][[52]], res[[2]][[44]])
vec200 <- c(res[[2]][[75]], res[[2]][[67]], res[[2]][[59]], res[[2]][[51]], res[[2]][[43]])
vec250 <- c(res[[2]][[74]], res[[2]][[66]], res[[2]][[58]], res[[2]][[50]], res[[2]][[42]])
vec500 <- c(res[[2]][[73]], res[[2]][[65]], res[[2]][[57]], res[[2]][[49]], res[[2]][[41]])

dd5[1, ] <- vec5
dd5[2, ] <- vec10
dd5[3, ] <- vec25
dd5[4, ] <- vec50
dd5[5, ] <- vec100
dd5[6, ] <- vec200
dd5[7, ] <- vec250
dd5[8, ] <- vec500

z5 <- dd5

hist3D(z = z5,
       axes = TRUE,
       label = TRUE,
       lighting = TRUE,
       space = 0.1,
       theta = 160,
       phi = 21,
       xlab = "m",
       ylab = "pm",
       zlab = "Relative Bias",
       expand = 0.5,
       colkey = list(side = 1, length = 0.5),
       zlim = c(0.0,0.25),
       ticktype = "simple",
       clab = "Relative Bias",
)
##-------------------------------------------------------------------------------------------------------------------##
###Diff CoefFMI MAR

dd6 <- matrix(ncol = 5, nrow = 8)
rownames(dd6) <- c("m = 5", "m = 10", "m = 25", "m = 50", "m = 100", "m = 200", "m = 250", "m = 500")
colnames(dd6) <- c("pm = 0.1", "pm = 0.25", "pm = 0.5", "pm = 0.75", "pm = 0.9")

vec5   <- c(res[[3]][[80]], res[[3]][[72]], res[[3]][[64]], res[[3]][[56]], res[[3]][[48]])
vec10  <- c(res[[3]][[79]], res[[3]][[71]], res[[3]][[63]], res[[3]][[55]], res[[3]][[47]])
vec25  <- c(res[[3]][[78]], res[[3]][[70]], res[[3]][[62]], res[[3]][[54]], res[[3]][[46]])
vec50  <- c(res[[3]][[77]], res[[3]][[69]], res[[3]][[61]], res[[3]][[53]], res[[3]][[45]])
vec100 <- c(res[[3]][[76]], res[[3]][[68]], res[[3]][[60]], res[[3]][[52]], res[[3]][[44]])
vec200 <- c(res[[3]][[75]], res[[3]][[67]], res[[3]][[59]], res[[3]][[51]], res[[3]][[43]])
vec250 <- c(res[[3]][[74]], res[[3]][[66]], res[[3]][[58]], res[[3]][[50]], res[[3]][[42]])
vec500 <- c(res[[3]][[73]], res[[3]][[65]], res[[3]][[57]], res[[3]][[49]], res[[3]][[41]])

dd6[1, ] <- vec5
dd6[2, ] <- vec10
dd6[3, ] <- vec25
dd6[4, ] <- vec50
dd6[5, ] <- vec100
dd6[6, ] <- vec200
dd6[7, ] <- vec250
dd6[8, ] <- vec500

z6 <- dd6

hist3D(z = z6,
       axes = TRUE,
       label = TRUE,
       lighting = TRUE,
       space = 0.1,
       theta = 160,
       phi = 21,
       xlab = "m",
       ylab = "pm",
       zlab = "Relative Bias",
       expand = 0.5,
       colkey = list(side = 1, length = 0.5),
       zlim = c(0.0,0.35),
       ticktype = "simple",
       clab = "Relative Bias",
)
##-------------------------------------------------------------------------------------------------------------------##
###SE values for FMIs
rm(list=ls(all=TRUE))
library("formattable")
setwd("/home/itsme/BachelorThesisCode/Final_Results")
fin <- readRDS("Results_without_TrueFmiUwe.rds")
fin <- fin[, -c(1:3,9)]
test <- fin[,c(1:3)]*100
test2 <- fin[, c(4:5)]
fin2 <- cbind(test, test2)

###MCAR
finmcar <- fin2[-c(41:80),]
dfmcar <- data.frame(finmcar)
dfmcar <- dfmcar[, c(5,4,1,2,3)]


dfmcar[c(2:8,10:16,18:24,26:32, 34:40),1] <- NA
dfmcar[, c(3:5)] <- format(round(dfmcar[, c(3:5)],2), nsmall = 2)
colnames(dfmcar) <- c("pm", "m", "SE*100(mean)", "SE*100(var)", "SE*100(cov)")
formattable(dfmcar, align = c("l", rep("r", NCOL(dfmcar) - 1)))


###MAR
finmar <- fin2[-c(1:40),]
dfmar <- data.frame(finmar)
dfmar <- dfmar[, c(5,4,1,2,3)]

dfmar[c(2:8,10:16,18:24,26:32, 34:40),1] <- NA
dfmar[, c(3:5)] <- format(round(dfmar[, c(3:5)],2), nsmall = 2)
colnames(dfmar) <- c("pm", "m", "SE*100(mean)", "SE*100(var)", "SE*100(cov)")
formattable(dfmar, align = c("l", rep("r", NCOL(dfmcar) - 1)))

##-------------------------------------------------------------------------------------------------------------------##
#Suggestions for m

c <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)
pm <- c(0.90, NA, NA, 0.75, NA, NA, 0.50, NA, NA, 0.25, NA, NA, 0.10, NA, NA)
###MEAN
SE <- c(2,3,4,3,4,5,3,4,5,2,3,4,1,2,3)/100
MCAR <- c(NA,500,50,250,100,50,500,100,50,250,100,50,200,50,25)
MAR <- c(50,10,NA,500,50,25,NA,NA,250,NA,500,100,500,50,25)
meansugg <- cbind(pm,SE,MCAR,MAR)
###VAR
SE <- c(1,2,3,2,3,4,3,4,5,2,3,4,1,2,3)/100
MCAR <- c(NA,100,25,500,200,100,250,100,NA,500,200,100,500,100,50)
MAR <- c(500,100,25,500,200,100,500,250,100,NA,500,200,NA,500,100)
varsugg <- cbind(pm,SE,MCAR,MAR)
###COV
pm <- c(0.90, NA, NA, 0.75, NA, NA, 0.50, NA, NA, 0.25, NA, NA, 0.10, NA, NA, NA)
SE <- c(5,6,7,6,7,8,8,9,10,6,7,8,4,5,6,7)/100
MCAR<- c(NA,500,50,NA,NA,100,250,50,25,500,50,25,500,50,NA,10)
MAR <- c(50,25,10,500,50,25,250,50,NA,NA,500,50,NA,NA,500,50)
covsugg <- cbind(pm,SE,MCAR,MAR)



meansug <- data.frame(meansugg)
varsug <- data.frame(varsugg)
covsug <- data.frame(covsugg)

formattable(meansug, align = c("l", rep("r", NCOL(meansug) - 1)))
formattable(varsug, align = c("l", rep("r", NCOL(meansug) - 1)))
formattable(covsug, align = c("l", rep("r", NCOL(meansug) - 1)))
