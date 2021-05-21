###Plots&Graphs###
rm(list=ls(all=TRUE))
setwd("/home/itsme/BachelorThesisCode/Final_Results/")
results <- readRDS("Results_without_TrueFmiUwe.rds")
trueFmi <- readRDS("TrueFmiResultsbig.rds")
listAvgMeanFmi <- readRDS("AvgMeanFmiList.rds")
source("../init.R")
library("plot3D")
#library("ggplot2")
#library("dplyr")
#library("hrbrthemes")
#library("reshape2")
###------------------------------------------------------------------------------###
# Some Testing
res <- data.frame(results$avgMeanFmi, results$pm, results$m)

dd <- matrix(ncol = 5, nrow = 8)
x <- c(5, 10, 25, 50, 100, 200, 250, 500)
y <- c(0.1, 0.25, 0.5, 0.75, 0.9)
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

hist3D(z = z, zlim = c(0,1), axes = TRUE, label = TRUE, lighting = TRUE, space = 0.4,
       theta = 40, phi = 13, main = "MeanFmi - MCAR", xlab = "m", ylab = "pm", zlab = "FMI",
       ticktype = "detailed", nticks = 9, expand = 0.8, bty = "b2",
       border = "azure2", colkey = list(side = 1, length = 0.5), clab = "FMI")

#https://www.youtube.com/watch?v=ZuC2Eh-bOlw currently : 2:25:00


# hist3D(x = x, y = y, z = z)
# #b”, “b2”, “f”, “g”, “bl”, “bl2”, “u”, “n”
# ylim = c(0, 500), bty = "g", expand = 20,
# xlim = c(0.1, 0.9)
# 
# 
# 
# 
# 
# d <- data.frame(results$avgMeanFmi)
# res <- data.frame(results$avgMeanFmi, results$pm, results$m)
# x <- results$m
# y <- results$pm
# z <- results$avgMeanFmi
# 
# hist3D(x = x, y = y, z = z, zlim = c(0,50), theta = 40, phi = 40, axes = TRUE, label = TRUE, nticks = 5, 
#        ticktype = "detailed", space = 0.5, lighting = TRUE, light = "diffuse", shade = 0.5)
# 
# 
# 
# 
# 
# s <- data.frame(results$avgMeanFmi,results$m)
# news <- s[-c(41:80),] 
# dfNews <- melt(news)
# ggplot(data = news, mapping = aes(x = results.m, y = results.avgMeanFmi, fill = results.avgMeanFmi))+
#   geom_bar(stat = "identity", position = "dodge")
# 
# p <-  ggplot(data = news, mapping = aes(x = results.m, y = results.avgMeanFmi, fill = results.avgMeanFmi))+
#   
# p
# 
# c <- ggplot(data = results)
# 
# ggplot(data = results$avgMeanFmi,
#        aes(x = results$m, y = results$pm))
# 
# hist
# t <- do.call(rbind, Map(data.frame, listAvgMeanFmi))
# t <- data.frame(listAvgMeanFmi)
# 
# test <- cbind(listAvgMeanFmi, conds)
# 
# 
# 
# #Avg Fmi values in graph
# 
# x <- MeanFmi <- results$avgMeanFmi 
# y <- VarFmi  <- results$avgVarFmi
# z <- CovFmi  <- results$avgCovFmi
#  
# pmm <- c(0.9, 0.75, 0.5, 0.25, 0.1)
# 
# scatter3D(MeanFmi <- x, VarFmi <- y, CovFmi <- z, clab = c("Fmi"), cex = 1.0, pch = 19,
#           colkey = list(side = 1, length = 0.5), phi = 0, theta = 16, bty = "g", xlab = "MeanFmi") 
# 
# hist3D(x = m, y = pmm, z = )
# 
