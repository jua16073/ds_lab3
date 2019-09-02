library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(rpart)
library(FAdist)

setwd("Documents/Data_Science/ds_lab3")
getwd()


data <- read.csv("datosImp.csv")
data

summary(data)

# Analisis de Variables
#GLP
descdist(data$GLP, discrete = FALSE)
dist <- fitdist(data$GLP,"norm")
plot(dist)

#GasAviacio
descdist(data$GasAviacion[complete.cases(data$GasAviacion)], discrete = FALSE)
dist <- fitdist(data$GasAviacion[complete.cases(data$GasAviacion)],"norm")
plot(dist)


# GasSuperior
descdist(data$GasSuperior, discrete = FALSE)
dist <- fitdist(data$GasSuperior,"norm")
plot(dist)

# GasRegular
descdist(data$GasRegular, discrete = FALSE)
dist <- fitdist(data$GasRegular,"norm")
plot(dist)

# Kerosina
descdist(data$Kerosina[complete.cases(data$Kerosina)], discrete = FALSE)
dist <- fitdist(data$Kerosina[complete.cases(data$Kerosina)],"norm")
plot(dist)

# rTurboJet
descdist(data$rTurboJet[complete.cases(data$rTurboJet)], discrete = FALSE)
dist <- fitdist(data$rTurboJet[complete.cases(data$rTurboJet)],"norm")
plot(dist)

# rTurboJet
descdist(data$Diesel[complete.cases(data$rTurboJet)], discrete = FALSE)
dist <- fitdist(data$rTurboJet[complete.cases(data$rTurboJet)],"norm")
plot(dist)
