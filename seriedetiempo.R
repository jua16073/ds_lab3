install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")

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
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(imputeTS)

setwd("D:/data science/lab3DataScience/ds_lab3/")
getwd()
data = read.csv("datosImp.csv") 

data$Diesel <- na.replace(data$Diesel,0)
data$DieselLS <- na.replace(data$DieselLS,0)
data$DieselULS <- na.replace(data$DieselULS,0)

data$Diesel <- data$Diesel+data$DieselLS+data$DieselULS

data <- within(data,rm("DieselLS", "DieselULS"))

descdist(data$Diesel)

anios <- table(data$Anio)
anios

timeseries <- ts(data$Diesel, start=c(2001, 1), end=c(2019, 6), frequency=12) 
plot(timeseries)

abline(reg=lm(timeseries~time(timeseries)), col=c("red"))

plot(aggregate(timeseries,FUN=mean))
dec.diesel<-decompose(timeseries)
plot(dec.diesel)
plot(dec.diesel$seasonal)
