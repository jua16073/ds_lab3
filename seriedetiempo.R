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
library(lmtest)
library(qpcR)
setwd("D:/data science/lab3DataScience/ds_lab3/")
getwd()
data = read.csv("datosImp.csv") 


###########################
#DIESEL####################
###########################

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

#transformacion logaritmica
logtime <- log(timeseries)
plot(decompose(logtime))

#raices unitarias
adfTest(logtime)
adfTest(diff(logtime))

#autocorrelacion
acf(logtime)

# Hacer el modelo

auto.arima(timeseries)

fit <- arima(log(timeseries), c(3, 1, 1),seasonal = list(order = c(1, 0, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(timeseries,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(timeseries), c(3, 1, 1),seasonal = list(order = c(3, 1, 1), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)

#comprobando independencia de residuos y suma de cuadrados modelo 2
Box.test(resid(fit2), lag = 1, type = c("Ljung-Box"), fitdf = 0)
RSS(fit2)

#comprobando independencia de residuos y suma de cuadrados modelo 1
Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)
RSS(fit)

e <- tsCV(timeseries, forecastfunction = naive,h=1)
e
plot(e)
#############################
##GASOLINA REGULAR###########
#############################

gasolinaReg <- ts(data$GasRegular, start=c(2001, 1), end=c(2018, 12), frequency=12) 
plot(gasolinaReg)

abline(reg=lm(gasolinaReg~time(gasolinaReg)), col=c("red"))

plot(aggregate(gasolinaReg,FUN=mean))
dec.reg<-decompose(gasolinaReg)
plot(dec.reg)
plot(dec.diesel$seasonal)

#transformacion logaritmica
logreg <- log(gasolinaReg)
plot(decompose(logreg))

#raices unitarias
adfTest(logreg)
adfTest(diff(logreg))

#autocorrelacion
acf(logreg)
pacf(logreg)
# Hacer el modelo

auto.arima(gasolinaReg)

fitreg <- arima(log(gasolinaReg), c(3, 1, 1),seasonal = list(order = c(1, 0, 1), period = 12))
predreg <- predict(fitreg, n.ahead = 10*12)
ts.plot(gasolinaReg,2.718^predreg$pred, log = "y", lty = c(1,3))

fit2reg <- arima(log(gasolinaReg), c(1, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

forecastAPreg <- forecast(fit2reg, level = c(95), h = 120)
autoplot(forecastAPreg)
forecastAPreg
coeftest(fit2reg)

#comprobando independencia de residuos y suma de cuadrados modelo 2
Box.test(resid(fit2reg), lag = 1, type = c("Ljung-Box"), fitdf = 0)
RSS(fit2reg)
acf(fit2reg$residuals)
fit2reg
accuracy(fit2reg)
#comprobando independencia de residuos y suma de cuadrados modelo 1
Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)
RSS(fit)

#############################
##GASOLINA SUPERIOR##########
#############################

gasolinaSup <- ts(data$GasSuperior, start=c(2001, 1), end=c(2018, 12), frequency=12) 
plot(gasolinaReg)

abline(reg=lm(gasolinaSup~time(gasolinaSup)), col=c("red"))

plot(aggregate(gasolinaSup,FUN=mean))
dec.sup<-decompose(gasolinaSup)
plot(dec.sup)
plot(dec.sup$seasonal)

#transformacion logaritmica
logsup <- log(gasolinaSup)
plot(decompose(logsup))

#raices unitarias
adfTest(logsup)
adfTest(diff(logsup))

#autocorrelacion
acf(logsup)
pacf(logsup)
# Hacer el modelo

auto.arima(gasolinaSup)

fitsup <- arima(log(gasolinaReg), c(3, 1, 1),seasonal = list(order = c(1, 0, 1), period = 12))
predsup <- predict(fitsup, n.ahead = 10*12)
ts.plot(gasolinaSup,2.718^predsup$pred, log = "y", lty = c(1,3))

fit2sup <- arima(log(gasolinaSup), c(1, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

forecastAPsup <- forecast(fit2sup, level = c(95), h = 120)
autoplot(forecastAPsup)
coeftest(fit2sup)

#comprobando independencia de residuos y suma de cuadrados modelo 2
Box.test(resid(fit2sup), lag = 1, type = c("Ljung-Box"), fitdf = 0)
RSS(fit2sup)
acf(fit2sup$residuals)


#comprobando independencia de residuos y suma de cuadrados modelo 1
Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)
RSS(fit)