library(corrplot)
library(tseries)
library(forecast)
data <- read.csv(file="C:/Users/mitch/Documents/GitHub/CyberData2_group30/set1.csv", header=TRUE, sep=",")

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

#remove always zero
data$S_PU3 <- NULL
data$F_PU3 <- NULL
data$S_PU5 <- NULL
data$F_PU5 <- NULL
data$S_PU9 <- NULL
data$F_PU9 <- NULL
data$S_PU1 <- NULL

#remove non numeric
numMat = data[-1]
numMat = numMat[-length(numMat)]

#some plots to check what kind of signals there are
#we plotted every signal once, but most are similar
#if you want to view a single line, change "lines" into "plot": lines adds another curve to existing plot
#water levels in the tanks
par(resetPar())
plot(data$L_T1[1:400], type = 'l', col = 'red', ylim=range(0,8), xlab="Time", ylab="Water level")
lines(data$L_T2[1:400], type = 'l', col = 'blue')
legend("topleft", inset=.05, legend=c("Tank 1", "Tank 2"), col=c("red", "blue"), lty=c(1,1), cex=0.8)
# plot(data$L_T3[1:400], type = 'l', col = 'green')
# plot(data$L_T4[1:400], type = 'l', col = 'yellow')
# plot(data$L_T5[1:400], type = 'l', col = 'black')
#plot(data$L_T6[1:400], type = 'l', col = 'violet')
#plot(data$L_T7[1:400], type = 'l', col = 'brown')

# statuses and water flows of the pumps: some of the flows/statuses are always zero
# pumps 6 and 11 are only on at really short periods of time
# clear correlation between status and flow 
par(mfrow=c(2,1))
plot(data$F_PU2[1:400], type = 'l', col = 'red', xlab="Time", ylab="Flow")
plot(data$S_PU2[1:400], type = 'l', col = 'blue', xlab="Time", ylab="Status")
# plot(data$F_PU4[1:400], type = 'l', col = 'red')
# plot(data$S_PU4[1:400], type = 'l', col = 'blue')
# plot(data$F_PU6, type = 'l', col = 'red')
# plot(data$S_PU6, type = 'l', col = 'blue')
# plot(data$F_PU7[1:400], type = 'l', col = 'red')
# plot(data$S_PU7[1:400], type = 'l', col = 'blue')
# plot(data$F_PU8[1:400], type = 'l', col = 'red')
# plot(data$S_PU8[1:400], type = 'l', col = 'blue')
# plot(data$F_PU10[1:400], type = 'l', col = 'red')
# plot(data$S_PU10[1:400], type = 'l', col = 'blue')
# plot(data$F_PU11, type = 'l', col = 'red')
# plot(data$S_PU11, type = 'l', col = 'blue')

# status and flow around valve V2
plot(data$F_V2[1:400], type = 'l', col = 'red')
plot(data$S_V2[1:400], type = 'l', col = 'blue')

# pressure in the pipes
par(resetPar())
plot(data$P_J280[1:400], type = 'l')
# plot(data$P_J269[1:400], type = 'l')
# plot(data$P_J300[1:400], type = 'l')
# plot(data$P_J256[1:400], type = 'l')
# plot(data$P_J289[1:400], type = 'l')
# plot(data$P_J415[1:400], type = 'l')
# plot(data$P_J280[1:400], type = 'l')
# plot(data$P_J269[1:400], type = 'l')
# plot(data$P_J300[1:400], type = 'l')
# plot(data$P_J256[1:400], type = 'l')
# plot(data$P_J289[1:400], type = 'l')
# plot(data$P_J415[1:400], type = 'l')

# plot correlation matrix
par(resetPar())
a <- cor(numMat)
corrplot(a, method="circle")

# correlation: pump 1 is off when 2 is on and vice versa
par(mfrow=c(2,1))
plot(data$F_PU1[1:400], type = 'l')
plot(data$F_PU2[1:400], type = 'l')
# correlation: from the .inp file, clear that some of the pipes are close to some pumps ->
# these pipes have higher/lower pressure when the pump is on/off
# as example, we plot pipe J280 and the flows of pump 1 and 2
par(mfrow=c(3,1))
plot(data$P_J280[1:400], type = 'l')
plot(data$F_PU1[1:400], type = 'l')
plot(data$F_PU2[1:400], type = 'l')

# valve V2: when it is open, pressure in J14 drops, J422 rises
par(mfrow=c(3,1))
plot(data$S_V2[1:400], type = 'l')
plot(data$P_J14[1:400], type = 'l')
plot(data$P_J422[1:400], type = 'l')

# checking whether prediction is easy
# check acf and pacf: depending on plot, use MA, AR or ARMA model to predict signal
# waterlevel: if you want to check other one, change data and change arima model according to acf and pacf
temp = data$L_T1
par(mfrow=c(1,2))
acf(temp)
pacf(temp)
# not clear from acf, looks like AR(2) from pacf
model <- arima(temp, order = c(2,0,0))
automodel <- auto.arima(temp)
onestep <- fitted(model)
onestepauto <- fitted(automodel)
error = abs(onestep - temp)
errorauto = abs(onestepauto - temp)
plot(onestep[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(error)
mean(error)
par(mfrow=c(1,2))
plot(onestepauto[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(errorauto)
mean(errorauto)
# arima model from auto.arima is clearly better

# prediction flow (pumps and valve)
temp = data$F_PU2
par(mfrow=c(1,2))
acf(temp)
pacf(temp)
# not clear from acf, looks like AR(1) from pacf for PU2
model <- arima(temp, order = c(1,0,0))
automodel <- auto.arima(temp)
onestep <- fitted(model)
onestepauto <- fitted(automodel)
error = abs(onestep - temp)
errorauto = abs(onestepauto - temp)
plot(onestep[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(error)
mean(error)
par(mfrow=c(1,2))
plot(onestepauto[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(errorauto)
mean(errorauto)
# again auto is clearly better

# prediction status (pumps and valve)
temp = data$S_PU2
par(mfrow=c(1,2))
acf(temp)
pacf(temp)
# not clear from acf, looks like AR(1) from pacf for PU2
model <- arima(temp, order = c(1,0,0))
automodel <- auto.arima(temp)
onestep <- fitted(model)
onestepauto <- fitted(automodel)
error = abs(onestep - temp)
errorauto = abs(onestepauto - temp)
plot(onestep[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(error)
mean(error)
par(mfrow=c(1,2))
plot(onestepauto[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(errorauto)
mean(errorauto)
# again auto is clearly better, but plot still looks really off

#
temp = data$P_J280
par(mfrow=c(1,2))
acf(temp)
pacf(temp)
# not clear from acf, looks like AR(1) from pacf for J280
model <- arima(temp, order = c(1,0,0))
automodel <- auto.arima(temp)
onestep <- fitted(model)
onestepauto <- fitted(automodel)
error = abs(onestep - temp)
errorauto = abs(onestepauto - temp)
plot(onestep[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(error)
mean(error)
par(mfrow=c(1,2))
plot(onestepauto[1:400], type = 'l', col="red")
plot(temp[1:400], type = 'l', col="blue")
par(resetPar())
plot(errorauto)
mean(errorauto)

