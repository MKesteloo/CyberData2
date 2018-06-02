library(forecast)
library(corrplot)

#load the data
data1 <- read.csv(file="c:/Users/pasca/Documents/CS/CyberData/practicum2/BATADAL_dataset03.csv", header=TRUE, sep=",")
data2 <- read.csv(file="c:/Users/pasca/Documents/CS/CyberData/practicum2/BATADAL_dataset04.csv", header=TRUE, sep=",")


#Select column
tsTrain = data1$L_T1
tsReal = data2$L_T1
plot(tsTrain, type = 'l')

#Determine the appropriate model parameters and rain the model
autocorr1 = acf(tsTrain)
autocorr2 = pacf(tsTrain)
dif = diff(tsTrain,differences = 1)
autocorrD1 = acf(dif)
autocorrD2 = pacf(dif)
trainedModel = arima(tsTrain, order = c(2,0,2))
hist(trainedModel$residuals)
resCorr = acf(trainedModel$residuals)
resCorrp = pacf(trainedModel$residuals)

#Anomaly detection
fc = Arima(tsReal, model = trainedModel)
error = abs(fc$fitted - tsReal)
plot(error, type = 'l')
realAnomalies = data2$ATT_FLAG == 1
alarm = error > min(boxplot(error)$out)

#metrics
TP = sum(realAnomalies & alarm)
FP = sum(alarm & !realAnomalies)
TN = sum(!realAnomalies & !alarm)
FN = sum(realAnomalies & !alarm)
numActualAttacks = sum(realAnomalies)

#overlap plot
plot(alarm, type = "l", col="red")
lines(realAnomalies, col="green")
