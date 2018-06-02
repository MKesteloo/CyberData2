library(ngram)
library(seewave)

#load the data
data1 <- read.csv(file="c:/Users/pasca/Documents/CS/CyberData/practicum2/BATADAL_dataset03.csv", header=TRUE, sep=",")
data2 <- read.csv(file="c:/Users/pasca/Documents/CS/CyberData/practicum2/BATADAL_dataset04.csv", header=TRUE, sep=",")

#model parameters
numAlpha = 20
numPAA = 100 #maximum possible without giving memory errors.
n = 2
mode = "quantiles"

#Discretize the time series and construct N-Gram words.
temp1 = data1$L_T1
s1 = SAX(temp1, numAlpha, numPAA, mode, collapse =  " ")
n1 = ngram(s1, n)
words1 = get.phrasetable(n1)
temp2 = data2$L_T1
s2 = SAX(temp2, numAlpha, numPAA, mode, collapse =  " ")
n2 = ngram(s2, n)
words2 = get.phrasetable(n2)

#Calculate the error table between the N-Grams of the attack-free and non-attack-free datasets.
errors = data.frame("word", 10)
names(errors) = c("word","error")
for(i in 1:length(words2$ngrams)){
  word = words2$ngrams[i]
  curr = gsub(" ", "", word)
  if(word %in% words1$ngrams){
    index = which(words1$ngrams == word)
    dif = abs(words2$prop[i] - words1$prop[index])
    new = data.frame(curr, dif)
    names(new) = c("word","error")
    errors = rbind(errors, new)
  }
  else {
    new = data.frame(curr, -1)
    names(new) = c("word","error")
    errors = rbind(errors, new)
  }
}
errors = errors[-1,]

#anomaly detection
realAnomalies = data2$ATT_FLAG == 1
alarm = rep(0, length(data2$ATT_FLAG))
concat = gsub(" ", "", s2)
conversionRate = floor(length(alarm) / numPAA)
unknown = errors$error == -1
threshold = boxplot(errors$error[!unknown])$stats[5]
for(i in 1:length(errors$word)){
  if(errors$error[i] == -1 || errors$error[i] > threshold){
    pos = conversionRate * unlist(gregexpr(errors$word[i], concat))
    paa = c(pos, pos + 1)#repeated n - 1 times.
    alarm[paa] = 1
  }
}

#metrics
TP = sum(realAnomalies & alarm)
FP = sum(alarm & !realAnomalies)
TN = sum(!realAnomalies & !alarm)
FN = sum(realAnomalies & !alarm)
print(cat("TP ", TP))
print(cat("FP ", FP))
print(cat("TN ", TN))
print(cat("FN ", FN))

plot(alarm, type = "l", col="red")
lines(realAnomalies, col="blue")