library(bio3d)
data1 <- read.csv(file="C:/Users/mitch/Documents/GitHub/CyberData2_group30/set1.csv", header=TRUE, sep=",")
data2 <- read.csv(file="C:/Users/mitch/Documents/GitHub/CyberData2_group30/set2.csv", header=TRUE, sep=",")

#don't do pca on attack flag and the date
train <- data1[2:44]
test <- data2[2:44]

#scale: mean 0 std 1 otherwise pca does not work well
pca.train <- scale(train)
pca.test <- scale(test)
pca.train <- data.frame(pca.train)
pca.test <- data.frame(pca.test)
#remove always zero
cols.dont.want <- c("S_PU3", "F_PU3", "S_PU5", "F_PU5", "S_PU9", "F_PU9", "S_PU1") 
pca.train <- pca.train[, ! names(pca.train) %in% cols.dont.want, drop = F]
pca.test <- pca.test[, ! names(pca.test) %in% cols.dont.want, drop = F]

#set all features to numeric
pca.train$S_PU2 <- as.numeric(pca.train$S_PU2)
pca.train$S_PU4 <- as.numeric(pca.train$S_PU4)
pca.train$S_PU6 <- as.numeric(pca.train$S_PU6)
pca.train$S_PU7 <- as.numeric(pca.train$S_PU7)
pca.train$S_PU8 <- as.numeric(pca.train$S_PU8)
pca.train$S_PU10 <- as.numeric(pca.train$S_PU10)
pca.train$S_PU11 <- as.numeric(pca.train$S_PU11)
pca.train$S_V2 <- as.numeric(pca.train$S_V2)

pca.test$S_PU2 <- as.numeric(pca.test$S_PU2)
pca.test$S_PU4 <- as.numeric(pca.test$S_PU4)
pca.test$S_PU6 <- as.numeric(pca.test$S_PU6)
pca.test$S_PU7 <- as.numeric(pca.test$S_PU7)
pca.test$S_PU8 <- as.numeric(pca.test$S_PU8)
pca.test$S_PU10 <- as.numeric(pca.test$S_PU10)
pca.test$S_PU11 <- as.numeric(pca.test$S_PU11)
pca.test$S_V2 <- as.numeric(pca.test$S_V2)

#compute components: by default it centers around mean 0
#using scale = T means setting std to 1
prin_comp <- pca.xyz(pca.train)

#compute std of each pc
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#scree plot: take number of pc that retains as much variance as possible
#plot created by lines below shows around 15~16 explain almost 100 percent variance
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

