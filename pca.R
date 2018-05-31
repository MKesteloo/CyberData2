data1 <- read.csv(file="C:/Users/mitch/Documents/GitHub/CyberData2_group30/set1.csv", header=TRUE, sep=",")
data2 <- read.csv(file="C:/Users/mitch/Documents/GitHub/CyberData2_group30/set2.csv", header=TRUE, sep=",")

#don't do pca on attack flag and the date
pca.train <- data1[2:44]
pca.test <- data2[2:44]

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
prin_comp <- prcomp(pca.train, scale. = T)

#compute std of each pc
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#scree plot: take number of pc that retains as much variance as possible
#plot created by lines below shows around 15~16 pcs should be used
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

prin_comp_train <- prin_comp[1:16]
prin_comp_test <- prcomp(pca.train, scale. = T)
prin_comp_test <- prin_comp_test[1:16]


