set.seed(1)
install.packages('caret')
library('caret')

data<-read.csv("E:\\TAMU\\TAMU_Academics\\Fall_18\\EDA\\Project\\train_set0506.csv", header=TRUE, sep=",")
data$c_claim <- ifelse(data$Claim_Amount ==0,0,1)
sum(data$c_claim)

data$Cat1[data$Cat1 == "?"] <- NA
data$Cat2[data$Cat2 == "?"] <- NA
data$Cat3[data$Cat3 == "?"] <- NA
data$Cat4[data$Cat4 == "?"] <- NA
data$Cat5[data$Cat5 == "?"] <- NA
data$Cat6[data$Cat6 == "?"] <- NA
data$Cat7[data$Cat7 == "?"] <- NA
data$Cat8[data$Cat8 == "?"] <- NA
data$Cat9[data$Cat9 == "?"] <- NA
data$Cat10[data$Cat10 == "?"] <- NA
data$Cat11[data$Cat11 == "?"] <- NA
data$Cat12[data$Cat12 == "?"] <- NA
data$OrdCat[data$OrdCat == "?"] <- NA

str(data)
data <- data[,-34]
data_f <- data[,c(1:5,21:28,30:34)]

sum(is.na(data_f))

install.packages("dummies")
library(dummies)
data_new <- dummy.data.frame(data_f)
str(data_new)

#Spliting the dataset

index<- sample(x=nrow(data_new), size=.70*nrow(data_new), replace=F)
trainSet <- data_new[index,]
testSet <- data_new[-index,]
str(trainSet)

require(gbm)

boos <- gbm(c_claim ~ ., data=trainSet, distribution="gaussian", shrinkage=.01, cv.folds=5, interaction.depth=4,n.trees=2500)
summary(boos)

predictions <- predict.gbm(boos, newdata=testSet, n.trees=2500, type="response")
head(predictions)
length(predictions)

summary(predictions)
table(predictions>0.005,testSet$c_claim)
hist(predictions)

