setwd("C:/Users/Siddharth/SkyDrive/Documents/Kaggle/Bike Sharing")
library(lubridate)
library(ggplot2)
library(caret)
library(rpart)

#Read Data

sam <- read.csv("sampleSubmission.csv")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Convert variables into required form (factor in this case)

train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
str(train)
train$hour <- hour(train$datetime)
train$datetime <- as.Date(train$datetime)
train$weekday <- weekdays(train$datetime)
train$hour <- as.factor(train$hour)
train$weekday <- as.factor(train$weekday)
train$month <- month(train$datetime)
train$month <- as.factor(train$month)
train <- train[,-c(1,10,11)]

# Data is clean, and ready to use till here. Now, we partition it.

set.seed(101)
intrain <- createDataPartition(train$count, p=0.7, list = F, times = 1)
validation <- train[-intrain,]
train <- train[intrain,]

# Creating formula to use in model training

n <- names(train)
f <- as.formula(paste("count ~", paste(n[!n %in% "count"], collapse = " + ")))

# We use regresstion trees algorithm here to train our model

fit<-rpart(f,method="anova",data=train)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit,uniform=TRUE)
text(fit,use.n=TRUE,all=TRUE,cex=1)
pfit<-prune(fit,cp=0.01160389)
plot(pfit,unifrom=TRUE)
text(pfit,use.n=TRUE,all=TRUE,cex=1)

# From the plots you can see that the tree has been created. Now we use this model (stored in "fit") to predict on validation data

predictrpart<-predict(fit,validation,type="vector")
errorrpart<-sqrt(mean((predictrpart - validation$count)^2))
denom<-sqrt(mean((validation$count-mean(validation$count))^2))
relativeerror <- errorrpart/denom # This is the error that we get when using this mdoel in validation set. Yes, its very bad.

# Processing Test data. Same as what was done on the training set.

test$season <- as.factor(test$season)
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)
str(test)
test$hour <- hour(test$datetime)
test$datetime <- as.Date(test$datetime)
test$weekday <- weekdays(test$datetime)
test$hour <- as.factor(test$hour)
test$weekday <- as.factor(test$weekday)
test$month <- month(test$datetime)
test$month <- as.factor(test$month)
test <- test[,-c(1)]
predictrparttest<-predict(fit,test,type="vector")
sam$count <- predictrparttest
sam$count <- as.integer(sam$count)
sam$datetime <- as.character(sam$datetime)
write.csv(sam, file = "rf.csv", row.names = F)