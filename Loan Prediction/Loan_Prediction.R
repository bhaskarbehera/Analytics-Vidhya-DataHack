library(dplyr)
library(corrplot)
library(caret)
library(lattice)
library(ggplot2)
library(glmnet)
library(rpart)
library(randomForest)
library(VIM)

Training_df <- read.csv("E:/DATA MINING/train_Loan_Prediction.csv",na.strings = c(""," ",NA))
Testing_df <- read.csv("E:/DATA MINING/test_Loan_Prediction.csv",na.strings = c(""," ",NA))


levels(Training_df$Dependents)[levels(Training_df$Dependents)=="3+"] <- "3"
Training_df$Dependents<-as.integer(as.character(Training_df$Dependents))

levels(Testing_df$Dependents)[levels(Testing_df$Dependents)=="3+"] <- "3"
Testing_df$Dependents<-as.integer(as.character(Testing_df$Dependents))

Training_df$Credit_History <- factor(Training_df$Credit_History)
Testing_df$Credit_History <- factor(Testing_df$Credit_History)
train$Married<-droplevels(train$Married)

summary(Training_df)





Training_df<- kNN(Training_df)
Training_df <- Training_df[,-14:-26]
Testing_df<- kNN(Testing_df)
Testing_df <- Testing_df[,-14:-26]
summary(Testing_df)

table(complete.cases(Training_df))

Training_df$Loan_Status <-factor(Training_df$Loan_Status)

id <- sample(2,nrow(Training_df),prob = c(.9,.1),replace = T)
id
train <- Training_df[id==1,]
validation_LP <- Training_df[id==2,]

colnames(train)
hist(train$LoanAmount)
train <- subset(train,train$LoanAmount<600|train$LoanAmount==600)
train <- subset(train,train$LoanAmount<10000|train$LoanAmount==10000)
train <- subset(train,train$LoanAmount>100)
View(train)
hist(train$Property_Area)
hist(Testing_df$Dependents)
#Exploratory Analysis
qplot(Loan_Amount_Term,data=train,fill=Loan_Status)
qplot(Credit_History,data=train,fill=Loan_Status)
qplot(Dependents,data=train,fill=Loan_Status)
qplot(Education,data=train,fill=Loan_Status)
qplot(Self_Employed,data=train,fill=Loan_Status)
qplot(ApplicantIncome,data=train,fill=Loan_Status)
qplot(LoanAmount,data=train,fill=Loan_Status)
qplot(Property_Area,data=train,fill=Loan_Status)
qplot(Married,data=train,fill=Loan_Status)


train_sub <- train[,-1]
train_sub$Gender <- as.numeric(train_sub$Gender)
train_sub$Married <- as.numeric(train_sub$Married)
train_sub$Education <- as.numeric(train_sub$Education)
train_sub$Self_Employed <- as.numeric(train_sub$Self_Employed)
train_sub$Property_Area <- as.numeric(train_sub$Property_Area)
train_sub$Loan_Status <- as.numeric(train_sub$Loan_Status)
train_sub$Dependents <- as.numeric(train_sub$Dependents)
train_sub$Credit_History <- as.numeric(train_sub$Credit_History)
str(train_sub)
correlation <- cor(train_sub)
correlation
corrplot(correlation,methods ="circle")

pc<- princomp(train_sub,cor=T,scores = T)
summary(pc)
plot(pc,type ="l")


library(e1071)

trainControl <- trainControl(method = "repeatedcv",number=10,repeats = 2)
metric <- "Accuracy"
#logistic regression
set.seed(3)

fit.glm <- train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df,method="glm",metric=metric,trControl=trainControl)
#linear discriminate analysis

set.seed(3)
fit.lda<-train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df, method="lda", metric=metric, trControl=trainControl)

#regularized logistic regression
set.seed(3)
fit.glmnet <- train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df, method="glmnet", metric=metric, trControl=trainControl)
#KNN
set.seed(3)
fit.knn <- train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df, method="knn", metric=metric, trControl=trainControl)

#CART

set.seed(3)
fit.cart <- train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df, method="rpart", metric=metric, trControl=trainControl)
#random forrest

set.seed(3)
fir.rf <- train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df, method="rf", metric=metric, trControl=trainControl)
set.seed(3)
#fir.nb <- train(Loan_Status~-Credit_History+Dependents+Gender,data = Training_df, method="naiveBayes", metric=metric, trControl=trainControl)

results <- resamples(list(GLM=fit.glm,IDA=fit.lda,GLMNET=fit.glmnet, CART=fit.cart, KNN=fit.knn))

summary(results)

classifier <- naiveBayes(Loan_Status~Credit_History+Dependents+Gender,data =Training_df)
predict_Loan <- predict(classifier,validation_LP[,-13])
conf.nb <- confusionMatrix(predict_Loan,validation_LP$Loan_Status)
conf.nb$overall[1]

Predict_test <- predict(classifier,Testing_df)

predict1 <- data.frame(Testing_df$Loan_ID,Predict_test)
colnames(predict1) <- c("Loan_ID","Loan_Status")
View(predict1)
write.csv(predict1,"E:/DATA MINING/Results2.csv")

