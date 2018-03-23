library(dplyr)
library(corrplot)
library(caret)
library(lattice)
library(ggplot2)
library(glmnet)
library(rpart)
library(randomForest)
library(VIM)
training_bsm <- read.csv("./Train.csv",na.strings =c("",NA))
testing_bsm <- read.csv("./Test.csv",na.strings = c("",NA))
sum(complete.cases(testing_bsm))
View(testing_bsm)
str(training_bsm)
#for training data set
training_bsm$Item_Fat_Content <- gsub("LF", "lowfat",training_bsm$Item_Fat_Content)
training_bsm$Item_Fat_Content <- gsub("low fat", "lowfat",training_bsm$Item_Fat_Content)
training_bsm$Item_Fat_Content <- gsub("Low Fat", "lowfat",training_bsm$Item_Fat_Content)
training_bsm$Item_Fat_Content <- gsub("reg", "Regular",training_bsm$Item_Fat_Content)
training_bsm$Item_Fat_Content <- as.factor(training_bsm$Item_Fat_Content)

#for testing data set
testing_bsm$Item_Fat_Content <- gsub("LF", "lowfat",testing_bsm$Item_Fat_Content)
testing_bsm$Item_Fat_Content <- gsub("low fat", "lowfat",testing_bsm$Item_Fat_Content)
testing_bsm$Item_Fat_Content <- gsub("Low Fat", "lowfat",testing_bsm$Item_Fat_Content)
testing_bsm$Item_Fat_Content <- gsub("reg", "Regular",testing_bsm$Item_Fat_Content)
testing_bsm$Item_Fat_Content <- as.factor(testing_bsm$Item_Fat_Content)
#for missing values knn imputation

training_bsm <- kNN(training_bsm)
training_bsm <- training_bsm[,1:12]
testing_bsm <- kNN(testing_bsm)
testing_bsm <- testing_bsm[,1:12]
View(training_bsm)
summary(training_bsm)

#calculate visibility for training
Data <- training_bsm %>% filter(Item_Visibility!=0)
visibility_model <- lm(Item_Visibility~Item_Weight + Item_Fat_Content +
                         Item_Type + Item_MRP + 
                         Outlet_Establishment_Year + Outlet_Size + 
                         Outlet_Location_Type + Item_Outlet_Sales,data=Data)
training_bsm$Item_Visibility[training_bsm$Item_Visibility==0] <- predict(visibility_model,newdata = training_bsm[training_bsm$Item_Visibility==0,])
View(training_bsm)


#calculate visibility for testing

Data2 <- testing_bsm %>% filter(Item_Visibility!=0)
visibility_model2 <- lm(Item_Visibility~Item_Weight + Item_Fat_Content +
                         Item_Type + Item_MRP + 
                         Outlet_Establishment_Year + Outlet_Size + 
                         Outlet_Location_Type,data=Data)
testing_bsm$Item_Visibility[testing_bsm$Item_Visibility==0] <- predict(visibility_model2,newdata = testing_bsm[testing_bsm$Item_Visibility==0,])


#histogram for outlier detection
hist(training_bsm$Item_Weight)
hist(training_bsm$Item_Visibility)
hist(training_bsm$Item_MRP)
hist(training_bsm$Item_Outlet_Sales)

#cross tables
fat_levels <- xtabs(~training_bsm$Item_Fat_Content)
xtabs(~training_bsm$Item_Type+training_bsm$Item_Fat_Content)


#converting to num values
training2 <- training_bsm
training2$Item_Fat_Content<-as.numeric(training2$Item_Fat_Content)
training2$Outlet_Size <- as.numeric(training2$Outlet_Size)
training2$Outlet_Location_Type <- as.numeric(training2$Outlet_Location_Type)
training2$Outlet_Type <- as.numeric(training2$Outlet_Type)
str(training2)
View(training2)
correlation <- cor(training2[,c(2,3,4,6,9,10,11,12)])
corrplot(correlation)

#hypothesis testing
#H0= sales not related to fat content
test1 <- xtabs(~training2$Item_Weight+training2$Item_Outlet_Sales)
chisq.test(test1)
test2 <- xtabs(~training2$Item_Fat_Content+training2$Item_Outlet_Sales)
chisq.test(test2)


#modelling
id <- sample(2,nrow(testing_bsm),prob = c(0.8,.2),replace = T)
train <- training_bsm[id==1,]
test <- training_bsm[id==2,]
nrow(train)

#linear regression
linear_reg <- lm(Item_Outlet_Sales~Item_Visibility+Item_Weight + Item_Fat_Content+
                   Item_Type + Item_MRP + 
                   Outlet_Establishment_Year + Outlet_Size + 
                   Outlet_Location_Type+Outlet_Type,data=training_bsm)
pred1 <- predict(linear_reg,newdata = test)

error1 <- sum((pred1-test$Item_Outlet_Sales)^2)
RMSE1 <- (error/nrow(test))^.5
RMSE1

predict_lm <- predict(linear_reg,testing_bsm)


result <-data.frame(testing_bsm$Item_Identifier,testing_bsm$Outlet_Identifier,predict_lm)
colnames(result) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
View(result)
write.csv(result,"./result_bsm2.csv")
#random forest
random_forest <- randomForest(Item_Outlet_Sales~Item_Visibility+Item_Weight + Item_Fat_Content+
                   Item_Type + Item_MRP + 
                   Outlet_Establishment_Year + Outlet_Size + 
                   Outlet_Location_Type+Outlet_Type,data=training_bsm)
predict_rf <- predict(random_forest,testing_bsm)


result2 <-data.frame(testing_bsm$Item_Identifier,testing_bsm$Outlet_Identifier,predict_rf)
colnames(result2) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
View(result2)
write.csv(result2,"./result_rf.csv")

pred2 <- predict(random_forest,newdata = test)

error2 <- sum((pred2-test$Item_Outlet_Sales)^2)
RMSE2 <- (error/nrow(test))^.5
RMSE2


















