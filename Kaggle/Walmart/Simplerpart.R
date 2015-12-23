setwd("/home/tinto/myFiles/Kaggle/Walmart")

rm(list = ls())

require("MLmetrics")
require("rpart")
library(caret)
require(stats)
library(reshape2)
require("randomForest")
require("gbm")

wall_train <- read.csv("train.csv", stringsAsFactors = TRUE)

wall_test <- read.csv("test.csv", stringsAsFactors = TRUE)

str(train)
summary(train)


levels(wall_train$DepartmentDescription)
levels(wall_test$DepartmentDescription)

train <- wall_train
test <- wall_test

rm (wall_train,wall_test)



########################################
#Feature Engineering
######################################################################

train$TripType <- as.factor(train$TripType)

levels(train$DepartmentDescription)
levels(test$DepartmentDescription)

levels(test$DepartmentDescription) = levels(train$DepartmentDescription)

train <- na.omit(train)


######################################################################
  #Convert factors to columns


train <- dcast(train,
      formula = TripType + VisitNumber+ Weekday~ DepartmentDescription,
      value.var = "ScanCount")

test <- dcast(test,
               formula = VisitNumber+ Weekday~ DepartmentDescription,
               value.var = "ScanCount")

test$WIRELESS <- 0

head(test)

######################################################################
#Drop Not needed Columns

train$VisitNumber <-NULL

#Save Needed
VisitNumber <- test$VisitNumber
test$VisitNumber <- NULL

Train <- train
Test <- test

#Drop Not Needed objects

rm (train,test)

######################################################################
#Splitting the data into Training and Testing 
#As data is huge am going to take ~7000 records for initial training
######################################################################

set.seed(3000)

#Method 1
require(caTools)
sample = sample.split(Train$TripType, SplitRatio = .6)
train = subset(Train, sample == TRUE)
test = subset(Train, sample == FALSE)

levels(test$TripType) <- levels(train$TripType)



str(train)
str(test)
head(train)
#plot(d$FinelineNumber,d$ScanCount)
#plot(d$DepartmentDescription)
#hist(d$FinelineNumber)

#Train

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                          # repeats = 10,
                           classProbs = TRUE)

#CART1
set.seed(1234)
rtree1 = train (TripType~.,
               train,
               method = "rpart",
               tuneLength=20,
               metric="ROC",
               verbose = 3,
               trControl = fitControl)


?make.names
rpart.model <- rpart(TripType~., data=train, method = "class")

summary(rpart.pred) 

gbm.model <- gbm(TripType~., data=train)

rf.model$finalModel

rf.model$f
  
  
?gbm
###########################################################
#Test and Check for Muliclass Log loss

TripType <- test$TripType

test$TripType <- NULL

rpart.pred <- predict(rf.model$finalModel, type = "response",newdata = test,n.trees=rf.model$bestTune$.n.trees)

MultiLogLoss(y_true=TripType, y_pred=rpart.pred)

######################################################################
#Predict

rpart.pred <- predict(rpart.model, type = "prob",newdata = Test)

######################################################################
#Save  to CSV

rpart.pred <- round(rpart.pred, digits = 2)

head(rpart.pred)

nrow(rpart.pred)

colnames(rpart.pred) <- paste("TripType", colnames(rpart.pred), sep = "_")

rpart.pred <- cbind(VisitNumber,rpart.pred)

rpart.pred <- format(rpart.pred, sci = FALSE) 

pred <- as.data.frame()
write.table(rpart.pred,"rpartpred.csv",sep = ",",row.names = FALSE,col.names = TRUE)

######################################################################
######################################################################
