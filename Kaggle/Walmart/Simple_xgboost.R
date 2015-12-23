
setwd("/home/tinto/myFiles/Kaggle/Walmart")

rm(list = ls())
require(xgboost)
require("MLmetrics")
require("rpart")
library(caret)
require(stats)
library(reshape2)
require("randomForest")
require("gbm")
require(Rtsne)

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

class.levels <- levels(train$TripType)
class.num <- length(class.levels)

train$Weekday <- as.integer(train$Weekday)
test$Weekday <- as.integer(test$Weekday)

train$TripType <- as.integer(train$TripType)

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
VisitNumber <- as.integer(test$VisitNumber)
test$VisitNumber <- NULL

#Train <- train
#Test <- test

#Drop Not Needed objects

#rm (train,test)

######################################################################
#Splitting the data into Training and Testing 
#As data is huge am going to take ~7000 records for initial training
######################################################################

#XgboostTrain

trainy <- train$TripType-1
train$TripType <- NULL

#trainy = gsub('Class_','',trainy)
#trainy = as.integer(trainy)-1 #xgboost take features in [0,numOfClass)

x = train
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = class.num)
              #"nthread" = 7)


#cv.res = xgb.cv(param,data = x, nfold = 5, label = trainy, nround = 2)

# Train the model
nround = 5
bst = xgboost(param=param, data = x, label = trainy, nrounds=nround)


###########################################################
#Test and Check for Muliclass Log loss

# test <- as.matrix(test)
# test = matrix(as.numeric(test),nrow(test),ncol(test))
# 
# # Make prediction
# pred = predict(bst,test)
# head(pred)
# pred = matrix(pred,class.num,length(pred)/class.num)
# pred = t(pred)
# 
# pred = matrix(as.numeric(pred),nrow(pred),ncol(pred))
# 
# colnames(pred) <- paste("TripType", class.levels , sep = "_")

######################################################################
#Predict

test <- as.matrix(test)
test = matrix(as.numeric(test),nrow(test),ncol(test))

# Make prediction
xgb.pred = predict(bst,test)

xgb.pred = matrix(xgb.pred,class.num,length(xgb.pred)/class.num)
xgb.pred = t(xgb.pred)



######################################################################
#Save  to CSV

xgb.pred <- round(xgb.pred, digits = 2)

colnames(xgb.pred) <- paste("TripType", class.levels , sep = "_")

xgb.pred <- cbind(VisitNumber,xgb.pred)


#xgb.pred <- format(xgb.pred, sci = FALSE) 


write.table(xgb.pred,"simplexgbpred.csv",sep = ",",row.names = FALSE,col.names = TRUE)

######################################################################
######################################################################
