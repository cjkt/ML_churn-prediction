# setting
library(dplyr)
library(readxl)
library(ggplot2)
#library(lapply)
library(corrplot)
library(car)
library(vcd)
library(DescTools)
library(VIM)
library(missForest)
library(mice)
library(caret)
library(MASS)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tidyverse)
library(tidymodels)
library(rattle) 
library(RColorBrewer) 
library(data.table) 
library(scales) 
library(ROCR) 
library(ineq)

### Random forest
setwd("")
df <- read.csv("churn-data_6_not_encoded.csv")
summary(df)
glimpse(df)
dim(df)

#df <- df %>% dplyr::select(-Customer_ID, -models_used, -X)
#df$handset_age <- ifelse(df$handset_age < 0, NA, df$handset_age)

# data transform
df[,"area"] = as.factor(df$area)
df[,"user_months"] = as.numeric(df$user_months)
df[,"phones_used"] = as.numeric(df$phones_used)
#df[,"models_used"] = as.numeric(df$models_used)
df[,"handset_price"] = as.numeric(df$handset_price)

df[,"handset_price"] = round(df$handset_price)

df[,"handset_age"] = as.numeric(df$handset_age)
df[,"refurb_or_new"] = as.factor(df$refurb_or_new)
df[,"dualband"] = as.factor(df$dualband)
df[,"web_capable"] = as.factor(df$web_capable)
df[,"manual_limit"] = as.logical(df$manual_limit)
df[,"PRIZM_code"] = as.factor(df$PRIZM_code)
df[,"credit_card"] = as.logical(df$credit_card)
df[,"cred_score"] = as.numeric(df$cred_score)
df[,"churn"] = as.factor(df$churn)

## NA
colSums(is.na(df))
df<-na.omit(df)
dim(df)
summary(df)
glimpse(df)

prop.table(table(df$churn))

df <- df %>% dplyr::select(-X)

# data random sampling
df <- dplyr::sample_n(df, 10000)
summary(df)
dim(df)

# data split: train and test
set.seed(12)
indices = sample.split(df$churn, SplitRatio = 0.8)
train = df[indices,]
test = df[!(indices),]
dim(train)
dim(test)

prop.table(table(train$churn))
prop.table(table(test$churn))

# tuning
set.seed(1)
bestMtry <- tuneRF(df[,-14],df[,14], stepFactor = 0.5,ntree = 500)
print(bestMtry)

# modeling
model <- randomForest(churn~., data=train, ntree=300, mtry=3)
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "0", "1"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"0"], 
          model$err.rate[,"1"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

varImpPlot(model)

# prediction
prediction_for_table <- predict(model,test[,-14])
table(observed=as.factor(test[,14]),predicted=as.factor(prediction_for_table))
confusionMatrix(test[,14],as.factor(prediction_for_table))


## modeling: only categorical
summary(df)
df <- df%>%dplyr::select(-user_months,-phones_used,-handset_price,
                         -handset_age,-cred_score)
# data split: train and test
set.seed(12)
indices = sample.split(df$churn, SplitRatio = 0.8)
train = df[indices,]
test = df[!(indices),]
dim(train)
dim(test)

prop.table(table(train$churn))
prop.table(table(test$churn))

# tuning
set.seed(1)
dim(df)
bestMtry <- tuneRF(df[,-9],df[,9], stepFactor = 0.5,ntree = 500)
print(bestMtry)

# modeling
model <- randomForest(churn~., data=train, ntree=300, mtry=2)
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "0", "1"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"0"], 
          model$err.rate[,"1"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

varImpPlot(model)

# prediction
prediction_for_table <- predict(model,test[,-9])
table(observed=as.factor(test[,9]),predicted=as.factor(prediction_for_table))
confusionMatrix(test[,9],as.factor(prediction_for_table))





## modeling: only numeric

### Random forest
setwd("")
df <- read.csv("churn-data_6_not_encoded.csv")
summary(df)
glimpse(df)
dim(df)

#df <- df %>% dplyr::select(-Customer_ID, -models_used, -X)
#df$handset_age <- ifelse(df$handset_age < 0, NA, df$handset_age)

# data transform
df[,"area"] = as.factor(df$area)
df[,"user_months"] = as.numeric(df$user_months)
df[,"phones_used"] = as.numeric(df$phones_used)
#df[,"models_used"] = as.numeric(df$models_used)
df[,"handset_price"] = as.numeric(df$handset_price)

df[,"handset_price"] = round(df$handset_price)

df[,"handset_age"] = as.numeric(df$handset_age)
df[,"refurb_or_new"] = as.factor(df$refurb_or_new)
df[,"dualband"] = as.factor(df$dualband)
df[,"web_capable"] = as.factor(df$web_capable)
df[,"manual_limit"] = as.logical(df$manual_limit)
df[,"PRIZM_code"] = as.factor(df$PRIZM_code)
df[,"credit_card"] = as.logical(df$credit_card)
df[,"cred_score"] = as.numeric(df$cred_score)
df[,"churn"] = as.factor(df$churn)

## NA
colSums(is.na(df))
df<-na.omit(df)
dim(df)
summary(df)
glimpse(df)
summary(df)
df <- df%>%dplyr::select(user_months,phones_used,handset_price,
                         handset_age,cred_score,churn)
# data split: train and test
set.seed(12)
indices = sample.split(df$churn, SplitRatio = 0.8)
train = df[indices,]
test = df[!(indices),]
dim(train)
dim(test)

prop.table(table(train$churn))
prop.table(table(test$churn))

# tuning
set.seed(1)
dim(df)
bestMtry <- tuneRF(df[,-6],df[,6], stepFactor = 0.5,ntree = 500)
print(bestMtry)

# modeling
model <- randomForest(churn~., data=train, ntree=300, mtry=2)
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "0", "1"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"0"], 
          model$err.rate[,"1"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

varImpPlot(model)

# prediction
prediction_for_table <- predict(model,test[,-6])
table(observed=as.factor(test[,6]),predicted=as.factor(prediction_for_table))
confusionMatrix(test[,6],as.factor(prediction_for_table))
