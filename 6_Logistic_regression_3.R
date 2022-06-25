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

### logistic regression: encoding_1

setwd("")
df <- read.csv("churn-data_6_not_encoded.csv")
summary(df)
glimpse(df)
dim(df)

df <- df %>% dplyr::select(-X)

## NA
colSums(is.na(df))
df_1<-na.omit(df)
dim(df_1)

# scaling
preproc1 <- preProcess(df_1, method=c("range"))
norm1 <- predict(preproc1, df_1)
summary(norm1)

df_1<-norm1

# data balance
summary(as.factor(df_1$churn))
46812/92880
46068/92880

# sampling
df_1<- df_1[sample(nrow(df_1), 20000), ]
dim(df_1)

# data format

# data split: train and test
set.seed(12)
indices = sample.split(df_1$churn, SplitRatio = 0.8)
train = df_1[indices,]
test = df_1[!(indices),]
dim(train)
dim(test)

## model fitting_1
summary(df_1)
str(df_1)
logi_1 <- glm(churn ~.,data=train, family=binomial()) 
summary(logi_1)

# feature selection
logi_2 <- stepAIC(logi_1, direction="backward")
summary(logi_2)   

# multicolinearity
vif(logi_2) > 2 # user_months, phones_used, handset_price

# prediction
pred <- predict(logi_2, type = "response", newdata = test)
summary(pred)
test$prob <- pred

# cut off: 0.5 
pred_churn <- factor(ifelse(pred >= 0.50,'yes', 'no'))
actual_churn <- factor(ifelse(test$churn == 1, 'yes', 'no'))
table(actual_churn,pred_churn)

# accuracy, sensitivitiy, specifity
#cutoff_churn <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(pred_churn, actual_churn, mode="everything",positive='yes')
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

conf_final
