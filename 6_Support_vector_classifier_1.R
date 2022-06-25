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
library(e1071)

### SVM_1

setwd("C:/Users/mindt/OneDrive/Sweden/Study/DU/Business Intelligence/THESIS")
df <- read.csv("churn-data_encoded_1.csv")
summary(df)
glimpse(df)
dim(df)

df <- df %>% dplyr::select(-X, -models_used )

## NA
colSums(is.na(df))
df_1<-na.omit(df)
dim(df_1)
summary(df_1)
str(df_1)
colSums(is.na(df_1))

# data balance
summary(as.factor(df_1$churn))

# data normalization
norm <- caret::preProcess(df_1, method='range')
df_2 <- predict(norm,df_1)
glimpse(df_2)

# data split: train and test
set.seed(12)
indices = sample.split(df_1$churn, SplitRatio = 0.7)
train = df_1[indices,]
test = df_1[!(indices),]
dim(train)
dim(test)


# model: basic
svm_r <- svm(churn~., data=train, kernel='radial',type='C-classification')
svm_l <- svm(churn~., data=train, kernel='linear',type='C-classification')
svm_p <- svm(churn~., data=train, kernel='polynomial',type='C-classification')
svm_s <- svm(churn~., data=train, kernel='sigmoid',type='C-classification')

print(svm_r)
print(svm_l)
print(svm_p)
print(svm_s)

# prediction
pred_r <- predict(svm_r, newdata=test[,-37])
pred_l <- predict(svm_l, newdata=test[,-37])
pred_p <- predict(svm_p, newdata=test[,-37])
pred_s <- predict(svm_s, newdata=test[,-37])

confusionMatrix(as.factor(pred_r),as.factor(test$churn))
confusionMatrix(pred_l,as.factor(test$churn))
confusionMatrix(pred_p,as.factor(test$churn))
confusionMatrix(pred_s,as.factor(test$churn))

# model eval
ROC(test=test, stat=test$churn, plot="ROC", AUC=T, main="SVM")

# tuning
tune_r <- tune(svm_r, churn~.,
               data=train,
               kernel='radial',
               ranges=list(cost=10^(-1:2), gamma=(0.5,2)))

# 
svm.predictions.best <- predict(svm.model.best, test.feature.vars, decision.value=T)
svm.prediction.values <- attributes(svm.predictions.best)$decision.values
predictions <- prediction(svm.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
}
plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
}
plot.roc.curve(predictions, title.text = "SVM ROC Curve")
plot.pr.curve(predictions, title.text = "SVM Precision/Recall Curve")



