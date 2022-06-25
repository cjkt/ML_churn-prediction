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
library(mlbench)
library(tree)
library(e1071)

### Decision tree_1

setwd("")
df <- read.csv("churn-data_imputated_2.csv")
summary(df)
glimpse(df)
dim(df)

df <- df %>% dplyr::select(-X, -models_used) 

df$cred_score <- ifelse(df$cred_score == 'A'|
                            df$cred_score == 'A2'|
                            df$cred_score == 'A3'|
                            df$cred_score == 'AA', 'A', 
                          ifelse(df$cred_score == 'B'|
                                   df$cred_score == 'B2'|
                                   df$cred_score == 'BA', 'B', 'C'))
df$handset_age <- ifelse(df$handset_age < 0, NA, df$handset_age)

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
df[,"cred_score"] = as.factor(df$cred_score)
df[,"churn"] = as.factor(df$churn)

## NA
colSums(is.na(df))
df_1<-na.omit(df)
dim(df_1)
summary(df_1)

# data balance
summary(as.factor(df_1$churn))

# DT_1

# data split: train and test
set.seed(12)
indices = sample.split(df_1$churn, SplitRatio = 0.7)
train = df_1[indices,]
test = df_1[!(indices),]
dim(train)
dim(test)

#test <- test %>% dplyr::select(-churn)
glimpse(test)

# model fitting
treemod<-tree(churn~. , data=train)
plot(treemod)
text(treemod)

# pruning
cv.trees<-cv.tree(treemod, FUN=prune.misclass ) # for classification decision tree
plot(cv.trees)

prune.trees <- prune.misclass(treemod, best=2)  # for regression decision tree, use prune.tree function
plot(prune.trees)
text(prune.trees, pretty=0)

ggplot(df, aes(churn, handset_age))+
  geom_boxplot()

tapply(df$handset_age, df$churn, summary)

# prediction
treepred <- predict(prune.trees, test, type='class')
confusionMatrix(treepred, test$churn)






# model fit_2: handset_age removed
## NA
colSums(is.na(df))
df_2<-na.omit(df)
dim(df_2)
summary(df_2)

df_2 <- df %>% dplyr::select(-handset_age) 

set.seed(1352)
indices = sample.split(df$churn, SplitRatio = 0.7)
train_2  = df_2[indices,]
test_2 = df_2[!(indices),]
dim(train_2 )
dim(test_2)

#test_2 <- test_2 %>% dplyr::select(-churn)
glimpse(test_2)

# model fitting
treemod<-tree(churn~. , data=train_2 )
plot(treemod)
text(treemod)

# pruning
cv.trees<-cv.tree(treemod, FUN=prune.misclass ) # for classification decision tree
plot(cv.trees)

prune.trees <- prune.misclass(treemod, best=2)  # for regression decision tree, use prune.tree function
plot(prune.trees)
text(prune.trees, pretty=0)

ggplot(df, aes(churn, handset_age))+
  geom_boxplot()

tapply(df$handset_age, df$churn, summary)

# prediction
treepred <- predict(prune.trees, test_2, type='class')
confusionMatrix(treepred, test_2$churn)



# model fit_3: numeric data removed
summary(df)
df_3 <- df %>% dplyr::select(-user_months,-phones_used,-handset_price,-handset_age)
summary(df_3)

set.seed(345)
indices = sample.split(df$churn, SplitRatio = 0.7)
train_3  = df_3[indices,]
test_3 = df_3[!(indices),]
dim(train_3 )
dim(test_3)

#test_3 <- test_3 %>% dplyr::select(-churn)
glimpse(test_3)

# model fitting
treemod<-tree(churn~. , data=train_3 )
plot(treemod)
text(treemod)

# pruning
cv.trees<-cv.tree(treemod, FUN=prune.misclass ) # for classification decision tree
plot(cv.trees)

prune.trees <- prune.misclass(treemod, best=2)  # for regression decision tree, use prune.tree function
plot(prune.trees)
text(prune.trees, pretty=0)

ggplot(df, aes(churn, handset_age))+
  geom_boxplot()

tapply(df$handset_age, df$churn, summary)

# prediction
treepred <- predict(prune.trees, test_3, type='class')
confusionMatrix(treepred, test_3$churn)
