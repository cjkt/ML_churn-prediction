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
library(stringr)
library(forcats)
library(tidyr)
library(leaps)

setwd("")
df <- read_excel("churn-data_preprocess_1.xlsx")

summary(df)

df <- df %>% dplyr::select(-models_used)

# data transform

df[,"area"] = as.factor(df$area)
df[,"user_months"] = as.numeric(df$user_months)
df[,"new_user"] = as.factor(df$new_user)
df[,"phones_used"] = as.numeric(df$phones_used)
#df[,"models_used"] = as.numeric(df$models_used)
df[,"handset_price"] = as.numeric(df$handset_price)
df[,"handset_age"] = as.numeric(df$handset_age)
df[,"refurb_or_new"] = as.factor(df$refurb_or_new)
df[,"dualband"] = as.factor(df$dualband)
df[,"web_capable"] = as.factor(df$web_capable)
df[,"manual_limit"] = as.logical(df$manual_limit)
df[,"PRIZM_code"] = as.factor(df$PRIZM_code)
df[,"credit_card"] = as.factor(df$credit_card)
df[,"cred_score"] = as.factor(df$cred_score)
df[,"churn"] = as.factor(df$churn)

# round handset_price
df[,"handset_price"] = round(df$handset_price)

# remove negative in handset_age
# handset age
nrow(df[df$handset_age<0, ])
df$handset_age <- ifelse(df$handset_age < 0, NA, df$handset_age)
table(is.na(df$handset_age))

# web_capable
df[,"web_capable"] = as.character(df$web_capable)
df$web_capable <- ifelse(df$web_capable=='NA'|
                           df$web_capable=='UNKW', 'UNKOWN', df$web_capable)

df[,"web_capable"] = as.factor(df$web_capable)

# NA processing
#colSums(is.na(df))
#df<- df %>% drop_na
#dim(df)

# ordinal coding: cred_score
summary(df$cred_score)

summary(df$cred_score) # A:2, B:1, C:0
df$cred_score <- ifelse(df$cred_score == 'A'|
                                 df$cred_score == 'A2'|
                                 df$cred_score == 'A3'|
                                 df$cred_score == 'AA', 2, 
                               ifelse(df$cred_score == 'B'|
                                        df$cred_score == 'B2'|
                                        df$cred_score == 'BA', 1, 0))


boxplot(df$cred_score)
summary(df)
dim(df)

boxplot(df$user_months)$stats
boxplot(df$handset_age)$stats
boxplot(df$handset_price)$stats
boxplot(df$phones_used)$stats




# outlier removal
# not encoded
write.csv(df, file = "churn-data_8_preDFS_not_encoded.csv")
 
