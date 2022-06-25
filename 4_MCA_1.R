#install.packages("lapply")

# setting
library(dplyr)
library(readxl)
library(ggplot2)
#library(lapply)
library(corrplot)
library(car)
library(vcd)
library(DescTools)

library(FactoMineR)
library(factoextra)


setwd("")
df <- read_excel("churn-data_preprocess_1.xlsx")

# NA to blank
df$web_capable[df$web_capable == "NA"] <- NA

# drop NA

# data transform
df[,"Customer_ID"] = as.character(df$Customer_ID)
df[,"area"] = as.factor(df$area)
df[,"user_months"] = as.numeric(df$user_months)
df[,"new_user"] = as.factor(df$new_user)
df[,"phones_used"] = as.numeric(df$phones_used)
df[,"models_used"] = as.numeric(df$models_used)
df[,"handset_price"] = as.numeric(df$handset_price)
df[,"handset_age"] = as.numeric(df$handset_age)
df[,"refurb_or_new"] = as.factor(df$refurb_or_new)
df[,"dualband"] = as.factor(df$dualband)
df[,"web_capable"] = as.factor(df$web_capable)
df[,"manual_limit"] = as.logical(df$manual_limit)
df[,"PRIZM_code"] = as.factor(df$PRIZM_code)
df[,"credit_card"] = as.logical(df$credit_card)
df[,"cred_score"] = as.factor(df$cred_score)
df[,"churn"] = as.factor(df$churn)


# transform churn variable
df$churn_text <- ifelse(df$churn == 1, 'churn', 'non-churn')
df_mca <- df %>%
  select(-churn) %>%
  rename('churn' = 'churn_text')

df_1 <- df_mca %>%
  select(web_capable, credit_card, churn)

# MCA
# data summary
summary(df_mca)
glimpse(df_mca)

result <- MCA(df_1, graph=FALSE)
res_val <- get_eigenvalue(result)
fviz_screeplot(result, addlabels=TRUE, ylim=c(0,45))

fviz_mca_biplot(result, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
