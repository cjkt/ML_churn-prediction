library(dplyr)

# DFS done
setwd("")
df <- read.csv("churn-data_9_DFS_no-selection.csv")

dim(df)
summary(df)

df <- df %>% dplyr::select(-X,-Customer_ID,-churn)

pca <- prcomp(df)
sum<- summary(pca)
summary(sum)
sum$importance

screeplot(pca,type="lines",pch=1,main="scree plot")
pca$rotation[,1:1]

biplot(pca,main="Biplot")

# DFS not done
df2 <- read.csv("churn-data_6_encoded.csv")

summary(df2)
df2 <- df2 %>% dplyr::select(-X,-churn)
df2<-na.omit(df2)
pca2 <- prcomp(df2)
sum2<- summary(pca2)

screeplot(pca2,type="lines",pch=1,main="scree plot")
biplot(pca2,main="Biplot")
