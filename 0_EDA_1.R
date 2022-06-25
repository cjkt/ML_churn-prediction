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

setwd("")
df <- read_excel("churn-data_preprocess_1.xlsx")

summary(df)
# NA to blank
df$web_capable[df$web_capable == "NA"] <- NA

# drop NA


# transform churn variable
df$churn_text <- ifelse(df$churn == 1, 'churn', 'non-churn')
df <- df %>%
  select(-churn) %>%
  rename('churn' = 'churn_text')

# summary
summary(df)
glimpse(df)
dim(df)
sum(is.na(df))
colSums(is.na(df))
class(df)

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

summary(df)


# summary: variables
summary(df$Customer_ID)
summary(df$area)
summary(df$user_months)
summary(df$new_user)
summary(df$phones_used)
summary(df$models_used)
summary(df$handset_price)
summary(df$handset_age)
summary(df$refurb_or_new)
summary(df$dualband)
summary(df$web_capable)
summary(df$manual_limit)
summary(df$PRIZM_code)
summary(df$credit_card)
summary(df$cred_score)
summary(df$churn)

glimpse(df)
summary(df)

# summary: churn in variables
tapply(df$area, df$churn, summary)
tapply(df$user_months, df$churn, summary)
tapply(df$new_user, df$churn, summary)
tapply(df$phones_used, df$churn, summary)
tapply(df$models_used, df$churn, summary)
tapply(df$handset_price, df$churn, summary)
tapply(df$handset_age, df$churn, summary)
tapply(df$refurb_or_new, df$churn, summary)
tapply(df$dualband, df$churn, summary)
tapply(df$web_capable, df$churn, summary)
tapply(df$manual_limit, df$churn, summary)
tapply(df$PRIZM_code, df$churn, summary)
tapply(df$credit_card, df$churn, summary)
tapply(df$cred_score, df$churn, summary)

# plots
# area
ggplot(df, aes(area))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(df, aes(area, fill=churn))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

mosaic(churn ~ area, data=df, direction='v', rot_labels=c(90,0,0,0))

# user_months
ggplot(df, aes(user_months))+
  geom_boxplot()

ggplot(df, aes(churn, user_months))+
  geom_boxplot()+
  scale_y_log10()

# n of churn by user_months
df_month <- df %>%
  select(user_months, churn) %>%
  filter(churn == 1)
ggplot(df, aes(user_months))+
  geom_bar()

# new_user
ggplot(df, aes(new_user))+
  geom_bar()

ggplot(df, aes(new_user, fill=churn))+
  geom_bar()

mosaic(churn ~ new_user, data=df, direction='v')

# phones_used
ggplot(df, aes(phones_used))+
  geom_boxplot()

ggplot(df, aes(phones_used))+
  geom_boxplot()+
  scale_x_log10()

ggplot(df, aes(churn, phones_used))+
  geom_boxplot()+
  scale_y_log10()

# models_used
ggplot(df, aes(models_used))+
  geom_boxplot()

ggplot(df, aes(churn, models_used))+
  geom_boxplot()

ggplot(df, aes(churn, models_used))+
  geom_violin()

# handset_price
ggplot(df, aes(handset_price))+
  geom_boxplot()

ggplot(df, aes(churn, handset_price))+
  geom_boxplot()+
  scale_y_log10()

# handset_age
ggplot(df, aes(handset_age))+
  geom_boxplot()

ggplot(df, aes(churn, handset_age))+
  geom_boxplot()+
  scale_y_log10()

ggplot(df, aes(x=handset_age, y=churn))+
  geom_violin()

# refurb_or_new
ggplot(df, aes(refurb_or_new))+
  geom_bar()

ggplot(df, aes(refurb_or_new, fill=churn))+
  geom_bar()

mosaic(churn ~ refurb_or_new, data=df, direction='v')

# dualband
ggplot(df, aes(dualband))+
  geom_bar()

ggplot(df, aes(dualband, fill=churn))+
  geom_bar()

mosaic(churn ~ dualband, data=df, direction='v')

# web_capable
ggplot(df, aes(web_capable, fill=churn))+
  geom_bar()

mosaic(churn ~ web_capable, data=df, direction='v')

# PRIZM_code
ggplot(df, aes(PRIZM_code, fill=churn))+
  geom_bar()

mosaic(churn ~ PRIZM_code, data=df, direction='v')

# credit_card
ggplot(df, aes(credit_card, fill=churn))+
  geom_bar

mosaic(churn ~ credit_card, data=df, direction='v')

# cred_score
ggplot(df,aes(cred_score, fill=churn))+
  geom_bar()

mosaic(churn ~ cred_score, data=df, direction='v')


# correlation analysis: pearson
# user_months, phones_used, models_used, handset_price, handset_age, churn
glimpse(df)
summary(df)

df_cor <- df%>%
  select(user_months,phones_used, models_used, handset_price,
         handset_age, churn)
df_cor$churn <- as.numeric(df_cor$churn)
summary(df_cor)
sum(is.na(df_cor))
df_cor <- na.omit(df_cor)

x <- cor(df_cor)
x

corrplot(x,
         method = 'color',
         addCoef.col = "black")

# models_used vs phones_used: 0.89
ggplot(df_cor, aes(models_used, phones_used))+
  geom_count()
# handset_age vs user_months: 0.49
ggplot(df_cor, aes(handset_age, user_months))+
  geom_count()
# handset_age vs handset_price: -0.48
ggplot(df_cor, aes(handset_age, handset_price))+
  geom_count()


# correlation analysis: spearman
cor(df, method = 'spearman', use='complete.obs')





# Chi-square test
df_chi <- df

df_chi[,"user_months"] = as.factor(df_chi$user_months)
df_chi[,"phones_used"] = as.factor(df_chi$phones_used)
df_chi[,"handset_price"] = as.factor(df_chi$handset_price)
df_chi[,"handset_age"] = as.factor(df_chi$handset_age)

chisq.test(df_chi$area, df_chi$churn)
chisq.test(df_chi$user_months, df_chi$churn)
chisq.test(df_chi$new_user, df_chi$churn)
chisq.test(df_chi$phones_used, df_chi$churn)
chisq.test(df_chi$models_used, df_chi$churn)
chisq.test(df_chi$handset_price, df_chi$churn)
chisq.test(df_chi$handset_age, df_chi$churn)
chisq.test(df_chi$refurb_or_new, df_chi$churn)
chisq.test(df_chi$dualband, df_chi$churn)
chisq.test(df_chi$web_capable, df_chi$churn)
chisq.test(df_chi$manual_limit, df_chi$churn)
chisq.test(df_chi$PRIZM_code, df_chi$churn)
chisq.test(df_chi$credit_card, df_chi$churn)
chisq.test(df_chi$cred_score, df_chi$churn)






# correlation analysis: Cramer's V
summary(df)
summary(df$handset_age)
x = subset(df, handset_age < 0)
x$handset_age
df$handset_age

df_cor2 <- df
df_cor2$handset_age[df_cor2$handset_age < 0] = NA

summary(df_cor2)

CramerV(df_cor2$churn, df_cor2$area)
CramerV(df_cor2$user_months, df_cor2$churn)
CramerV(df_cor2$new_user, df_cor2$churn)
CramerV(df_cor2$phones_used, df_cor2$churn)
CramerV(df_cor2$models_used, df_cor2$churn)
CramerV(df_cor2$handset_price, df_cor2$churn)
CramerV(df_cor2$handset_age, df_cor2$churn)
CramerV(df_cor2$refurb_or_new, df_cor2$churn)
CramerV(df_cor2$dualband, df_cor2$churn)
CramerV(df_cor2$web_capable, df_cor2$churn)
CramerV(df_cor2$manual_limit, df_cor2$churn)
CramerV(df_cor2$PRIZM_code, df_cor2$churn)
CramerV(df_cor2$credit_card, df_cor2$churn)
CramerV(df_cor2$cred_score, df_cor2$churn)

char_cor_vars(df_cor2)


# Multiple correspondence analysis (MCA)
df_mca <- df
glimpse(df_mca)

# outlier
df2 <- df

# user_months
ggplot(df, aes(user_months,churn))+
  geom_boxplot()

ggplot(df, aes(user_months,churn))+
  geom_violin()

boxplot(df2$user_months)$out
length(boxplot(df2$user_months)$out)

months <- df2$user_months
z<- as.data.frame(sapply(months, function(months) (abs(months-mean(months))/sd(months))))
summary(z)

# phones_used

# models_used

# handset_price

# handset_age: negative values
length(df2$handset_age[df2$handset_age<0])
df2$handset_age[df2$handset_age<0] = NA
summary(df2$handset_age)
tapply(df2$handset_age, df2$churn, summary)
