# Setup ------------------------

library(formatR)
library(ggplot2)
library(ggfortify)
library(quantreg)
library(gridExtra)
library(Hmisc)
library(corrplot)
library(GGally)
library(caret)
library(psych)
library(car)
library(huxtable)
library(stargazer)
library(DataExplorer)
library(GGally)
library(MASS)
library(data.table)
library(e1071)
library(pROC)
library(tidyverse)
library(arm)
library(lme4)
library(lmerTest)
library(ggpubr)
library(readxl)
library(readr)
library(stringr)
library(tm)
library(tidytext)
library(RWeka)
library(randomForest)
library(mlbench)
library(gbm)
library(fastAdaboost)
library(ada)
library(C50)

setwd("C:/Users/Juan David Martínez/Desktop/Datathon data/Duke Datathon 10-29-19/valassis_tallskinny")

options(scipen=999)


# Data cleanning -----------------

train_original <- read.csv("training_tallskinny.csv")

train <- train_original

train[is.na(train)] <- 0

# Almost 9 million rows

# How many have both long term and short term values for the interest

train_long_short <- train[(!is.na(train$stiFeatures) & !is.na(train$ltiFeatures)),]

# 1.5 million have both

# How many have only short

# train_short <- train[(!is.na(train$stiFeatures) & is.na(train$ltiFeatures)),]

train_short <- train[(!is.na(train$stiFeatures)),]


prop.table(table(train_short$inAudience))

# 4.1 million

# How many have only long

# train_long <- train[(!is.na(train$ltiFeatures) & is.na(train$stiFeatures)),]

train_long <- train[(!is.na(train$ltiFeatures)),]

# 4.3 million

prop.table(table(train$inAudience))

# Total true 172433 

prop.table(table(train_long_short$inAudience))


# Both: 21715

prop.table(table(train_long$inAudience))


# Long: 98087

prop.table(table(train_short$inAudience))


# Short: 96061






# Take sample of users

ids <- unique(train$userID)

# How many users there are?

length(ids)

set.seed(3)

sample_ids <- sample(ids, 1000)

train_sample <- train[which(train$userID %in% sample_ids),]

summary(train_sample)

max_lt <- setNames(aggregate(ltiFeatures ~ userID, train_sample, max), c("userID", "max_lt_value"))

max_st <- setNames(aggregate(stiFeatures ~ userID, train_sample, max), c("userID", "max_st_value"))

max_topic_lt <- setNames(aggregate(ltiFeatures ~ topic_id, train_sample, max), c("topic_id", "topic_max_lt_value"))

max_topic_st <- setNames(aggregate(stiFeatures ~ topic_id, train_sample, max), c("topic_id", "topic_max_st_value"))

num_topics_lt <- setNames(aggregate(topic_id ~ userID, train_sample[!is.na(train_sample$ltiFeatures),], FUN=function(x) length(unique(x))), c("userID", "num_lt_topics"))

num_topics_st <- setNames(aggregate(topic_id ~ userID, train_sample[!is.na(train_sample$stiFeatures),], FUN=function(x) length(unique(x))), c("userID", "num_st_topics"))

sum_lt <- setNames(aggregate(ltiFeatures ~ userID, train_sample, sum), c("userID", "sum_lt"))

sum_st <- setNames(aggregate(stiFeatures ~ userID, train_sample, sum), c("userID", "sum_st"))





train_full <- train_sample %>% inner_join(max_lt, by = "userID")

train_full <- train_full %>% inner_join(max_st, by = "userID")

train_full <- train_full %>% inner_join(max_topic_lt, by = "topic_id")

train_full <- train_full %>% inner_join(max_topic_st, by = "topic_id")

train_full <- train_full %>% inner_join(num_topics_lt, by = "userID")

train_full <- train_full %>% inner_join(num_topics_st, by = "userID")




# New variables

train_full$user_st.ratio <- (train_full$stiFeatures/train_full$max_st_value)

train_full$user_lt.ratio <- (train_full$ltiFeatures/train_full$max_lt_value)

train_full$user_st.ratio[is.na(train_full$user_st.ratio)] <- 0

train_full$user_lt.ratio[is.na(train_full$user_lt.ratio)] <- 0

summary(train_full)

train_full$user_rd <- train_full$user_st.ratio - train_full$user_lt.ratio

train_full$topic_st.ratio <- (train_full$stiFeatures/train_full$topic_max_st_value)

train_full$topic_lt.ratio <- (train_full$ltiFeatures/train_full$topic_max_lt_value)

train_full$topic_st.ratio[is.na(train_full$topic_st.ratio)] <- 0

train_full$topic_lt.ratio[is.na(train_full$topic_lt.ratio)] <- 0

train_full$topic_rd <-  train_full$topic_st.ratio - train_full$topic_lt.ratio

summary(train_full)

str(train_full)


# inAudience ~ user_rd + topic_rd

control <- trainControl(method="repeatedcv", number=10, repeats=3)

# take inAudience = True

train_full_True <- train_full[which(train_full$inAudience == "True"),]

train_full_False <- train_full[which(train_full$inAudience == "False"),]

train_full_False_sample <- sample_n(train_full_False, 2775)
  
train_full_balanced <- rbind(train_full_True, train_full_False_sample)  



logit1 <- train(inAudience ~ user_rd + topic_rd, data=train_full_balanced, method="LogitBoost", 
                  trControl=control)

logit1


rf <- train(inAudience ~ user_rd + topic_rd, data=train_full_balanced, method="rf", 
                trControl=control)

rf


svm <- train(inAudience ~ user_rd + topic_rd, data=train_full_balanced, method="svmPoly", 
            trControl=control)

svm



# load validation set -----------------

validation <- read.csv("validation_tallskinny.csv")


max_lt <- setNames(aggregate(ltiFeatures ~ userID, validation, max), c("userID", "max_lt_value"))

max_st <- setNames(aggregate(stiFeatures ~ userID, validation, max), c("userID", "max_st_value"))

max_topic_lt <- setNames(aggregate(ltiFeatures ~ topic_id, validation, max), c("topic_id", "topic_max_lt_value"))

max_topic_st <- setNames(aggregate(stiFeatures ~ topic_id, validation, max), c("topic_id", "topic_max_st_value"))

num_topics_lt <- setNames(aggregate(topic_id ~ userID, validation[!is.na(validation$ltiFeatures),], FUN=function(x) length(unique(x))), c("userID", "num_lt_topics"))

num_topics_st <- setNames(aggregate(topic_id ~ userID, validation[!is.na(validation$stiFeatures),], FUN=function(x) length(unique(x))), c("userID", "num_st_topics"))

sum_lt <- setNames(aggregate(ltiFeatures ~ userID, validation, sum), c("userID", "sum_lt"))

sum_st <- setNames(aggregate(stiFeatures ~ userID, validation, sum), c("userID", "sum_st"))

test_full <- validation %>% inner_join(max_lt, by = "userID")

test_full <- test_full %>% inner_join(max_st, by = "userID")

test_full <- test_full %>% inner_join(max_topic_lt, by = "topic_id")

test_full <- test_full %>% inner_join(max_topic_st, by = "topic_id")

test_full <- test_full %>% inner_join(num_topics_lt, by = "userID")

test_full <- test_full %>% inner_join(num_topics_st, by = "userID")

# New variables

test_full$user_st.ratio <- (test_full$stiFeatures/test_full$max_st_value)

test_full$user_lt.ratio <- (test_full$ltiFeatures/test_full$max_lt_value)

test_full$user_st.ratio[is.na(test_full$user_st.ratio)] <- 0

test_full$user_lt.ratio[is.na(test_full$user_lt.ratio)] <- 0

test_full$user_rd <- test_full$user_st.ratio - test_full$user_lt.ratio

test_full$topic_st.ratio <- (test_full$stiFeatures/test_full$topic_max_st_value)

test_full$topic_lt.ratio <- (test_full$ltiFeatures/test_full$topic_max_lt_value)

test_full$topic_st.ratio[is.na(test_full$topic_st.ratio)] <- 0

test_full$topic_lt.ratio[is.na(test_full$topic_lt.ratio)] <- 0

test_full$topic_rd <-  test_full$topic_st.ratio - test_full$topic_lt.ratio

# predict -------------------

test_full_sample <- sample_n(test_full, 5000)

rf_predict <- predict(rf, test_full_sample)

confusionMatrix(rf_predict, test_full_sample$inAudience)


