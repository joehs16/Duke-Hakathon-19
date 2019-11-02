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

train <- read.csv("training_tallskinny.csv")

# Almost 9 million rows

# How many have both long term and short term values for the interest

train_long_short <- train[complete.cases(train), ]

# 1.5 million have both

# How many have only short

train_short <- train[(!is.na(train$stiFeatures) & is.na(train$ltiFeatures)),]

# 4.1 million

# How many have only long

train_long <- train[(!is.na(train$ltiFeatures) & is.na(train$stiFeatures)),]

# 4.3 million



# Take sample of users

ids <- unique(train$userID)

# How many users there are?

length(ids)

set.seed(3)

sample_ids <- sample(ids, 100)

train_sample <- train[which(train$userID %in% sample_ids),]

summary(train_sample)

max_lt <- setNames(aggregate(ltiFeatures ~ userID, train_sample, max), c("userID", "max_lt_value"))

max_st <- setNames(aggregate(stiFeatures ~ userID, train_sample, max), c("userID", "max_st_value"))

topic_lt <- setNames(aggregate(ltiFeatures ~ topic_id, train_sample, mean), c("topic_id", "topic_avg_lt_value"))

topic_st <- setNames(aggregate(stiFeatures ~ topic_id, train_sample, mean), c("topic_id", "topic_avg_st_value"))

num_topics_lt <- setNames(aggregate(topic_id ~ userID, train_sample[!is.na(train_sample$ltiFeatures),], FUN=function(x) length(unique(x))), c("userID", "num_lt_topics"))

num_topics_st <- setNames(aggregate(topic_id ~ userID, train_sample[!is.na(train_sample$stiFeatures),], FUN=function(x) length(unique(x))), c("userID", "num_st_topics"))

train_full <- train_sample %>% inner_join(max_lt, by = "userID")

train_full <- train_full %>% inner_join(max_st, by = "userID")

train_full <- train_full %>% inner_join(topic_lt, by = "topic_id")

train_full <- train_full %>% inner_join(topic_st, by = "topic_id")

train_full <- train_full %>% inner_join(num_topics_lt, by = "userID")

train_full <- train_full %>% inner_join(num_topics_st, by = "userID")

# New variables

train_full$user_rd <- (train_full$stiFeatures/train_full$max_st_value) - (train_full$ltiFeatures/train_full$max_lt_value)

train_full$topic_rd <- (train_full$stiFeatures/train_full$topic_avg_st_value) - (train_full$ltiFeatures/train_full$topic_avg_lt_value)


# length(unique(train[which(train$userID == 519 & !is.na(train_sample$ltiFeatures)),]$topic_id))



