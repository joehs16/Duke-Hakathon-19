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

train = read.csv("training_tallskinny.csv")

# Almost a million rows

