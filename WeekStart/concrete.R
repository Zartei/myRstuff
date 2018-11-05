# Clear Envierment.
rm(list = ls())
# My support functions.
source("myScripts.R")

# Libs used.
library(XLConnect)
library(RWeka)
library(rpart)
library(psych)

# Functions

# Variables
input <- "Concrete_Data.xls"


# Start.

# Load workbook
wb <- loadWorkbook(input)
concrete <- readWorksheet(wb, sheet = "Sheet1")
rm(wb)
names <- c('Cement','FurnaceSlag','FlyAsh','Water','Superplasticizer','CoarseAggregate','FireAggregate','Age','ConcreteCompressive')
colnames(concrete) <- names
cor(concrete)
pairs.panels(concrete[vars])

con_set <- splitTrainSet(concrete)



lm_model <- lm(Age ~ ., data = con_set$test)
# summary(lm_model)
lm_pred <- predict(lm_model, con_set$test[,-8])
MAE(con_set$test$Age, lm_pred)
MAE(mean(concrete$Age), plsClasses)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100,1)

vars <- c('Cement', 'Water', 'ConcreteCompressive', 'Age')
vars <- c('ConcreteCompressive', 'Age')
cor(concrete)
lm_model <- lm(Age ~ ., data = con_set$test[vars])
summary(lm_model)
lm_model
lm_pred <- predict(lm_model, con_set$test[9])
er <- MAE(con_set$test$Age, lm_pred)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100, 1)
er <- MAE(mean(concrete$Age), lm_pred)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100, 1)

concrete <- dplyr:::filter(concrete, Age <= 100)

# M5P - Prediction.
M5_Model <- M5P(Age ~ ., data = con_set$test)
M5_Prediction <- predict(M5_Model, con_set$test[, -8])
MAE(con_set$test$Age, M5_Prediction)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100, 1)

# R-Part
rp_model <- rpart(Age ~ ., data = con_set$test)
rp_pred <- predict(rp_model, con_set$test[, -8])
MAE(con_set$test$Age, rp_pred)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100, 1)

