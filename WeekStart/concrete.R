# Clear Envierment.
rm(list = ls())
# My support functions.
source("myScripts.R")

# Libs used.
library(XLConnect)

# Functions

## START - 

# Load workbook
wb <- loadWorkbook("Concrete_Data.xls")
concrete <- readWorksheet(wb, sheet = "Sheet1")
rm(wb)
names <- c('Cement','FurnaceSlag','FlyAsh','Water','Superplasticizer','CoarseAggregate','FireAggregate','Age','ConcreteCompressive')
colnames(concrete) <- names
cor(concrete)
con_set <- splitTrainSet(concrete)
lm_model <- lm(Age ~ ., data = con_set$test)
summary(lm_model)
lm_pred <- predict(lm_model, con_set$test[,-8])
er <- MAE(con_set$test$Age, lm_pred)
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

gplay <- dplyr:::filter(gplay, Rating != '19')

# M5P - Prediction.
library(RWeka)
M5_Model <- M5P(Age ~ ., data = con_set$test)
M5_Prediction <- predict(M5_Model, con_set$test[, -8])
MAE(con_set$test$Age, M5_Prediction)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100, 1)

# R-Part
library(rpart)
rp_model <- rpart(Age ~ ., data = con_set$test)
rp_pred <- predict(rp_model, con_set$test[, -8])
MAE(con_set$test$Age, rp_pred)
span <- abs(max(concrete$Age) - min(concrete$Age))
round((er / span) * 100, 1)


library(psych)
cor(concrete)
pairs.panels(concrete[vars])