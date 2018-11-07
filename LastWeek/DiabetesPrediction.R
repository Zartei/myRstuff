#Libs.
library(neuralnet)
library(XLConnect)
library(caret)

# Functions
source("../WeekStart/myScripts.R")

# Variables
input <- "Diabetes.csv"

# Start.
dia <- read.csv(input, sep = ";", encoding="UTF-8")
colnames(dia)[1] <- "Graviditeter"
dia$Diabetes <- as.factor(dia$Diabetes)

# Start to predict with C5.0
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
m <- train(Diabetes ~ .,
           data = dia,
           method = "C5.0",
           trControl = train_control)
m
summary(m$finalModel)
plot(varImp(m,scale = F))

# Treebaging
ctrl <- trainControl(method = "cv", number = 10)
m_tb <- train(
    Diabetes ~ .,
    data = dia,
    method = "treebag",
    trControl = train_control)
summary(m_tb)

print(m_tb)

# Neuralnet.
# nnet
m_nnn <- train(
    Diabetes ~ .,
    data = dia,
    method = "nnet",
    trControl = train_control)

m_nnn$finalModel
m_nnn$bestTune
plot(varImp(m_nnn, scale = FALSE))

# mlp
m_nn <- train(
    Diabetes ~ .,
    data = dia,
    method = "mlp",
    trControl = train_control)
# library(RSNNS)
m_nn$finalModel
m_nn$bestTune
plot(varImp(m_nn, scale = FALSE))

library(kernlab)

m_nn <- train(
    Diabetes ~ .,
    data = dia,
    method = "svmRadialSigma",
    trControl = train_control)

m_nn$finalModel

plot(varImp(m_nn, scale = FALSE))

bagctrl <- bagControl(fit = ctreeBag$fit,
                        predict = ctreeBag$pred,
                        aggregate = ctreeBag$aggregate)
library(kernlab)
svmbag <- train(
   Diabetes ~ .,
   data = dia,
   "bag",
   bagControl = bagctrl,
   trControl = train_control)
plot(varImp(svmbag, scale = FALSE))