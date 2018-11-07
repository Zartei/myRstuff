#Libs.
library(neuralnet)
library(XLConnect)
library(caret)
library(C50)
library(plyr)
library(party)
library(doParallel)
library(ipred)
library(e1071)

# Functions
source("../WeekStart/myScripts.R")

# Variables
input <- "Diabetes.csv"

# Tuning seting to use 4 cores.
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
stopCluster(cl)
# Start.
dia <- read.csv(input, sep = ";", encoding="UTF-8")
colnames(dia)[1] <- "Graviditeter"
dia$Diabetes <- as.factor(dia$Diabetes)


trainVector <- createDataPartition(dia$Diabetes, p = 0.75, list = F)
dia.train <- dia[trainVector,]
dia.test <- dia[-trainVector,]

# Start to predict with C5.0
{
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

grid <- expand.grid(.model = c("tree", "rules"),
                      .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                      .winnow = "FALSE")

c5_m <- train(
    Diabetes ~ .,
    data = dia.train,
    method = "C5.0",
    trControl = train_control,
    tuneGrid = grid
    )

c5_m
summary(c5_m$finalModel)
plot(varImp(c5_m, scale = F))

c5_pred <- predict(c5_m,dia.test[-9])
postResample(pred = c5_pred,obs = dia.test$Diabetes)
}

# Treebaging - Model Test
{ 
ctrl <- trainControl(method = "cv", number = 10)
m_tb <- train(
    Diabetes ~ .,
    data = dia,
    method = "treebag",
    trControl = train_control)
summary(m_tb)

print(m_tb)
}

# Neuralnet. - Model Test
{
# nnet
m_nnn <- train(
    Diabetes ~ .,
    data = dia,
    method = "nnet",
    trControl = train_control)

m_nnn$finalModel
m_nnn$bestTune
plot(varImp(m_nnn, scale = FALSE))
}

# mlp - Model Test
{
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
}

# svmRadialSigma - Model Test
{
m_nn <- train(
    Diabetes ~ .,
    data = dia,
    method = "svmRadialSigma",
    trControl = train_control)

m_nn$finalModel
plot(varImp(m_nn, scale = FALSE))
}

# bag - Model Test
{
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
}

# Cost adjustment.
{
data.set <- dia[dia$Diabetes == 0,]
pos <- dia[dia$Diabetes == 1,]
for (i in 1:5) {
    data.set <- rbind(data.set, pos)
}
trainVector <- createDataPartition(data.set$Diabetes, p = 0.75, list = F)
cost.train <- data.set[trainVector,]
cost.train.test <- data.set[-trainVector,]


train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
m <- train(Diabetes ~ .,
           data = cost.train,
           method = "C5.0",
           trControl = train_control)
m
summary(m$finalModel)

c5_cost_pred <- predict(m, dia.test[-9])
postResample(pred = c5_cost_pred, obs = dia.test$Diabetes)
}