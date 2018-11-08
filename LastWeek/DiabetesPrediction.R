#Libs.
library(caret)
library(C50)
library(plyr)
library(party)
library(doParallel)
library(ipred)
library(e1071)
library(kernlab)
library(dplyr)

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
    data = dia,
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
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
m_nnn <- train(
    Diabetes ~ .,
    data = dia,
    method = "nnet",
    trControl = train_control
    )

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
svmbag <- train(
   Diabetes ~ .,
   data = dia,
   "bag",
   bagControl = bagctrl,
   trControl = train_control)
plot(varImp(svmbag, scale = FALSE))
}

# Cost adjustment.
# Results:
#   Accuracy        Kappa 
# 0.9322916667 0.8574040219

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

# Data edit.
library(dbplyr)
mean(dia$Blodtryck)
filter(dia, Blodtryck > 0)
dia <- as.table(dia)
str(dia)
dia_fixedMean <- dia
bm <- dia %>% filter(Blodtryck > 0) %>% select(Blodtryck) %>% summarise(mean = mean(Blodtryck))
dia_fixedMean <- transform(dia_fixedMean, Blodtryck = ifelse(Blodtryck == 0, round(bm$mean), Blodtryck))

GlukostoleransM <- dia %>% filter(Glukostolerans > 0) %>% select(Glukostolerans) %>% summarise(mean = mean(Glukostolerans))
dia_fixedMean <- transform(dia_fixedMean, Glukostolerans = ifelse(Glukostolerans == 0, round(GlukostoleransM$mean), Glukostolerans))

HudtjocklekM <- dia %>% filter(Hudtjocklek > 0) %>% select(Hudtjocklek) %>% summarise(mean = mean(Hudtjocklek))
dia_fixedMean <- transform(dia_fixedMean, Hudtjocklek = ifelse(Hudtjocklek == 0, round(HudtjocklekM$mean), Hudtjocklek))

BMIM <- dia %>% filter(BMI > 0) %>% select(BMI) %>% summarise(mean = mean(BMI))
dia_fixedMean <- transform(dia_fixedMean, BMI = ifelse(BMI == 0, round(BMIM$mean), BMI))

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

grid <- expand.grid(.model = c("tree", "rules"),
                      .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                      .winnow = "FALSE")

c5_m <- train(
    Diabetes ~ .,
    data = dia_fixedMean,
    method = "C5.0",
    trControl = train_control,
    tuneGrid = grid
    )

knn <- train(
    Diabetes ~ .,
    data = dia_fixedMean,
    method = "knn",
    trControl = train_control
    )

knn <- train(
    Diabetes ~ .,
    data = dia,
    method = "knn",
    trControl = train_control
    )