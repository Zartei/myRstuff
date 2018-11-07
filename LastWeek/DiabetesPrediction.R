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


train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

m <- train(Diabetes ~ .,
           data = dia,
           method = "C5.0",
           trControl = train_control)
m
m$finalModel
plot(varImp(m,scale = F))

score <- p== dia$Diabetes
prop.table(table(score))

ctrl <- trainControl(method = "cv", number = 10)
m_tb <- train(
    Diabetes ~ .,
    data = dia,
    method = "treebag",
    trControl = train_control)
summary(m_tb)

print(m_tb)

# Neuralnet.
m_nn <- train(
    Diabetes ~ .,
    data = dia,
    method = "nnet",
    trControl = train_control)

m_nn$finalModel
m_nn$bestTune
results1 <- predict(m_nn, newdata = dia)

importance <- varImp(m_nn, scale = FALSE)

plot(varImp(m_nn, scale = FALSE))