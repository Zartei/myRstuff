# My support functions.
source("myScripts.R")

# Libs used.
library(dplyr)
library(e1071)
library(caret)
library(klaR)

# Functions
# Function to filter Genres to just have one value.

# Variables
input <- "googleplaystore.csv"

# Start.
gplay <- read.csv(input)

gplay <- gplay[c('Category','Rating','Reviews')]
gplay <- gplay[-10473,]

gplay$Rating <- as.factor(gplay$Rating)
gplay$Reviews <- as.factor(gplay$Reviews)

levels(gplay$Rating)[levels(gplay$Rating) == 'NaN'] <- "Missing"
gplay <- droplevels(gplay)

gplay_set <- splitTrainSet(gplay, split = 0.9)

x = gplay_set$train[, -1]
y = gplay_set$train$Category

model = train(x, y, 'nb', trControl = trainControl(method = 'cv', number = 10))
# model
pre <- predict(model$finalModel, gplay_set$test[,-1])

checkPrediction(pre$class, gplay_set$test$Category)