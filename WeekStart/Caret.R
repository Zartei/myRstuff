library(caret)
library(XLConnect)
wb <- loadWorkbook("Concrete_Data.xls")
concrete <- readWorksheet(wb, sheet = "Sheet1")
rm(wb)
names <- c('Cement', 'FurnaceSlag', 'FlyAsh', 'Water', 'Superplasticizer', 'CoarseAggregate', 'FireAggregate', 'Age', 'ConcreteCompressive')
colnames(concrete) <- names

inTrain <- createDataPartition(
  y = concrete$Age,
## the outcome data are needed
  p = .75,
## The percentage of data in the
## training set
  list = FALSE
)

training <- concrete[inTrain,]
testing <- concrete[-inTrain,]

ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <- train(
  Age ~ .,
  data = training,
  method = "parRF",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl
)

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)
summary(plsFit)
MAE(testing$Age, plsClasses)
MAE(mean(concrete$Age), plsClasses)

inTrain <- createDataPartition(
  y = concrete$Age,
## the outcome data are needed
  p = .75,
## The percentage of data in the
## training set
  list = FALSE
)

training <- concrete[inTrain,]
testing <- concrete[-inTrain,]

ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <- train(
  Age ~ .,
  data = training,
  method = "parRF",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl
)

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)
summary(plsFit)
MAE(testing$Age, plsClasses)
MAE(mean(concrete$Age), plsClasses)