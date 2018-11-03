source("myScripts.R")

abalone <- read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'), header = FALSE)
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", "WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight", "Rings");

c_n <- as.data.frame(lapply(abalone[2:9], normalize))
colnames(c_n)[8]    <- "RingsNorm"
c_n$sexFact <- ifelse(abalone$Sex == 'M', 1, ifelse(abalone$Sex == 'F', 2, 0))
qs <- summary(abalone$Rings)
c_n$ringSize <- ifelse(abalone$Rings < qs[2], 'Small', ifelse(abalone$Rings > qs[5], 'Large', 'Normal'))

ringsNorm <- as.data.frame(lapply(abalone$Rings, normalize))
c <- cbind('Sex' = abalone$Sex, c_n,  'Rings' = abalone$Rings)

mlset <- splitTrainSet(c,split = 0.9)
head(mlset$test[columns])
head(mlset$train)
myK <- round(sqrt(nrow(c)))
library(class)

# Predict Rings
columns <- c("Length", "Diameter", "Height", "WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight")
P <- knn(mlset$train[columns], mlset$test[columns], cl = mlset$train$Rings, k = myK)

output <- as.data.frame(cbind("Test" = mlset$test$Rings, "Prediction" = P))
output$diff <- abs(output$Test - output$Prediction)
output$error <- ifelse(output$diff < 2, 0, 1)

modleError <- sum(output$error) / nrow(output)

sprintf("Miss labeled abalone %s%%. With a K value of %s", round(modleError * 100), myK)

help(histogram)
boxplot(output$diff, horizontal = T, col = "lightgray")
histogram(output$diff, col = rainbow(5))

CrossTable(x = mlset$test$Rings, y = P, prop.c = F, prop.chisq = F, prop.r = F, prop.t = F, dnn = c(' Test ', ' Prediction '), format = 'SAS')

# Predict Sex.
columns <- c("Length", "Diameter", "Height", "WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight", "RingsNorm")
predictSex <- knn(mlset$train[columns], mlset$test[columns], cl = mlset$train$Sex, k = myK)

output <- as.data.frame(cbind("Test" = mlset$test$Sex, "Prediction" = predictSex))
output$diff <- abs(output$Test - output$Prediction)
output$error <- ifelse(output$diff < 2, 0, 1)

modleError <- sum(output$error) / nrow(output)
round(modleError * 100)

CrossTable(x = mlset$test$Sex, y = predictSex, prop.c = F, prop.chisq = F, prop.r = F, prop.t = F, dnn = c(' Test ', ' Prediction '))

# Predict Size
columns <- c("Length", "Diameter", "Height", "WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight","sexFact")
myPrediction <- knn(mlset$train[columns], mlset$test[columns], cl = mlset$train$ringSize, k = myK)

output <- as.data.frame(cbind("Test" = mlset$test$ringSize, "Prediction" = predictSex))
str(output$Test)

modleError <- sum(output$error) / nrow(output)
round(modleError * 100)

ct <- CrossTable(x = mlset$test$ringSize, y = myPrediction, prop.c = F, prop.chisq = F, prop.r = F, prop.t = F, dnn = c(' Test ', ' Prediction '))
ct$t
modleError <- (nrow(output) - sum(diag(ct$t))) / nrow(output)

sprintf("Miss labeled abalone %s%%. With a K value of %s", round(modleError * 100), myK)


head(output)
nrow(output)



pred <- prediction(predictions = output$Prediction, labels = output$Test);


library(gmodels)

help(CrossTable)
ifelse(mlset$test[1:5, 1] == 'M', 1, ifelse(mlset$test[1:5, 1] == 'F',2,0))
mlset$test[1:5, 1]
help(ifelse)

PCV <- knn.cv(mlset$train[, 2:8], cl = mlset$train[, 9], k = myK)

n = nrow(c)
set.seed(101)
trainIndex = sample(1:n, size = round(0.7 * n), replace = FALSE)
itrain = c[trainIndex, 2:8]
itest = c[-trainIndex, 2:8]
train_labels <- c[trainIndex, 9]
test_labels <- c[-trainIndex, 9]
myK <- round(sqrt(4000))

P <- knn(itrain, itest, cl = train_labels, k = myK, prob = TRUE)
