library(class)

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

wbcdSet <- splitTrainSet(wbcd)


round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

normalize <- function(x) {(x - min(x)) / (max(x) - min(x)) }


wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_n <- cbind.data.frame(wbcd$diagnosis, wbcd_n)

head(wbcd_n)


head(wbcdSet$test)

wbcd_train <- as.data.frame(lapply(wbcdSet$train[2:31], normalize))
wbcd_test <- as.data.frame(lapply(wbcdSet$test[2:31], normalize))

wbcd_train_labels <- wbcdSet$train[,1]
wbcd_test_labels <- wbcdSet$test[, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)


CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)