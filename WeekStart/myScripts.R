## File with small R functions created by Johan Sebenius.

# Small function to split data set into Training and Test set.
# In parameters are a dataset
# Seed setings, 
#   Setseed TRUE/FALSE Pick if you wanna set a seed or go for random seed.
#   seedValue is the seed to use.
# split is the % size of the pick for the training set, default 70%.
splitTrainSet <- function(Dataset = 'missing', Setseed = FALSE, seedValue = 101, split = 0.7) {
    if (!is.data.frame(Dataset)) {
        print("Dataset missing fix it, defaulting to iris dataset.")
        Dataset = iris;
    }
    if (Setseed) {
        set.seed(seedValue)
    }
    n = nrow(Dataset)
    trainIndex = sample(1:n, size = round(split * n), replace = FALSE)
    trainSet = Dataset[trainIndex,]
    testSet = Dataset[-trainIndex,]
    return(list("train" = trainSet, "test" = testSet));
}

# Mean Average Error.
MAE <- function(actual, predicted) {
    mean(abs(actual - predicted))
}


# Normalize function.
normalize <- function(x) {(x - min(x)) / (max(x) - min(x)) }