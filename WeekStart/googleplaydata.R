# My support functions.
source("myScripts.R")

# Libs used.
library(C50)

library(dplyr)

# Functions
# Function to filter Genres to just have one value.
myfiter <- function(x) {
    gen <- unlist(strsplit(paste(x), "[;]"));
    return(gsub(" ", "", gen[1], fixed = TRUE))
}


# Start.
gplay <- read.csv("googleplaystore.csv")

# str(gplay)

# Droping some columns,
# App - App name
# Category - Since it is what we wanna look for more or less.
# Last.Update
# Current.Ver
# Andriod.Ver
gplay <- gplay[-c(1, 2, 11, 12, 13)]

# sum(apply(gplay, 2, is.na)) # 1474
# nrow(gplay) # 10841
# nrow(na.omit(gplay)) # Check to see if it is okey todrop rows with na ~10% of data set so no good.
# table(gplay$Rating, useNA = "ifany")
# Find that it is the raiting that has alot of missing values.
gplay$Rating <- as.factor(gplay$Rating)
levels(gplay$Rating)[levels(gplay$Rating) == 'NaN'] <- "Missing"
# sum(is.na(gplay$Rating))
# sum(apply(gplay, 2, is.na)) # 0

# str(gplay)
# Change from 120 Factor lvls to just 48 factor lvls
gplay$Genres <- as.factor(sapply(gplay$Genres, myfiter))
# One broken record.
gplay <- dplyr:::filter(gplay, Rating != '19')

# Change lvls from "" or Nans
levels(gplay$Content.Rating)[1] = "missing";
levels(gplay$Type)[3] = "missing";

# Split the dataset.
gplay_set <- splitTrainSet(gplay, split = 0.9)

help(C5.0)

c5_model <- C5.0(gplay_set$train[c(1,2,3,4,5,6,7)], gplay_set$train$Genres)

c5_prediction <- predict(c5_model, gplay_set$test[c(1, 2, 3, 4, 5, 6, 7)])

imax <- length(c5_prediction)
c = 0;
for (i in 1:imax) {
    if (c5_prediction[i] == gplay_set$test$Genres[i]) {
        c <- c+1
    }
}
correct <- round((c / imax) * 100,1)
correct

table(gplay$Rating)
table(gplay$Size)
table(gplay$Installs)
table(gplay$Type)
table(gplay$Price)

rm(list = ls())

#JRip
library(RWeka)
JRip_model <- JRip(Genres ~. ,data = gplay_set$train[,-9])

JRip_prediction <- predict(c5_model, gplay_set$test[c(1, 2, 3, 4, 5, 6, 7)])
imax <- length(JRip_prediction)
c = 0;
for (i in 1:imax) {
    if (JRip_prediction[i] == gplay_set$test$Genres[i]) {
        c <- c + 1
    }
}
correct <- round((c / imax) * 100, 1)
correct