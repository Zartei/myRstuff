# My support functions.
source("myScripts.R")

# Libs used.
library(dplyr)
library(e1071)

# Functions
# Function to filter Genres to just have one value.
myfiter <- function(x) {
    gen <- unlist(strsplit(paste(x), "[;]"));
    return(gsub(" ", "", gen[1], fixed = TRUE))
}

# Function to filter app names.
filterName <- function(x) {
    str <- iconv(paste(x), from = 'UTF-8', to = 'ASCII//TRANSLIT')
    gen <- unlist(strsplit(str, "[ ]"));
    return(paste(head(gen, 1), tail(gen, 1), sep = " "))
}

# Variables
input <- "googleplaystore.csv"

# Start.
gplay <- read.csv(input)
nb_googl <- select(gplay, Rating, Reviews, Genres)
str(nb_googl)
#Ta bort felaktig rad
nb_googl <- nb_googl[-10473,]
#Ta bort Na's
nb_googl <- na.omit(nb_googl)
#Ta bort allt efter ; i Genres
nb_googl$Genres <- gsub("\\;.*", "", nb_googl$Genres)
#Dela in Genres i 4 typgrupper. (efter tester med ca 120 Genres som gav ca 17-20%)
school <- c("Art & Design", "Books & Reference", "Education", "Libraries & Demo", "Word", "News & Magazines", "Maps & Navigation", "Educational")
health <- c("Beauty", "Food & Drink", "Health & Fitness", "Lifestyle", "Medical")
games <- c("Adventure", "Arcade", "Card", "Action", "Strategy", "Puzzle", "Racing", "Simulation", "Board", "Trivia", "Role Playing", "Casino")
nb_googl$GenreType <- ifelse(nb_googl$Genres %in% school, "school", ifelse(nb_googl$Genres %in% health, "health", ifelse(nb_googl$Genres %in% games, "games", "other")))
nb_googl$GenreType <- factor(nb_googl$GenreType)
table(nb_googl$GenreType)
#Gör till factor
nb_googl$Rating <- as.factor(nb_googl$Rating)
nb_googl$Reviews <- as.factor(nb_googl$Reviews)
nb_googl$Genres <- factor(nb_googl$Genres)
#Ta bort $ tecken och gör till numeric
#nb_googl$Price <- as.factor(gsub("\\$", "", nb_googl$Price))


#Split i Train/Test set
nb_googl_set <- splitTrainSet(nb_googl)
#Lable tabeller som behövs för Naive Bayes
nb_googl_train_lable <- nb_googl_set$train$GenreType
nb_googl_test_lable <- nb_googl_set$test$GenreType
table(nb_googl_train_lable)
table(nb_googl_test_lable)
#Träna modellen
#install.packages("e1071")
library(e1071)

cols <- c("Rating", "Reviews")
str(nb_googl_set$train)
nb_googl_classifier <- naiveBayes(nb_googl_set$train[cols], nb_googl_set$train$GenreType)
nb_googl_test_pred <- predict(nb_googl_classifier, nb_googl_set$test[cols])
summary(nb_googl_test_pred)
#CrossTable
library(gmodels)
CrossTable(nb_googl_test_pred, nb_googl_test_lable, prop.chisq = FALSE, prop.t = FALSE,
          prop.r = FALSE, dnn = c('predicted', 'actual'))
table(nb_googl_test_pred)
table(nb_googl_test_lable)

imax <- length(nb_googl_test_pred)
c = 0;
for (i in 1:imax) {
    if (nb_googl_test_pred[i] == nb_googl_set$test$GenreType[i]) {
        c <- c + 1
    }
}
correct <- round((c / imax) * 100, 1)
correct

Kappa(table(nb_googl_set$test$GenreType, nb_googl_test_pred))