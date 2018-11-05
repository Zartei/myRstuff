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
