# STAT 8330 project #3
# David Reynolds
# Modeling

library(caret)
library(data.table)
library(dplyr)

# Read in the data
setwd("~/Downloads/Documents/GitHub/Data-III-Project-3/Data")
plays <- read.csv("david_modeling.csv", stringsAsFactors = TRUE)
plays$X <- NULL
cols <- c("quarter", "down", "close_game", "penalty", "epa_bi")
plays[cols] <- lapply(plays[cols], factor)
sapply(plays, class)

# Split the data into training and test sets
set.seed(1)
train_sample <- sample(1:nrow(plays), size = floor(nrow(plays) *.8))
train <- plays[train_sample,]
test <- plays[-train_sample,]

## "Multinomial logistic regression" (actually a neural network)

# Implement 5-fold repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

# Fit the model
set.seed(1)
multinom <- caret::train(pass_result ~., data = train, method = "multinom", trControl = train_control, trace = FALSE)
