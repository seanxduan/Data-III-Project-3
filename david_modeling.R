# STAT 8330 project #3
# David Reynolds
# Modeling

library(caret)
library(data.table)
library(dplyr)

# Read in the data
train <- read.csv("NFL_Train.csv")
test <- read.csv("NFL_Test.csv")

# Create variables for the differences
train$weight_diff <- train$AvgWeightDef - train$AvgWeightOff
train$height_diff <- train$AvgHeightDef - train$AvgHeightOff
train$age_diff <- train$AvgAgeDef - train$AvgAgeOff

test$weight_diff <- test$AvgWeightDef - test$AvgWeightOff
test$height_diff <- test$AvgHeightDef - test$AvgHeightOff
test$age_diff <- test$AvgAgeDef - test$AvgAgeOff

# Filter for plays with no sacks
library(dplyr)
train <- train %>% 
  filter(passResult != "S")

test <- test %>% 
  filter(passResult != "S")

# Change passResult to binary
train$pass_result <- ifelse(train$passResult == "C", "1", "0")
test$pass_result <- ifelse(test$passResult == "C", "1", "0")

# Change levels of side
train$side <- ifelse(train$side == "own", "1", "0")
test$side <- ifelse(test$side == "own", "1", "0")

# Remove, rename, and reorder predictors
train <- train %>% 
  select(quarter, down, "yards_to_go" = yardsToGo, "offensive_formation" = offenseFormation, 
         "defenders_in_box" = defendersInTheBox, "pass_rushers" = numberOfPassRushers, 
         "dropback_type" = typeDropback, "absolute_yardline" = absoluteYardlineNumber, score_diff, 
         side, time_remaining, close_game, penalty, dbs, weight_diff, height_diff, age_diff, 
         pass_result)

test <- test %>% 
  select(quarter, down, "yards_to_go" = yardsToGo, "offensive_formation" = offenseFormation, 
         "defenders_in_box" = defendersInTheBox, "pass_rushers" = numberOfPassRushers, 
         "dropback_type" = typeDropback, "absolute_yardline" = absoluteYardlineNumber, score_diff, 
         side, time_remaining, close_game, penalty, dbs, weight_diff, height_diff, age_diff, 
         pass_result)

# Make sure the variables are of the right type
factors <- c("quarter", "down", "offensive_formation", "dropback_type", "side", "close_game", 
             "penalty", "pass_result")
train[factors] <- lapply(train[factors], factor)
test[factors] <- lapply(test[factors], factor)

numerics <- c("yards_to_go", "defenders_in_box", "pass_rushers", "absolute_yardline", "score_diff", 
              "time_remaining", "dbs", "weight_diff", "height_diff", "age_diff")
train[numerics] <- sapply(train[numerics], as.numeric)
test[numerics] <- sapply(test[numerics], as.numeric)

# Rename level for offensive_formation
levels(train$offensive_formation)[levels(train$offensive_formation) == ""] <- "UNKNOWN"
levels(test$offensive_formation)[levels(test$offensive_formation) == ""] <- "UNKNOWN"

## Logistic regression

# Train the model
library(caret)
nfl_glm <- train(pass_result ~., data = train, 
                 trControl = trainControl(method = "cv", number = 5), method = "glm", 
                 family = "binomial")

# Predict on the test set
nfl_glm_pred <- predict(nfl_glm, test)

# Report the confusion matrix and accuracy (0.6591)
confusionMatrix(nfl_glm_pred, test$pass_result)

## LDA

# Train the model
nfl_lda <- train(pass_result ~., data = train, trControl = trainControl(method = "cv", number = 5),
                 method = "lda")

# Predict on the test set
nfl_lda_pred <- predict(nfl_lda, test)

# Report the confusion matrix and accuracy (0.6582)
confusionMatrix(nfl_lda_pred, test$pass_result)

## KNN

# Train the classifier and calculate the training error for a range of K
train_control <- trainControl(method = "cv", number = 10)

knn_accuracy = rep(NA, 15)
for (i in 1:15) {
  knn = train(pass_result ~., data = train, method = "knn", trControl = train_control, 
                     preProcess = c("center", "scale"), tuneGrid = data.frame(k = i))
  knn_accuracy[i] = as.numeric(knn$results[2])
}

# Refit the model using K = 15
nfl_knn <- train(pass_result ~., data = train, method = "knn", trControl = train_control, 
                 preProcess = c("center", "scale"), tuneGrid = data.frame(k = 15))

# Predict on the test set
nfl_knn_pred <- predict(nfl_knn, test)

# Report the confusion matrix and accuracy (0.6417)
confusionMatrix(nfl_knn_pred, test$pass_result)

### Trees

## Classification tree

# Train the model
nfl_tree <- train(pass_result ~., data = train, method = "rpart", trControl = train_control, tuneLength = 10)

# Plot model accuracy vs. different values of the complexity parameter
plot(nfl_tree)

# Predict on the test set
nfl_tree_pred <- predict(nfl_tree, test)

# Report the confusion matrix and accuracy (0.6521)
confusionMatrix(nfl_tree_pred, test$pass_result)

## Bagging

# Train the model
nfl_bag <- train(pass_result ~., data = train, method = "treebag", trControl = train_control)

# Predict on the test set
nfl_bag_pred <- predict(nfl_bag, test)

# Report the confusion matrix and accuracy (0.6243)
confusionMatrix(nfl_bag_pred, test$pass_result)

## Boosting

## Random forest

### Support vector machines

### Neural networks


