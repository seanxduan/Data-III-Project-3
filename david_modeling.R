# STAT 8330 project #3
# David Reynolds
# Modeling

library(caret)
library(data.table)
library(dplyr)

# Read in the data
setwd("~/Downloads/Documents/GitHub/Data-III-Project-3/Data")
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
         "dropback_type" = typeDropback, "absolute_yardline" = absoluteYardlineNumber, 
         "play_result" = offensePlayResult,score_diff, side, time_remaining, close_game, penalty, 
         dbs, epa_bi, weight_diff, height_diff,age_diff, pass_result)

test <- test %>% 
  select(quarter, down, "yards_to_go" = yardsToGo, "offensive_formation" = offenseFormation, 
         "defenders_in_box" = defendersInTheBox, "pass_rushers" = numberOfPassRushers, 
         "dropback_type" = typeDropback, "absolute_yardline" = absoluteYardlineNumber, 
         "play_result" = offensePlayResult, score_diff, side, time_remaining, close_game, penalty, 
         dbs, epa_bi, weight_diff, height_diff, age_diff, pass_result)

# Make sure the variables are of the right type
factors <- c("quarter", "down", "offensive_formation", "dropback_type", "side", "close_game", 
             "penalty", "epa_bi", "pass_result")
train[factors] <- lapply(train[factors], factor)
test[factors] <- lapply(test[factors], factor)

numerics <- c("yards_to_go", "defenders_in_box", "pass_rushers", "absolute_yardline", "play_result", 
              "score_diff", "time_remaining", "dbs", "weight_diff", "height_diff", "age_diff")
train[numerics] <- sapply(train[numerics], as.numeric)
test[numerics] <- sapply(test[numerics], as.numeric)

sapply(train, class)
sapply(test, class)

## Logistic regression

# Train the model
library(caret)
nfl_glm <- train(pass_result ~., data = train, 
                  trControl = trainControl(method = "cv", number = 5),
                  method = "glm", family = "binomial")

# Predict on the test set
nfl_glm_pred <- predict(nfl_glm, test)

# Report the confusion matrix and accuracy
table(nfl_glm_pred, test$pass_result)
