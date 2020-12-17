# STAT 8330 project #3
# David Reynolds
# Modeling

library(caret)
library(DT)
library(dplyr)
library(e1071)
library(gbm)
library(ggplot2)
library(glmnet)
library(mltools)
library(nnet)

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

### Logistic regression

# Train the model
nfl_glm <- train(pass_result ~., data = train, 
                 trControl = trainControl(method = "cv", number = 5), method = "glm", 
                 family = "binomial")

# Predict on the test set
nfl_glm_pred <- predict(nfl_glm, test)

# Report the confusion matrix and accuracy (0.6590515)
confusionMatrix(nfl_glm_pred, test$pass_result)
model1_acc <- mean(nfl_glm_pred == test$pass_result)

### Penalized logistic regression
train_mat <- model.matrix(pass_result ~., data = train)
test_mat <- model.matrix(pass_result ~., data = test)

## L1

# Train the model
set.seed(1)
nfl_cv1 <- cv.glmnet(train_mat, train$pass_result, alpha = 1, family = "binomial")
nfl_l1 <- glmnet(train_mat, train$pass_result, alpha = 1, family = "binomial", 
                 lambda = nfl_cv1$lambda.min)

# Predict on the test set
nfl_l1_probs <- predict(nfl_l1, test_mat)
nfl_l1_pred <- ifelse(nfl_l1_probs > 0.5, "1", "0")

# Calculate the accuracy (0.6240602)
model2_acc <- mean(nfl_l1_pred == test$pass_result)

## L2

# Train the model
set.seed(1)
nfl_cv2 <- cv.glmnet(train_mat, train$pass_result, alpha = 0, family = "binomial")
nfl_l2 <- glmnet(train_mat, train$pass_result, alpha = 0, family = "binomial", 
                 lambda = nfl_cv2$lambda.min)

# Predict on the test set
nfl_l2_probs <- predict(nfl_l2, test_mat)
nfl_l2_pred <- ifelse(nfl_l2_probs > 0.5, "1", "0")

# Calculate the accuracy (0.6249277)
model3_acc <- mean(nfl_l2_pred == test$pass_result)

### LDA

# Train the model
nfl_lda <- train(pass_result ~., data = train, trControl = trainControl(method = "cv", number = 10),
                 method = "lda")

# Predict on the test set
nfl_lda_pred <- predict(nfl_lda, test)

# Report the confusion matrix and accuracy (0.6581839)
confusionMatrix(nfl_lda_pred, test$pass_result)
model4_acc <- mean(nfl_lda_pred == test$pass_result)

### KNN

# Train the classifier and calculate the training accuracy for a range of K
train_control <- trainControl(method = "cv", number = 10)

set.seed(1)
knn_accuracy = rep(NA, 15)
for (i in 1:15) {
  knn = train(pass_result ~., data = train, method = "knn", trControl = train_control, 
              preProcess = c("center", "scale"), tuneGrid = data.frame(k = i))
  knn_accuracy[i] = as.numeric(knn$results[2])
}
knn_accuracy

# Refit the model using K = 15
set.seed(1)
nfl_knn <- train(pass_result ~., data = train, method = "knn", trControl = train_control, 
                 preProcess = c("center", "scale"), tuneGrid = data.frame(k = 15))

# Predict on the test set
nfl_knn_pred <- predict(nfl_knn, test)

# Report the confusion matrix and accuracy (0.6414112)
confusionMatrix(nfl_knn_pred, test$pass_result)
model5_acc <- mean(nfl_knn_pred == test$pass_result)

### Trees

## Classification tree

# Train the model
set.seed(1)
nfl_tree <- train(pass_result ~., data = train, method = "rpart", trControl = train_control, tuneLength = 10)

# Predict on the test set
nfl_tree_pred <- predict(nfl_tree, test)

# Report the confusion matrix and accuracy (0.652111)
confusionMatrix(nfl_tree_pred, test$pass_result)
model6_acc <- mean(nfl_tree_pred == test$pass_result)

# Produce a variable importance plot
varImp(nfl_tree)

## Bagging

# Train the model
set.seed(1)
nfl_bag <- train(pass_result ~., data = train, method = "treebag", trControl = train_control)

# Predict on the test set
nfl_bag_pred <- predict(nfl_bag, test)

# Report the confusion matrix and accuracy (0.6211683)
confusionMatrix(nfl_bag_pred, test$pass_result)
model7_acc <- mean(nfl_bag_pred == test$pass_result)

# Produce a variable importance plot
varImp(nfl_bag)

## Boosting

# Specify a grid of tuning parameters
boost_grid <- expand.grid(interaction.depth = c(1, 5), n.trees = c(500, 1500), shrinkage = 0.01, 
                          n.minobsinnode = 20)

# Train the model
set.seed(1)
nfl_boost <- train(pass_result ~., data = train, method = "gbm", trControl = train_control, tuneGrid = boost_grid, 
                   verbose = FALSE)

# Predict on the test set
nfl_boost_pred <- predict(nfl_boost, test)

# Report the confusion matrix and accuracy (0.6578947)   
confusionMatrix(nfl_boost_pred, test$pass_result)
model8_acc <- mean(nfl_boost_pred == test$pass_result)

# Produce a variable importance plot
nfl_boost_imp <- as.data.frame(varImp(nfl_boost)$importance)
nfl_boost_imp$Variable <- as.factor(rownames(nfl_boost_imp))
rownames(nfl_boost_imp) <- NULL

ggplot(data = nfl_boost_imp, aes(x = reorder(Variable, Overall), y = Overall)) + geom_bar(stat = "identity", fill = "#48a8d4") + 
  labs(x = "Variable", y = "Importance", title = "Variable importance for boosting") + coord_flip() + theme_minimal() 
                                                                                 
## Random forest

# Specify a grid of tuning parameters
rf_grid <- expand.grid(mtry = c(1, 3, 5), splitrule = "gini", min.node.size = c(1, 5))

# Train the model
set.seed(1)
nfl_rf <- train(pass_result ~., data = train, method = "ranger", trControl = train_control, tuneGrid = rf_grid, 
                importance = "impurity")

# Predict on the test set
nfl_rf_pred <- predict(nfl_rf, test)

# Report the confusion matrix and accuracy (0.6578947)
confusionMatrix(nfl_rf_pred, test$pass_result)
model9_acc <- mean(nfl_rf_pred == test$pass_result)

# Produce a variable importance plot
varImp(nfl_rf)

### Support vector machines

## Linear kernel

# Train the model
set.seed(1)
nfl_svm1 <- tune(svm, pass_result ~., data = train, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1)))

# Predict on the test set
nfl_svm_pred1 <- predict(nfl_svm1$best.model, test)

# Report the confusion matrix and accuracy (0.6573164)
nfl_svm_cm1 <- table(nfl_svm_pred1, test$pass_result)
model10_acc <- sum(diag(nfl_svm_cm1)) / sum(nfl_svm_cm1)

## Polynomial kernel

# Train the model
set.seed(1)
nfl_svm2 <- tune(svm, pass_result ~., data = train, kernel = "polynomial", ranges = list(cost = c(0.01, 1), 
                                                                                         degree = c(2, 3)))
# Predict on the test set
nfl_svm_pred2 <- predict(nfl_svm2$best.model, test)

# Report the confusion matrix and accuracy (0.6558704)
nfl_svm_cm2 <- table(nfl_svm_pred2, test$pass_result)
model11_acc <- sum(diag(nfl_svm_cm2)) / sum(nfl_svm_cm2)

## Radial kernel

# Train the model
set.seed(1)
nfl_svm3 <- tune(svm, pass_result ~., data = train, kernel = "radial", ranges = list(cost = c(0.01, 1), 
                                                                                     gamma = c(0.01, 1)))

# Predict on the test set
nfl_svm_pred3 <- predict(nfl_svm3$best.model, test)

# Report the confusion matrix and accuracy (0.652111)
nfl_svm_cm3 <- table(nfl_svm_pred3, test$pass_result)
model12_acc <- sum(diag(nfl_svm_cm3)) / sum(nfl_svm_cm3)

## Sigmoid kernel

# Train the model
set.seed(1)
nfl_svm4 <- tune(svm, pass_result ~., data = train, kernel = "sigmoid", ranges = list(cost = c(0.01, 1), 
                                                                                      gamma = c(0.01, 1)))

# Predict on the test set
nfl_svm_pred4 <- predict(nfl_svm4$best.model, test)

# Report the confusion matrix and accuracy (0.6463274)
nfl_svm_cm4 <- table(nfl_svm_pred4, test$pass_result)
model13_acc <- sum(diag(nfl_svm_cm4)) / sum(nfl_svm_cm4)

### Neural network

# Convert all categorical variables to dummies through one-hot encoding
train_nn <- one_hot(as.data.table(train), cols = factors)
train_nn$pass_result_0 <- NULL
train_nn$pass_result_1 <- NULL
train_nn <- cbind(train_nn, "pass_result" = train$pass_result)
train_nn <- as.data.frame(train_nn)

test_nn <- one_hot(as.data.table(test), cols = factors)
test_nn$pass_result_0 <- NULL
test_nn$pass_result_1 <- NULL
test_nn <- cbind(test_nn, "pass_result" = test$pass_result)
test_nn <- as.data.frame(test_nn)

# Scale the numeric predictors
train_nn[numerics] <- scale(train_nn[numerics], scale = TRUE)
test_nn[numerics] <- scale(test_nn[numerics], scale = TRUE)

# Train the model
set.seed(1)
nfl_nn <- nnet(pass_result ~., data = train_nn, size = 5)

# Predict on the test set
nfl_nn_pred <- predict(nfl_nn, test_nn, type = "class")

# Report the confusion matrix and accuracy (0.6396761)
nfl_nn_cm <- table(nfl_nn_pred, test_nn$pass_result)
model14_acc <- sum(diag(nfl_nn_cm)) / sum(nfl_nn_cm)

### Summary of models

## Create a table for comparing model performance
Model <- c("Logistic regression", "L1 penalized logistic regression", 
           "L2 penalized logistic regression", "LDA", "KNN", "Classification tree", "Bagging", 
           "Boosting", "Random forest", "SVM (linear kernel)", "SVM (polynomial kernel)", 
           "SVM (radial kernel)", "SVM (sigmoid kernel)", "Neural network")

Accuracy <- round(c(model1_acc, model2_acc, model3_acc, model4_acc, model5_acc, model6_acc, model7_acc, 
              model8_acc, model9_acc, model10_acc, model11_acc, model12_acc, model13_acc, model14_acc), 3)

performance <- cbind(Model, Accuracy)
performance <- as.data.frame(performance)

performance <- performance %>% 
  arrange(desc(Accuracy))

datatable(performance, rownames = FALSE)
