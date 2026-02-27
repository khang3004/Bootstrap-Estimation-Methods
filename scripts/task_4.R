library(rpart)          
library(randomForest)  
library(pROC)           


#a
#' Train a single decision tree and return Accuracy & AUC
#'
#' @param train Data frame containing training data with target variable "diagnosis"
#' @param test Data frame containing test data with target variable "diagnosis"
#' @return A list with:
#'   - accuracy: Classification accuracy on test set
#'   - auc: AUC score on test set
#'   
#' @export
train_single_tree <- function(train, test) {
  
  features <- colnames(train)[colnames(train) != "diagnosis"]
  form <- as.formula(
    paste("diagnosis ~", paste(features, collapse = " + "))
  )
  
  model <- rpart(form, data = train, method = "class")
  
  prob <- predict(model, test, type = "prob")[, "M"]
  pred <- factor(ifelse(prob > 0.5, "M", "B"),
                 levels = c("B", "M"))
  
  list(
    accuracy = mean(pred == test$diagnosis),
    auc = as.numeric(auc(test$diagnosis, prob))
  )
}

#b
#' Train Bagging (Bootstrap Aggregation) with decision trees
#'
#' @param data Data frame containing features and target "diagnosis"
#' @param seed Random seed
#' @param B Number of bootstrap models (default = 200)
#' @return List with accuracy and auc on test set
#' 
#' @export
train_bagging <- function(train, test, B = 200) {
  
  features <- colnames(train)[colnames(train) != "diagnosis"]
  form <- as.formula(
    paste("diagnosis ~", paste(features, collapse = " + "))
  )
  
  prob_sum <- rep(0, nrow(test))
  
  for (b in 1:B) {
    boot_idx <- sample(seq_len(nrow(train)), replace = TRUE)
    boot_data <- train[boot_idx, ]
    
    model <- rpart(form, data = boot_data, method = "class")
    prob <- predict(model, test, type = "prob")[, "M"]
    
    prob_sum <- prob_sum + prob
  }
  
  prob_avg <- prob_sum / B
  
  pred <- factor(ifelse(prob_avg > 0.5, "M", "B"),
                 levels = c("B", "M"))
  
  list(
    accuracy = mean(pred == test$diagnosis),
    auc = as.numeric(auc(test$diagnosis, prob_avg))
  )
}

#d)
#' Train Random Forest classifier and evaluate performance
#'
#' @param train Data frame containing training data with target variable "diagnosis"
#' @param test Data frame containing test data with target variable "diagnosis"
#' @param ntree Number of trees in the forest (default = 200)
#'
#' @return A list containing:
#'   - accuracy: Classification accuracy on test set
#'   - auc: Area Under the ROC Curve on test set
#'   
#' @export
train_rf <- function(train, test, ntree = 200) {
  
  features <- colnames(train)[colnames(train) != "diagnosis"]
  form <- as.formula(
    paste("diagnosis ~", paste(features, collapse = " + "))
  )
  
  model <- randomForest(
    formula = form,
    data = train,
    ntree = ntree
  )
  
  prob <- predict(model, test, type = "prob")[, "M"]
  pred <- factor(ifelse(prob > 0.5, "M", "B"),
                 levels = c("B", "M"))
  
  list(
    accuracy = mean(pred == test$diagnosis),
    auc = as.numeric(pROC::auc(test$diagnosis, prob))
  )
}

