source("./scripts/task_4.R")

data <- read.csv("./data/breast_cancer.csv")

# Clean data
data <- data[, colSums(is.na(data)) != nrow(data)]  
data$id <- NULL
data$diagnosis <- as.factor(data$diagnosis)

# Helper: split 70/30
split_data <- function(data, seed) {
  set.seed(seed)
  n <- nrow(data)
  idx <- sample(seq_len(n), size = 0.7 * n)
  list(
    train = data[idx, ],
    test  = data[-idx, ]
  )
}

# ================================
# a) Single Tree with 3 seeds
# ================================
seeds <- c(1, 2, 3)

results_a <- do.call(rbind,
                     lapply(seeds, function(s) {
                       sp <- split_data(data, s)
                       res <- train_single_tree(sp$train, sp$test)
                       
                       data.frame(
                         seed = s,
                         accuracy = res$accuracy,
                         auc = res$auc
                       )
                     })
)

print(results_a)


# ================================
# b) Bagging (B = 200)
# ================================
sp <- split_data(data, 123)
res_bag <- train_bagging(sp$train, sp$test, B = 200)

cat("Bagging (B=200)\n",
    "Accuracy:", res_bag$accuracy, "\n",
    "AUC:", res_bag$auc, "\n\n")


# ================================
# c) Variance comparison (50 runs)
# ================================
n_repeat <- 50

acc_tree <- numeric(n_repeat)
acc_bag  <- numeric(n_repeat)

for (i in 1:n_repeat) {
  sp <- split_data(data, i)
  
  acc_tree[i] <- train_single_tree(sp$train, sp$test)$accuracy
  acc_bag[i]  <- train_bagging(sp$train, sp$test, B = 200)$accuracy
}

mean_tree <- mean(acc_tree)
var_tree  <- var(acc_tree)

mean_bag <- mean(acc_bag)
var_bag  <- var(acc_bag)

var_reduction <- (var_tree - var_bag) / var_tree * 100

cat("Single Tree:\n",
    "Mean Accuracy:", mean_tree, "\n",
    "Variance:", var_tree, "\n\n")

cat("Bagging:\n",
    "Mean Accuracy:", mean_bag, "\n",
    "Variance:", var_bag, "\n\n")

cat("Variance Reduction (%):", var_reduction, "%\n\n")


# ================================
# d) Bagging vs Random Forest
# ================================
sp <- split_data(data, 999)

res_bag <- train_bagging(sp$train, sp$test, B = 200)
res_rf  <- train_rf(sp$train, sp$test, ntree = 200)

results_d <- data.frame(
  model = c("Bagging", "Random Forest"),
  accuracy = c(res_bag$accuracy, res_rf$accuracy),
  auc = c(res_bag$auc, res_rf$auc)
)

print(results_d)