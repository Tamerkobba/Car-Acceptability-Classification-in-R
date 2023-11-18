# TODO: remove this from here, it will be present earlier in the markdown
library(MASS)
library(caret)
library(stats)
library(cvms)
library(pROC)

dataset <- read.csv("DataAssign2.csv")
dataset$V1 <- as.numeric(factor(dataset$V1))
dataset$V2 <- as.numeric(factor(dataset$V2))
dataset$V3 <- as.numeric(factor(dataset$V3))
dataset$V4 <- as.numeric(factor(dataset$V4))
dataset$V5 <- as.numeric(factor(dataset$V5))
dataset$V6 <- as.numeric(factor(dataset$V6))

set.seed(123)
train_indices <- sample(1:nrow(dataset), nrow(dataset) * 0.1)
data_X <- dataset[c("V1", "V2", "V5", "V6")]
data_y <- dataset["V7"]
train_data_X <- data_X[train_indices, ]
train_data_y <- data_y$V7[train_indices]
test_data_X <- data_X[-train_indices, ]
test_data_y <- data_y$V7[-train_indices]

# FITTING THE MODEL
model <- qda(train_data_y ~ ., data=train_data_X)

# SIGNIFICANT FEATURES
mean_diffs <- abs(diff(model$means))
# we see 1 and 3 are ~ 0.1 but everything else is ~ 0.3

# TEST
predictions <- predict(model, test_data_X)

# CONFUSION MATRIX
pred_classes <- predictions$class
actual <- test_data_y
actual <- as.factor(actual)
confusion_matrix <- cvms::confusion_matrix(targets = pred_classes, predictions = actual)
plot_confusion_matrix(confusion_matrix$`Confusion Matrix`[[1]])

# METRICS
conf_mat <- table(pred_classes, actual)
true_negative <- conf_mat[1]
false_positive <- conf_mat[2]
false_negative <- conf_mat[3]
true_positive <- conf_mat[4]

accuracy <- (true_positive + true_negative) / (sum(conf_mat))
precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)
f1_score <- 2 * ((precision * recall) / (precision + recall))

roc_obj <- roc(response=test_data_y, predictor=predictions$posterior[,2])
plot(roc_obj, print.auc=TRUE, print.thres=TRUE, print.auc.y=0.2)