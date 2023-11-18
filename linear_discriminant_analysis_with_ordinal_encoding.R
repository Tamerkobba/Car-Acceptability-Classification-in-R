# TODO: remove this from here, it will be present earlier in the markdown
library(MASS)
library(caret)
dataset <- read.csv("DataAssign2.csv")
# TODO: exclude useless columns
dataset$V1 <- as.numeric(factor(dataset$V1))
dataset$V2 <- as.numeric(factor(dataset$V2))
dataset$V3 <- as.numeric(factor(dataset$V3))
dataset$V4 <- as.numeric(factor(dataset$V4))
dataset$V5 <- as.numeric(factor(dataset$V5))
dataset$V6 <- as.numeric(factor(dataset$V6))


set.seed(965)
train_indices <- sample(1:nrow(dataset), nrow(dataset) * 0.75)
data_X <- dataset[c("V1", "V2", "V5", "V6")] # pick features here
data_y <- dataset["V7"]
train_data_X <- data_X[train_indices, ]
train_data_y <- data_y$V7[train_indices]
test_data_X <- data_X[-train_indices, ]
test_data_y <- data_y$V7[-train_indices]

# fit an lda model imported from `MASS` on the training data we split earlier
model <- lda(train_data_y ~ ., data=train_data_X)
print(model)

# we can now use the model to predict on our test data
predictions <- predict(model, test_data_X)

# let's create a confusion matrix to discuss our model's performance
classes <- predictions$class

actual <- test_data_y

confusion_matrix <- table(classes, actual)
print(confusion_matrix)

true_negative <- confusion_matrix[1]
false_positive <- confusion_matrix[2]
false_negative <- confusion_matrix[3]
true_positive <- confusion_matrix[4]

accuracy <- (true_positive + true_negative) / (sum(confusion_matrix))
accuracy

precision <- true_positive / (true_positive + false_positive)
precision

recall <- true_positive / (true_positive + false_negative)
recall

f1_score <- 2 * ((precision * recall) / (precision + recall))
f1_score

# ROC CURVE
library(pROC)
roc_obj <- roc(response=test_data_y, predictor=predictions$posterior[,2])
# TODO: tune `print.auc.y`
plot(roc_obj, print.auc = TRUE, print.thres = TRUE, print.auc.y = 0.2)

