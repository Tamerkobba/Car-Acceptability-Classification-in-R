# TODO: remove this from here, it will be present earlier in the markdown
library(MASS)
library(caret)
noncat_dataset <- read.csv("DataAssign2.csv")
# TODO: exclude useless columns
cat_dataset <- dummyVars("~ . - V7 - V3 - V4", data=noncat_dataset)
dataset <- data.frame(predict(cat_dataset, noncat_dataset))

set.seed(965)
train_indices <- sample(1:nrow(dataset), nrow(dataset) * 0.75)
train_data_X <- dataset[train_indices, ]
test_data_X <- dataset[-train_indices, ]
train_data_Y <- noncat_dataset$V7[train_indices]
test_data_Y <- noncat_dataset$V7[-train_indices]


# fit an lda model imported from `MASS` on the training data we split earlier
lda.fit <- lda(train_data_Y ~ ., data=train_data_X)
print(lda.fit)

# we can now use the model to predict on our test data
lda.preds <- predict(lda.fit, test_data_X)

# let's create a confusion matrix to discuss our model's performance
lda.classes <- lda.preds$class

actual <- test_data_Y

confusion_matrix <- table(lda.classes, actual)
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
roc_obj <- roc(response=test_data_Y, predictor=lda.preds$posterior[,2])
# TODO: tune `print.auc.y`
plot(roc_obj, print.auc = TRUE, print.thres = TRUE, print.auc.y = 0.2)

