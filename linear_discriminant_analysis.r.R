# TODO: remove this from here, it will be present earlier in the markdown
library(MASS)
dataset <- read.csv("DataAssign2.csv")
set.seed(123)
train_indices <- sample(1:nrow(dataset), nrow(dataset) * 0.8)
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

# fit an lda model imported from `MASS` on the training data we split earlier
lda.fit <- lda(V7 ~ V1 + V2 + V3 + V4 + V5 + V6, data=train_data)
print(lda.fit)

# we can now use the model to predict on our test data
lda.preds <- predict(lda.fit, test_data)

# let's create a confusion matrix to discuss our model's performance
lda.classes <- lda.preds$class

actual <- test_data$V7

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
roc_obj <- roc(response=test_data$V7, predictor=lda.preds$posterior[,2])
# TODO: tune `print.auc.y`
plot(roc_obj, print.auc = TRUE, print.thres = TRUE, print.auc.y = 0.2)


# precision recall f1
# load metrics package
# library(Metrics)
# prec <- Metrics::precision(lda.preds, actual)
# recall <- Metrics::recall(lda.preds, actual)
# f1 <- Metrics::f1(lda.preds, actual)
# print("precision:",prec)
# print("recall:",recall)
# print("f1:",f1)