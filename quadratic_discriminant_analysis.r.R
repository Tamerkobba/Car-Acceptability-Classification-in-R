# TODO: remove this from here, it will be present earlier in the markdown
library(MASS)
library(caret)
noncat_dataset <- read.csv("car.csv")
cat_dataset <- dummyVars("~ . - Car_Acceptability", data=noncat_dataset)
dataset <- data.frame(predict(cat_dataset, newdata=noncat_dataset))

set.seed(965)
train_indices <- sample(1:nrow(dataset), nrow(dataset) * 0.8)
train_data_X <- dataset[train_indices, ]
test_data_X <- dataset[-train_indices, ]
train_data_Y <- noncat_dataset$Car_Acceptability[train_indices]
test_data_Y <- noncat_dataset$Car_Acceptability[-train_indices]

# FITTING THE MODEL
model <- qda(train_data_Y ~ ., data=train_data_X)
model

# TEST
predictions <- predict(model, test_data_X)

# CONFUSION MATRIX
classes <- predictions$class

actual <- test_data_Y

confusion_matrix <- table(classes, actual)
print(confusion_matrix)

true_negative <- confusion_matrix[1]
false_positive <- confusion_matrix[2]
false_negative <- confusion_matrix[3]
true_positive <- confusion_matrix[4]

# METRICS
accuracy <- (true_positive + true_negative) / (sum(confusion_matrix))
accuracy

precision <- true_positive / (true_positive + false_positive)
precision

recall <- true_positive / (true_positive + false_negative)
recall

f1_score <- 2 * ((precision * recall) / (precision + recall))
f1_score
