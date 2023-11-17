
library(ggplot2)
library(pROC)

data <- read.csv("DataAssign2.csv")
data$V7_binary <- ifelse(data$V7 == 'good', 1, 0)

# Set a random seed for reproducibility
set.seed(123)

# Generate random indices for the training set, equivalent to an 80-20 split
train_indices <- sample(1:nrow(data), nrow(data) * 0.8)

# Create the training set using the sampled indices
train_data <- data[train_indices, ]

# Create the test set by excluding the training set indices
test_data <- data[-train_indices, ]

# Create a binary response variable
data$V7_binary <- ifelse(data$V7 == 'good', 1, 0)

# Fit the logistic regression model using V7_binary as the response variable
model <- glm(formula = V7_binary ~ V6 + V2 + V1 + V5, data = train_data, family = binomial)

# Print the summary of the model
summary(model)

# Predict on the test set
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to class predictions (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(Actual = test_data$V7_binary, Predicted = predicted_classes)
# Convert the confusion matrix to a data frame
conf_matrix_df <- as.data.frame.matrix(conf_matrix)
# Plot the confusion matrix
print(conf_matrix)
# Calculate the accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat(paste("Accuracy:", accuracy * 100))

# Calculate precision, recall, and F1 score for each class
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print precision, recall, and F1 score
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Define object to plot and calculate AUC
rocobj <- roc(test_data$V7_binary, predictions)
auc <- round(auc(rocobj),4)

# Create ROC plot
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  theme_minimal()

