# Load required libraries
library(class)
library(ROCR)
library(caret)
library(pROC)  # Include pROC library for ROC curve plotting

# Read the data from "DataAssign2.csv"
data <- read.csv("DataAssign2.csv")

# Select columns V1, V2, V5, V6, and V7
selected_columns <- data[, c("V1", "V2", "V5", "V6", "V7")]

# Create dummy variables
data_dummy <- dummyVars(" ~ .", data = selected_columns)
data_dummy <- data.frame(predict(data_dummy, newdata = selected_columns))

# Display the structure of the new data frame
str(data_dummy)

# Set seed for reproducibility
set.seed(123)

# Define the sample size for training data
samplesize1 <- round(0.75 * nrow(data_dummy), 0)

# Create indices for training data
index1 <- sample(seq_len(nrow(data_dummy)), size = samplesize1)



# Create training and testing datasets
kn_train <- data_dummy[index1, ]

str(kn_train)

kn_test <- data_dummy[-index1, ]


# Create training and testing labels (response variable V7)
kn_train_label <- selected_columns$V7[index1]
kn_test_label <- selected_columns$V7[-index1]


# Define a range of K values
k_values <- seq(1, 100, by = 1)


# Initialize a vector to store test errors
test_errors <- numeric(length(k_values))

# Perform KNN for each K value
for (k in k_values) {
  # Perform k-nearest neighbors classification
  pred_knn <- knn(train = kn_train,
                  test = kn_test, 
                  cl = kn_train_label, 
                  k = k)
  
  # Create a confusion matrix
  conf_matrix <- confusionMatrix(table(pred_knn, kn_test_label))
  
  # Calculate accuracy (test error)
  accuracy <- conf_matrix$overall["Accuracy"]
  test_errors[k] <- 1 - accuracy
}


# Plot test errors for different values of K
plot(k_values, test_errors, type = "b", pch = 19, col = "steelblue", 
     xlab = "Number of Neighbors (K)", ylab = "Test Error", 
     main = "Test Error vs. Number of Neighbors")

# Identify the K value with the minimum test error
min_error_index <- which.min(test_errors)
min_error_k <- k_values[min_error_index]

# Highlight the point with the minimum test error
points(min_error_k, test_errors[min_error_index], col = "red", pch = 19)

# Print the minimum test error and the corresponding K value
cat("Minimum Test Error:", round(test_errors[min_error_index], 4), "\n")
cat("Optimal K value:", min_error_k, "\n")

# Perform k-nearest neighbors classification with the optimal K value
k_value_chosen <-min_error_k
pred_knn <- knn(train = kn_train,
                test = kn_test, 
                cl = kn_train_label, 
                k = k_value_chosen)

# Create a confusion matrix
conf_matrix <- confusionMatrix(table(pred_knn, kn_test_label))
print(conf_matrix)

# Calculate precision, recall, and F1 score
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Recall']
f1_score <- conf_matrix$byClass['F1']

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Create V7_binary in kn_test based on predictions

# Create V7_binary in kn_test based on predictions
kn_test$V7_binary <- ifelse(pred_knn == "bad", "Bad", "Good")
# Define object to plot and calculate AUC
rocobj <- roc(as.factor(kn_test$V7_binary), as.numeric(pred_knn))

# Calculate AUC
auc <- round(auc(rocobj), 4)

# Create ROC plot using ggroc from pROC
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  theme_minimal()
