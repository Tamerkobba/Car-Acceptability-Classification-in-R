# Read the data from the CSV file
data <- read.csv("DataAssign2.csv")

# Set a random seed for reproducibility
set.seed(123)

# Generate random indices for the training set, equivalent to an 80-20 split
train_indices <- sample(1:nrow(data), nrow(data) * 0.8)

# Create the training set using the sampled indices
train_data <- data[train_indices, ]

# Create the test set by excluding the training set indices
test_data <- data[-train_indices, ]
