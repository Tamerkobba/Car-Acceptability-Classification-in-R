# Read the data from "DataAssign2.csv"
data <- read.csv("DataAssign2.csv")

# Convert all columns to factor variables
data[] <- lapply(data, as.factor)

# Function to calculate the p-value for the chi-square test between two categorical variables
calculate_chi_square <- function(var1, var2) {
  # Create a contingency table for the two variables
  contingency_table <- table(data[[var1]], data[[var2]])
  
  # Perform the chi-square test and retrieve the p-value
  chi_square_result <- chisq.test(contingency_table)
  return(chi_square_result$p.value)
}

# Create a matrix to store chi-square test results
chi_square_results <- matrix(NA, ncol = ncol(data), nrow = ncol(data))
rownames(chi_square_results) <- colnames(chi_square_results) <- colnames(data)

# Loop through all pairs of variables and calculate chi-square test p-values
for (i in 1:(ncol(data) - 1)) {
  for (j in (i + 1):ncol(data)) {
    # Get the names of the variables for the current pair
    var1 <- colnames(data)[i]
    var2 <- colnames(data)[j]
    
    # Calculate the chi-square p-value and store it in the matrix
    result <- calculate_chi_square(var1, var2)
    chi_square_results[var1, var2] <- chi_square_results[var2, var1] <- result
  }
}

# Print the matrix of chi-square test p-values
print(chi_square_results)
