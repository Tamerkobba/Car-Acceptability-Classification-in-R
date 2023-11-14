# Load necessary packages
library(ggplot2)
library(polycor)

# Read the data from the CSV file
data <- read.csv("DataAssign2.csv")
# Rename variables for better understanding
colnames(data) <- c("Buying_Price", "Maintenance_Price", "Number_of_Doors", 
                    "Capacity", "Luggage_Boot_Size", "Estimated_Safety", "Acceptability")

# Convert all variables to factors
data[] <- lapply(data, as.factor)


# Calculate the correlation matrix using hetcor
correlation_result <- hetcor(data)

# Extract the correlation matrix
correlation_matrix <- correlation_result$correlations


# Create a heatmap with negative values in the scale
heatmap_plot <- ggplot(data = as.data.frame(as.table(correlation_matrix)),
                       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  labs(title = "Categorical Variable Correlation Heatmap",
       x = "Variable 1", y = "Variable 2") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-0.5, 1))

# Print the heatmap
print(heatmap_plot)


