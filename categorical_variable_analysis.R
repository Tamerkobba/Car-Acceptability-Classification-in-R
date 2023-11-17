# Load the required libraries
library(tidyverse)


# Read the data from the CSV file
data <- read_csv("DataAssign2.csv")


# Function to generate plot and summary for a categorical variable
plot_and_summary <- function(data, x_var, fill_var, y_var) {
  # Plot
  plot <- ggplot(data, aes_string(x = x_var, fill = fill_var)) +
    geom_bar() +
    labs(y = y_var, fill = "Acceptability")
  
  # Summary
  summary_table <- data %>%
    group_by_at(vars(x_var, fill_var)) %>%
    summarise(count = n()) %>%
    arrange(across(c(x_var, fill_var)))
  
  # Display the plot and return the summary table
  print(plot)
  return(summary_table)
}

# List of categorical variables
categorical_vars <- c("V1", "V2", "V3", "V4", "V5", "V6")

# Apply the function for each variable
results <- lapply(categorical_vars, function(var) {
  plot_and_summary(data, var, "V7", "Count")
})

