library(dplyr)

# Read the data from the CSV file
data <- read.csv("DataAssign2.csv")

# Display the structure of the dataset
str(data)

# Provide a summary of the dataset
summary(data)

# Display the first few rows of the dataset
head(data)

# Provide a concise summary of the dataset
glimpse(data)

# Explore unique values in the response variable (V7)
unique(data$V7)

# Check for missing values in the dataset
colSums(is.na(data))

# Display the proportion of each unique value in the response variable (V7)
prop.table(table(data$V7))
