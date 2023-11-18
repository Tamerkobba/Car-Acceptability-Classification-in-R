
data <- read.csv("DataAssign2.csv")

data[] <- lapply(data, as.factor)

calculate_chi_square <- function(var1, var2) {
  contingency_table <- table(data[[var1]], data[[var2]])
  chi_square_result <- chisq.test(contingency_table)
  return(chi_square_result$p.value)
}


chi_square_results <- matrix(NA, ncol = ncol(data), nrow = ncol(data))
rownames(chi_square_results) <- colnames(chi_square_results) <- colnames(data)

for (i in 1:(ncol(data) - 1)) {
  for (j in (i + 1):ncol(data)) {
    var1 <- colnames(data)[i]
    var2 <- colnames(data)[j]
    result <- calculate_chi_square(var1, var2)
    chi_square_results[var1, var2] <- chi_square_results[var2, var1] <- result
  }
}

print(chi_square_results)

