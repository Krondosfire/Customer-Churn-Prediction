# Sample dataset creation
customer_data <- data.frame(
  CustomerID = 1:10,
  Gender = c("Male", "Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male"),
  Age = c(34, 45, 23, 54, 31, 29, 40, 37, 50, 28),
  Tenure = c(12, 24, 6, 36, 18, 9, 30, 15, 40, 8),
  MonthlyCharges = c(70.5, 85.2, 40.3, 120.0, 65.4, 55.2, 95.0, 75.5, 130.0, 50.0),
  TotalCharges = c(846.0, NA, NA, 4320.0, NA, NA, NA, NA, NA, NA),
  ContractType = c("Month-to-Month", "Two-Year", "Month-to-Month", "One-Year", "Two-Year", 
                   "Month-to-Month", "One-Year", "Two-Year", "One-Year", "Month-to-Month"),
  Churn = c("No", "No", "Yes", "No", "No", "Yes", "No", "Yes", "No", "Yes")
)

# View the dataset
print(customer_data)

# Handle missing data
# Replace missing values in TotalCharges with the median
customer_data$TotalCharges[is.na(customer_data$TotalCharges)] <- median(customer_data$TotalCharges, na.rm = TRUE)

#Handle Outliers
# Calculate IQR for MonthlyCharges
Q1 <- quantile(customer_data$MonthlyCharges, 0.25)
Q3 <- quantile(customer_data$MonthlyCharges, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Replace outliers with the median value
customer_data$MonthlyCharges[customer_data$MonthlyCharges < lower_bound | customer_data$MonthlyCharges > upper_bound] <- median(customer_data$MonthlyCharges)

# Encode Categorical Variables
# One-hot encoding for ContractType
install.packages("fastDummies")
library(fastDummies)
customer_data <- dummy_cols(customer_data, select_columns = "ContractType")

# View the updated dataset
print(head(customer_data))
# Label encoding for Gender and Churn
customer_data$Gender <- as.numeric(factor(customer_data$Gender, levels = c("Male", "Female")))
customer_data$Churn <- as.numeric(factor(customer_data$Churn, levels = c("No", "Yes")))

# Normalize Numerical Features

# Normalize Age, Tenure, MonthlyCharges, and TotalCharges using Min-Max scaling
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

customer_data$Age <- normalize(customer_data$Age)
customer_data$Tenure <- normalize(customer_data$Tenure)
customer_data$MonthlyCharges <- normalize(customer_data$MonthlyCharges)
customer_data$TotalCharges <- normalize(customer_data$TotalCharges)

# Final Preprocessed Dataset
# View preprocessed dataset
print(head(customer_data))

# Save the preprocessed dataset to a CSV file in the current directory
write.csv(customer_data, "customer_data_preprocessed.csv", row.names = FALSE)

# Confirm the file is saved by checking the working directory
print("Preprocessed dataset saved as 'customer_data_preprocessed.csv' in the current directory.")












