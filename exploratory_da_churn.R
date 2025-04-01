# Load the preprocessed dataset
# Load necessary libraries
library(tidyverse)

# Load the dataset
customer_data <- read_csv("customer_data_preprocessed.csv")

# View the first few rows of the dataset
head(customer_data)


# Visualize Churn Distribution Across Features
## Churn Distribution
# Bar plot for churn distribution
ggplot(customer_data, aes(x = as.factor(Churn))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Churn Distribution", x = "Churn (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()

## Churn by Contract Type
# Bar plot for churn by contract type
ggplot(customer_data, aes(x = ContractType, fill = as.factor(Churn))) +
  geom_bar(position = "dodge") +
  labs(title = "Churn by Contract Type", x = "Contract Type", y = "Count", fill = "Churn") +
  theme_minimal()

## Monthly Charges vs Churn
# Box plot for Monthly Charges vs. Churn
ggplot(customer_data, aes(x = as.factor(Churn), y = MonthlyCharges)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Monthly Charges vs. Churn", x = "Churn (0 = No, 1 = Yes)", y = "Monthly Charges") +
  theme_minimal()

# Identify Correlation Using Heatmaps
## Compute Correlation Matrix
# Select numeric columns
numeric_columns <- customer_data %>% select_if(is.numeric)

# Compute correlation matrix
# Check if cor_matrix exists
exists("cor_matrix")  # Returns TRUE if the object exists, FALSE otherwise
# Ensure numeric columns are selected from the dataset
numeric_columns <- customer_data %>% select_if(is.numeric)

# Compute correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")

print(cor_matrix)

## Visualize the Correlation Matrix using corrplot
# Install and load corrplot package
install.packages("corrplot")
library(corrplot)

# Create a correlation heatmap
corrplot(cor_matrix, method = "color", type = "lower",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Heatmap", mar = c(0, 0, 1, 0))

## Visualize Correlation Matrix using ggplot2

# Install and load reshape2 for melting the correlation matrix
install.packages("reshape2")
library(reshape2)

# Melt the correlation matrix into long format
melted_cor_matrix <- melt(cor_matrix)

# Create heatmap with ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Correlation Heatmap", x="", y="")














