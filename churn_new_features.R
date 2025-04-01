library(tidyverse)
library(caret)
library(iml)

# Load dataset and preprocess it
customer_data <- read_csv("customer_data_preprocessed.csv")

# Handle missing values and infinite values
customer_data <- customer_data %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  na.omit()

# Convert character columns to factors and ensure target variable is a factor
customer_data <- customer_data %>%
  mutate(across(where(is.character), as.factor))
customer_data$Churn <- as.factor(customer_data$Churn)

# Train a Random Forest model using caret
rf_model <- train(Churn ~ ., data = customer_data, method = "rf")

# Prepare data for SHAP analysis (exclude CustomerID and Churn)
# Exclude only Churn while retaining other predictors
predictor_data <- customer_data[, -which(names(customer_data) == "Churn")]

# Verify structure of predictor data
str(predictor_data)

# Remove CustomerID and Churn from the dataset
predictor_data <- customer_data %>%
  select(-CustomerID, -Churn)

# Verify structure of predictor data
str(predictor_data)



# Create a Predictor object for SHAP analysis
predictor <- Predictor$new(
  model = rf_model,
  data = predictor_data,
  y = customer_data$Churn
)

# Compute SHAP values and plot summary plot
shap_values <- Shapley$new(predictor)
shap_values$plot()
