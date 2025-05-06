library(tidyverse)
library(caret)
library(iml)

# Load dataset and preprocess it
customer_data <- read_csv("customer_data_preprocessed.csv")

# Handle missing values and infinite values
customer_data <- customer_data %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  na.omit()
# Check column names in the dataset
names(customer_data)

# Exclude CustomerID before converting character columns to factors
customer_data <- customer_data %>%
  select(-CustomerID) %>% # Remove CustomerID
  mutate(across(where(is.character), as.factor))

# Ensure Churn is a factor
customer_data$Churn <- as.factor(customer_data$Churn)

str(customer_data)
# Check class distribution
table(customer_data$Churn)

library(dplyr)

# Downsample majority class
majority_class <- customer_data %>% filter(Churn == "No")
minority_class <- customer_data %>% filter(Churn == "Yes")

set.seed(123)
majority_downsampled <- majority_class %>% sample_n(nrow(minority_class))

# Combine downsampled majority and minority classes
balanced_data <- bind_rows(majority_downsampled, minority_class)

# Verify class distribution
table(balanced_data$Churn)

library(fastDummies)

# Example of one-hot encoding ContractType
customer_data <- dummy_cols(customer_data, select_columns = "ContractType")

# Verify column names after encoding
names(customer_data)

# Explicitly define predictors if automatic selection fails
rf_model <- randomForest(
  Churn ~ Age + Tenure + MonthlyCharges + TotalCharges + ContractType_Month-to-Month,
  data = customer_data,
  sampsize = c(6, 6),
  strata = customer_data$Churn
)






##########################################################################
library(randomForest)

# Train weighted Random Forest
rf_model <- randomForest(
  Churn ~ ., 
  data = customer_data,
  sampsize = c(6, 6), # Equalize samples for each class
  strata = customer_data$Churn # Stratify sampling by Churn
)

# Prepare data for SHAP analysis (exclude CustomerID and Churn)
#predictor_data <- customer_data %>%
#  select(-CustomerID, -Churn)
# Verify structure of predictor data
str(predictor_data)

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
