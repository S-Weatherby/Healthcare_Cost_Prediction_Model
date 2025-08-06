# Modeling Script - Script 03
# Author: Shelita Smith
# Date: August 2025
# Purpose: Predictive modeling for insurance charges using engineered features
# Goals: Multiple model comparison, validation, interpretation, and deployment preparation

# 0 Setup ####

## libraries
library(tidyverse)
library(caret)           # For model training and evaluation
library(randomForest)    # Random Forest (modeling)
library(glmnet)          # Ridge/Lasso regression
library(e1071)           # SVM (model)
library(gbm)             # Gradient Boosting
library(corrplot)        # Correlation visualization
library(VIM)             # Missing data visualization
library(pROC)            # ROC curves (if classification)
library(ModelMetrics)    # Additional metrics

# prepared datasets
load_data_safely <- function(file_path) {
  if (file.exists(file_path)) {
    return(read_csv(file_path, show_col_types = FALSE))
  } else {
    return(NULL)
  }
}

high_impact_data <- load_data_safely("data/processed/modeling_data_high_impact.csv")
essential_data <- load_data_safely("data/processed/modeling_data_essential.csv")
high_impact_scaled <- load_data_safely("data/processed/modeling_data_high_impact_scaled.csv")
essential_scaled <- load_data_safely("data/processed/modeling_data_essential_scaled.csv")
ential_scaled <- read_csv("data/processed/modeling_data_essential_scaled.csv")

# reference tables
complete_feature_analysis <- read_csv("outputs/tables/complete_feature_analysis_final.csv")
top_features <- read_csv("outputs/tables/top_performing_features.csv")

# high impact data as primary dataset
modeling_data <- high_impact_data

# 1 Data Exploration ####

dataset_summary <- tibble(
  dataset = "Primary Modeling Data",
  n_features = ncol(modeling_data) - 1,
  n_observations = nrow(modeling_data),
  target_mean = mean(modeling_data$charges),
  target_sd = sd(modeling_data$charges)
)

## 1.2 Target Variable Analysis ####
charges_analysis <- modeling_data %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

# 1.3 Feature Correlation ####
numeric_features <- modeling_data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_features, use = "complete.obs")

# Identify highly correlated features (>0.8)
high_cor_features <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)

# 2 Data Pre-processing ####

## 2.1 Missing values check
missing_summary <- modeling_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  mutate(missing_percentage = missing_count / nrow(modeling_data) * 100) %>%
  filter(missing_count > 0)

## 2.2 Train/Validation/Test Split for modeling_data (High_Impact_data_unscaled) ####

set.seed(123)  # For reproducibility

# Create stratified split based on charges quartiles
modeling_data$charges_quartile <- ntile(modeling_data$charges, 4)

# 70% train, 15% validation, 15% test
train_indices <- createDataPartition(modeling_data$charges_quartile, 
                                     p = 0.70, list = FALSE)
temp_data <- modeling_data[-train_indices, ]
val_test_indices <- createDataPartition(temp_data$charges_quartile, 
                                        p = 0.5, list = FALSE)

train_data <-modeling_data[train_indices, ] %>% select(-charges_quartile)
validation_data <- temp_data[val_test_indices, ] %>% select(-charges_quartile)
test_data <- temp_data[-val_test_indices, ] %>% select(-charges_quartile)

## 2.3 Train/Validation/Test Split for other ______ data set; to be done ####

# 3 Baseline Models ####
## Linear Regression, (baseline performance)
## Random Forest (feature importance validation)
## XGBoost (gradient boosting benchmark)

## 3.1 Simple Linear Regression ####
# Top 5 features from ANOVA analysis

top_5_features <- top_features$feature_name[1:5]
formula_simple <- as.formula(paste("charges ~", paste(top_5_features, collapse = " + ")))

lm_baseline <- lm(formula_simple, data = train_data)
summary(lm_baseline)

# Predictions and evaluation
lm_pred_val <- predict(lm_baseline, validation_data)
lm_rmse <- RMSE(lm_pred_val, validation_data$charges)
lm_r2 <- R2(lm_pred_val, validation_data$charges)

## 3.2 All Features Linear Regression ####
# Full model with all available features
lm_full <- lm(charges ~ ., data = train_data)
summary(lm_full)

# Stepwise selection
lm_step <- step(lm_full, direction = "both")
summary(lm_step)

# Evaluate stepwise model
step_pred_val <- predict(lm_step, validation_data)
step_rmse <- RMSE(step_pred_val, validation_data$charges)
step_r2 <- R2(step_pred_val, validation_data$charges)
