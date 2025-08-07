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

# reference tables
complete_feature_analysis <- read_csv("outputs/tables/complete_feature_analysis_final.csv")
top_features <- read_csv("outputs/tables/top_performing_features.csv")

# 1 Data Exploration ####

## 1.1 Data Summary ####

### 1.1.1 High Impact 
dataset_summary <- tibble(
  dataset = "High Impact Data",
  n_features = ncol(high_impact_data) - 1,
  n_observations = nrow(high_impact_data),
  target_mean = mean(high_impact_data$charges),
  target_sd = sd(high_impact_data$charges)
)

### 1.1.2 High Impact Scaled
dataset_summary_hi_scaled <- tibble(
  dataset = "High Impact Scaled Data",
  n_features = ncol(high_impact_scaled) - 1,
  n_observations = nrow(high_impact_scaled),
  target_mean = mean(high_impact_scaled$charges),
  target_sd = sd(high_impact_scaled$charges)
)

### 1.1.3 Essential 
dataset_summary_ess <- tibble(
  dataset = "Essential Data",
  n_features = ncol(essential_data) - 1,
  n_observations = nrow(essential_data),
  target_mean = mean(essential_data$charges),
  target_sd = sd(essential_data$charges)
)

### 1.1.4 Essential Scaled
dataset_summary_ess_scaled <- tibble(
  dataset = "Essential Scaled Data",
  n_features = ncol(essential_scaled) - 1,
  n_observations = nrow(essential_scaled),
  target_mean = mean(essential_scaled$charges),
  target_sd = sd(essential_scaled$charges)
)
  
## 1.2 Target Variable Analysis ####

### 1.2.1 High Impact
charges_analysis <- high_impact_data %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

### 1.2.2 High Impact Scaled TVA
charges_analysis_hi_scaled <- high_impact_scaled %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

### 1.2.3 Essential TVA
charges_analysis_ess <- essential_data %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

### 1.2.4 Essential Scaled TVA
charges_analysis_ess_scaled <- essential_scaled %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

## 1.3 Feature Correlation ####

### 1.3.1 High Impact 
numeric_features <- high_impact_data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_features, use = "complete.obs")

high_cor_features_hi <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)

### 1.3.2 High Impact Scaled Feature Corr
numeric_features_hi_scaled <- high_impact_scaled %>% select_if(is.numeric)
cor_matrix_hi_scaled <- cor(numeric_features_hi_scaled, use = "complete.obs")

high_cor_features_hi_scaled <- findCorrelation(cor_matrix_hi_scaled, cutoff = 0.8, names = TRUE)

### 1.3.3 Essential Feature Corr
numeric_features_ess <- essential_data %>% select_if(is.numeric)
cor_matrix_ess <- cor(numeric_features_ess, use = "complete.obs")

high_cor_features_ess <- findCorrelation(cor_matrix_ess, cutoff = 0.8, names = TRUE)

### 1.3.4 Essenital Scaled FEature Corr
numeric_features_ess_scaled <- essential_scaled %>% select_if(is.numeric)
cor_matrix_ess_scaled <- cor(numeric_features_ess_scaled, use = "complete.obs")

high_cor_features_ess_scaled <- findCorrelation(cor_matrix_ess_scaled, cutoff = 0.8, names = TRUE)

# Identify highly correlated features (>0.8)
high_cor_features <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)

# 2 Data Pre-processing ####

set.seed(123)  # For reproducibility 

## 2.1 Missing Values Check(MVC) ####

### 2.1.1 High Impact Missing Values check
missing_summary <- high_impact_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  mutate(missing_percentage = missing_count / nrow(high_impact_data) * 100) %>%
  filter(missing_count > 0)

### 2.1.2 High Impact Scaled MVC
missing_summary_hi_scaled <- high_impact_scaled %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  mutate(missing_percentage = missing_count / nrow(high_impact_scaled) * 100) %>%
  filter(missing_count > 0)

### 2.1.3 Essential MVC
missing_summary_ess <- essential_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  mutate(missing_percentage = missing_count / nrow(essential_data) * 100) %>%
  filter(missing_count > 0)

### 2.1.4 Essential Scaled MVC
missing_summary_ess_scaled <- essential_scaled %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  mutate(missing_percentage = missing_count / nrow(essential_scaled) * 100) %>%
  filter(missing_count > 0)

## 2.2 Train/Validation/Test Split for modeling_data ####

### 2.2.1 Create Train/Validation/Test Splits for High Impact 
# Create stratified split based on charges quartiles
high_impact_data$charges_quartile <- ntile(high_impact_data$charges, 4)

# 70% train, 15% validation, 15% test
train_idx_hi <- createDataPartition(high_impact_data$charges_quartile, p = 0.70, list = FALSE)
temp_data_hi <- high_impact_data[-train_idx_hi, ]
val_test_idx_hi <- createDataPartition(temp_data_hi$charges_quartile, p = 0.5, list = FALSE)

train_data_hi <- high_impact_data[train_idx_hi, ] %>% select(-charges_quartile)
validation_data_hi <- temp_data_hi[val_test_idx_hi, ] %>% select(-charges_quartile)
test_data_hi <- temp_data_hi[-val_test_idx_hi, ] %>% select(-charges_quartile)

### 2.2.2 Create Train/Validation/Test Splits for High Impact Scaled
# Create stratified split based on charges quartiles
high_impact_scaled$charges_quartile <- ntile(high_impact_scaled$charges, 4)

# 70% train, 15% validation, 15% test
train_idx_hi_scaled <- createDataPartition(high_impact_scaled$charges_quartile, p = 0.70, list = FALSE)
temp_data_hi_scaled <- high_impact_scaled[-train_idx_hi_scaled, ]
val_test_idx_hi_scaled <- createDataPartition(temp_data_hi_scaled$charges_quartile, p = 0.5, list = FALSE)

train_data_hi_scaled <- high_impact_scaled[train_idx_hi_scaled, ] %>% select(-charges_quartile)
validation_data_hi_scaled <- temp_data_hi_scaled[val_test_idx_hi_scaled, ] %>% select(-charges_quartile)
test_data_hi_scaled <- temp_data_hi_scaled[-val_test_idx_hi_scaled, ] %>% select(-charges_quartile)

### 2.2.3 Create Train/Validation/Test Splits for Essential
# Create stratified split based on charges quartiles
essential_data$charges_quartile <- ntile(essential_data$charges, 4)

# 70% train, 15% validation, 15% test
train_idx_ess <- createDataPartition(essential_data$charges_quartile, p = 0.70, list = FALSE)
temp_data_ess <- essential_data[-train_idx_ess, ]
val_test_idx_ess <- createDataPartition(temp_data_ess$charges_quartile, p = 0.5, list = FALSE)

train_data_ess <- essential_data[train_idx_ess, ] %>% select(-charges_quartile)
validation_data_ess <- temp_data_ess[val_test_idx_ess, ] %>% select(-charges_quartile)
test_data_ess <- temp_data_ess[-val_test_idx_ess, ] %>% select(-charges_quartile)

### 2.2.4 Create Train/Validation/Test Splits for Essential Scaled
# Create stratified split based on charges quartiles
essential_scaled$charges_quartile <- ntile(essential_scaled$charges, 4)

# 70% train, 15% validation, 15% test
train_idx_ess_scaled <- createDataPartition(essential_scaled$charges_quartile, p = 0.70, list = FALSE)
temp_data_ess_scaled <- essential_scaled[-train_idx_ess_scaled, ]
val_test_idx_ess_scaled <- createDataPartition(temp_data_ess_scaled$charges_quartile, p = 0.5, list = FALSE)

train_data_ess_scaled <- essential_scaled[train_idx_ess_scaled, ] %>% select(-charges_quartile)
validation_data_ess_scaled <- temp_data_ess_scaled[val_test_idx_ess_scaled, ] %>% select(-charges_quartile)
test_data_ess_scaled <- temp_data_ess_scaled[-val_test_idx_ess_scaled, ] %>% select(-charges_quartile)

# 3 Models ####
## Linear Regression (baseline performance, scaled)
## Regularized Regression (Ridge, Lasso, and Elastic Net; good for over fitting, scaled)
## Random Forest (feature importance validation, scale-invariant)
## XGBoost (gradient boosting benchmark, scale invariant)

## 3.1 Linear Regression ####
# Features w/ eta-squared (effect size) > 0.6
# top 5 features= Simple

### features w/ 5 highest effect sizes = Simple
top_5_features <- top_features$feature_name[1:5] 
formula_simple <- as.formula(paste("charges ~", paste(top_5_features, collapse = " + ")))
### compound_lifestyle_risk_score not in essential data; as effect-size is more impactful, foregoing essential data as a modeling set 
# available_features <- top_5_features[top_5_features %in% names(train_data_ess_scaled)]
# formula_essential <- as.formula(paste("charges ~", paste(available_features, collapse = " + ")))

### 3.1.1 HI Scaled Simple LM
lm_hi_scaled_simp <- lm(formula_simple, data = train_data_hi_scaled)
summary(lm_hi_scaled_simp)

# Predictions and evaluation
hi_scaled_lm_pred_val <- predict(lm_hi_scaled_simp, validation_data_hi_scaled)
lm_rmse <- RMSE(hi_scaled_lm_pred_val, validation_data_hi_scaled$charges)
lm_r2 <- R2(hi_scaled_lm_pred_val, validation_data_hi_scaled$charges)

plot(lm_hi_scaled_simp)

### 3.1.2 Top Features Linear Regression ###
# Full model with Top Features
lm_full <- lm(charges ~ ., data = train_data_hi_scaled)
summary(lm_full)

# Stepwise selection
lm_step <- step(lm_full, direction = "both")
summary(lm_step)

# Evaluate stepwise model
step_pred_val <- predict(lm_step, validation_data_hi_scaled)
step_rmse <- RMSE(step_pred_val, validation_data_hi_scaled$charges)
step_r2 <- R2(step_pred_val, validation_data_hi_scaled$charges)

plot(lm_full)

## 3.2 Regularized Regression ####


