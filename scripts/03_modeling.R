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
library(pdp)             # Partial dependence plots

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
    skewness = moments::skewness(charges),
    kurtosis = moments::kurtosis(charges),
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
## 3.1 Linear Models ####
# Features w/ eta-squared (effect size) > 0.6
# top 5 features= Simple
# full features

### features w/ 5 highest effect sizes = Simple
top_5_features <- top_features$feature_name[1:5] 
formula_simple <- as.formula(paste("charges ~", paste(top_5_features, collapse = " + ")))
### compound_lifestyle_risk_score not in essential data; as effect-size is more impactful, foregoing essential data as a modeling set 
# available_features <- top_5_features[top_5_features %in% names(train_data_ess_scaled)]
# formula_essential <- as.formula(paste("charges ~", paste(available_features, collapse = " + ")))

### 3.1.1 HI Scaled Simple LM top 5 features
lm_hi_scaled_simp <- lm(formula_simple, data = train_data_hi_scaled)
model_summary_simple <- summary(lm_hi_scaled_simp)

# Predictions and evaluation
hi_scaled_lm_pred_val <- predict(lm_hi_scaled_simp, validation_data_hi_scaled)
lm_rmse <- RMSE(hi_scaled_lm_pred_val, validation_data_hi_scaled$charges)
lm_r2 <- R2(hi_scaled_lm_pred_val, validation_data_hi_scaled$charges)

# save simple diagnostic plots
png("outputs/plots/simple_linear_diagnostics.png", width = 1200, height = 800, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(lm_hi_scaled_simp, main = "Simple Linear Model Diagnostics") ##generate 4 diagnostic plots: Residuals vs fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage
dev.off()

### 3.1.2 Top Features Linear Regression 
# Full model with Top Features
lm_full <- lm(charges ~ ., data = train_data_hi_scaled)
model_summary_full <- summary(lm_full)

# Save full model diagnostic plots
png("outputs/plots/full_linear_diagnostics.png", width = 1200, height = 800, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(lm_full, main = "Full Linear Model Diagnostics")
dev.off()

### 3.1.3
# Stepwise selection
lm_step <- step(lm_full, direction = "both")
model_summary_step <- summary(lm_step)

# Save stepwise model diagnostic plots
png("outputs/plots/stepwise_linear_diagnostics.png", width = 1200, height = 800, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(lm_step, main = "Stepwise Linear Model Diagnostics")
dev.off()

# Evaluate stepwise model
step_pred_val <- predict(lm_step, validation_data_hi_scaled)
step_rmse <- RMSE(step_pred_val, validation_data_hi_scaled$charges)
step_r2 <- R2(step_pred_val, validation_data_hi_scaled$charges)

plot(lm_full) 

### 3.1.4 Linear Model Comparison
# Linear models comparison
linear_model_results <- data.frame(
  Model = c("Simple (Top 5)", "Stepwise", "Full Model"),
  Features = c(length(top_5_features), 
               length(coef(lm_step)) - 1,
               length(coef(lm_full)) - 1),
  RMSE = c(lm_rmse, step_rmse, 
           RMSE(predict(lm_full, validation_data_hi_scaled), validation_data_hi_scaled$charges)),
  R_squared = c(lm_r2, step_r2,
                R2(predict(lm_full, validation_data_hi_scaled), validation_data_hi_scaled$charges)),
  Adj_R_squared = c(model_summary_simple$adj.r.squared, 
                    model_summary_step$adj.r.squared,
                    model_summary_full$adj.r.squared),
  AIC = c(AIC(lm_hi_scaled_simp), AIC(lm_step), AIC(lm_full))
)

# Round for readability
linear_model_results$RMSE <- round(linear_model_results$RMSE, 2)
linear_model_results$R_squared <- round(linear_model_results$R_squared, 4)
linear_model_results$Adj_R_squared <- round(linear_model_results$Adj_R_squared, 4)
linear_model_results$AIC <- round(linear_model_results$AIC, 1)

# Save linear model comparison
write.csv(linear_model_results, "outputs/tables/linear_model_comparison.csv", row.names = FALSE)

# Linear models performance comparison plot
png("outputs/plots/linear_models_performance.png", width = 1500, height = 500, res = 100)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(validation_data_hi_scaled$charges, hi_scaled_lm_pred_val,
     main = paste("Simple Model (R² =", round(lm_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("blue", 0.6))
abline(0, 1, col = "red", lwd = 2)

plot(validation_data_hi_scaled$charges, step_pred_val,
     main = paste("Stepwise Model (R² =", round(step_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("green", 0.6))
abline(0, 1, col = "red", lwd = 2)

full_pred_val <- predict(lm_full, validation_data_hi_scaled)
full_r2 <- R2(full_pred_val, validation_data_hi_scaled$charges)
plot(validation_data_hi_scaled$charges, full_pred_val,
     main = paste("Full Model (R² =", round(full_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("orange", 0.6))
abline(0, 1, col = "red", lwd = 2)

dev.off()

## 3.2 Regularized Models ####
### 3.2.1 Regularized Setup

# Prepare data for glmnet (using scaled data)
x_train <- model.matrix(charges ~ ., train_data_hi_scaled)[,-1]
y_train <- train_data_hi_scaled$charges
x_val <- model.matrix(charges ~ ., validation_data_hi_scaled)[,-1]
y_val <- validation_data_hi_scaled$charges

### 3.2.2 Ridge (adds penalty for large coefficients, good for correlated predictors)
# Ridge Regression with cross-validation
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)

# Ridge coefficient plot - INTERACTIVE PLOT 
ridge_plot <- plot(ridge_model) #5 star plot generated

ridge_pred <- predict(ridge_model, x_val, s = "lambda.min")
ridge_rmse <- RMSE(ridge_pred, y_val)
ridge_r2 <- R2(ridge_pred, y_val)

### 3.2.3 Lasso

# Lasso Regression with cross-validation
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Lasso coefficient plot
lasso_plot <- plot(lasso_model)

lasso_pred <- predict(lasso_model, x_val, s = "lambda.min")
lasso_rmse <- RMSE(lasso_pred, y_val)
lasso_r2 <- R2(lasso_pred, y_val)

### 3.2.4 Elastic
elastic_model <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 10)

# Elastic Net coefficient plot 
elastic_plot <- plot(elastic_model)

elastic_pred <- predict(elastic_model, x_val, s = "lambda.min")
elastic_rmse <- RMSE(elastic_pred, y_val)
elastic_r2 <- R2(elastic_pred, y_val)

### 3.2.5 Regularized Comparison
regularized_results <- data.frame(
  Model = c("Ridge", "Lasso", "Elastic Net"),
  RMSE = c(ridge_rmse, lasso_rmse, elastic_rmse),
  R_squared = c(ridge_r2, lasso_r2, elastic_r2),
  Lambda_Min = c(ridge_model$lambda.min, lasso_model$lambda.min, elastic_model$lambda.min),
  Features_Selected = c(
    sum(coef(ridge_model, s = "lambda.min")[-1] != 0),
    sum(coef(lasso_model, s = "lambda.min")[-1] != 0),
    sum(coef(elastic_model, s = "lambda.min")[-1] != 0)
  )
)

# Round for readability
regularized_results[,2:4] <- round(regularized_results[,2:4], 4)

write.csv(regularized_results, "outputs/tables/regularized_model_comparison.csv", row.names = FALSE)

# Regularized models performance plot
png("outputs/plots/regularized_models_performance.png", width = 1500, height = 500, res = 100)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(y_val, ridge_pred, main = paste("Ridge (R² =", round(ridge_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("purple", 0.6))
abline(0, 1, col = "red", lwd = 2)

plot(y_val, lasso_pred, main = paste("Lasso (R² =", round(lasso_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("darkgreen", 0.6))
abline(0, 1, col = "red", lwd = 2)

plot(y_val, elastic_pred, main = paste("Elastic Net (R² =", round(elastic_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("brown", 0.6))
abline(0, 1, col = "red", lwd = 2)

dev.off()



## 3.3 Tree Models ####
### 3.3.1 Random Forests

# Tune Random Forest
rf_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10))

rf_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE
)

rf_model <- train(
  charges ~ .,
  data = train_data_hi,
  method = "rf",
  tuneGrid = rf_grid,
  trControl = rf_control,
  ntree = 500
)

# Feature importance plot
RF_plot <- plot(varImp(rf_model), top = 15) #appears to align well with the feature eta-squared (yay!)

# Predictions
rf_pred <- predict(rf_model, validation_data_hi)
rf_rmse <- RMSE(rf_pred, validation_data_hi$charges)
rf_r2 <- R2(rf_pred, validation_data_hi$charges)

### 3.3.3 XGBoost 

# GBM tuning parameters
gbm_grid <- expand.grid(
  n.trees = c(100, 300, 500),
  interaction.depth = c(3, 5, 7),
  shrinkage = c(0.01, 0.1, 0.2),
  n.minobsinnode = c(10, 20)
)

gbm_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE
)

gbm_model <- train(
  charges ~ .,
  data = train_data_hi,
  method = "gbm",
  tuneGrid = gbm_grid,
  trControl = gbm_control,
  verbose = FALSE
)

# GBM tuning plot 
gbm_plot <- plot(gbm_model)

gbm_pred <- predict(gbm_model, validation_data_hi)
gbm_rmse <- RMSE(gbm_pred, validation_data_hi$charges)
gbm_r2 <- R2(gbm_pred, validation_data_hi$charges)

# GBM feature importance
png("outputs/plots/gbm_feature_importance.png", width = 1500, height = 600, res = 100)
plot(varImp(gbm_model), top = 15, main = "GBM Feature Importance")
dev.off()

# GBM performance plot
png("outputs/plots/gbm_performance.png", width = 1200, height = 600, res = 100)
par(mfrow = c(1, 2))

plot(validation_data_hi$charges, gbm_pred,
     main = paste("GBM (R² =", round(gbm_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("purple", 0.6))
abline(0, 1, col = "red", lwd = 2)

plot(gbm_pred, validation_data_hi$charges - gbm_pred,
     main = "GBM Residuals",
     xlab = "Predicted", ylab = "Residuals", pch = 16, col = alpha("purple", 0.6))
abline(h = 0, col = "red", lwd = 2)

dev.off()

# Training history plot
png("outputs/plots/gbm_training_history.png", width = 1000, height = 600, res = 100)
plot(gbm_model$finalModel, main = "GBM Training Error by Iteration")
dev.off()

### 3.3.4 Tree Model Comparison

tree_results <- data.frame(
  Model = c("Random Forest", "GBM"),
  RMSE = c(rf_rmse, gbm_rmse),
  R_squared = c(rf_r2, gbm_r2),
  Best_Parameters = c(
    paste("mtry =", rf_model$bestTune$mtry),
    paste("trees =", gbm_model$bestTune$n.trees, ", depth =", gbm_model$bestTune$interaction.depth)
  )
)

# Round for readability
tree_results[,2:3] <- round(tree_results[,2:3], 4)

write.csv(tree_results, "outputs/tables/tree_model_comparison.csv", row.names = FALSE)

# Tree models performance plot
png("outputs/plots/tree_models_performance.png", width = 1000, height = 500, res = 100)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(validation_data_hi$charges, rf_pred,
     main = paste("Random Forest (R² =", round(rf_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("forestgreen", 0.6))
abline(0, 1, col = "red", lwd = 2)

plot(validation_data_hi$charges, gbm_pred,
     main = paste("GBM (R² =", round(gbm_r2, 3), ")"),
     xlab = "Actual", ylab = "Predicted", pch = 16, col = alpha("darkblue", 0.6))
abline(0, 1, col = "red", lwd = 2)

dev.off()

# 4 Model Comparison ####
## 4.1 Performance Comparison ####
all_models_comparison <- bind_rows(
  linear_model_results %>% select(Model, RMSE, R_squared) %>% mutate(Type = "Linear"),
  regularized_results %>% select(Model, RMSE, R_squared) %>% mutate(Type = "Regularized"),
  tree_results %>% select(Model, RMSE, R_squared) %>% mutate(Type = "Tree-based")
) %>% 
  arrange(RMSE) %>%
  mutate(
    Rank_RMSE = row_number(),
    MAE = case_when(
      Model == "Simple (Top 5)" ~ MAE(hi_scaled_lm_pred_val, validation_data_hi_scaled$charges),
      Model == "Stepwise" ~ MAE(step_pred_val, validation_data_hi_scaled$charges),
      Model == "Full Model" ~ MAE(full_pred_val, validation_data_hi_scaled$charges),
      Model == "Ridge" ~ MAE(ridge_pred, y_val),
      Model == "Lasso" ~ MAE(lasso_pred, y_val),
      Model == "Elastic Net" ~ MAE(elastic_pred, y_val),
      Model == "Random Forest" ~ MAE(rf_pred, validation_data_hi$charges),
      Model == "GBM" ~ MAE(gbm_pred, validation_data_hi$charges)
    )
  )

# Round MAE
all_models_comparison$MAE <- round(all_models_comparison$MAE, 2)

write.csv(all_models_comparison, "outputs/tables/comprehensive_model_comparison.csv", row.names = FALSE)

# Comprehensive performance visualization
png("outputs/plots/comprehensive_model_comparison.png", width = 1200, height = 800, res = 100)
par(mfrow = c(2, 2), mar = c(5, 4, 2, 1))

# RMSE comparison
barplot(all_models_comparison$RMSE, names.arg = all_models_comparison$Model,
        las = 2, main = "RMSE Comparison", ylab = "RMSE", col = rainbow(nrow(all_models_comparison)))

# R-squared comparison  
barplot(all_models_comparison$R_squared, names.arg = all_models_comparison$Model,
        las = 2, main = "R-squared Comparison", ylab = "R-squared", col = rainbow(nrow(all_models_comparison)))

# MAE comparison
barplot(all_models_comparison$MAE, names.arg = all_models_comparison$Model,
        las = 2, main = "MAE Comparison", ylab = "MAE", col = rainbow(nrow(all_models_comparison)))

# Performance by model type
boxplot(RMSE ~ Type, data = all_models_comparison, main = "RMSE by Model Type", ylab = "RMSE")

dev.off()

## 4.2 Cross-validation Comparison ####
# 10-fold CV for top performing models (caret models only)
cv_models <- list()

# Add models that were trained with caret
if(exists("rf_model")) cv_models$RandomForest <- rf_model
if(exists("gbm_model")) cv_models$GBM <- gbm_model

if(length(cv_models) > 0) {
  cv_results <- resamples(cv_models)
  
  # CV results plot 
  bwplot(cv_results)
  
  # Save CV summary
  cv_summary <- summary(cv_results)
  write.csv(cv_summary$statistics$RMSE, "outputs/tables/cross_validation_results.csv")
}

## 4.3 RMSE Best Model ####
### RMSE - root mean square error <- avg distance between predicted and observed values 

##Best Model: GBM
best_model_name <- all_models_comparison$Model[1]
best_model_rmse <- all_models_comparison$RMSE[1]
best_model_r2 <- all_models_comparison$R_squared[1]

# Store the actual best model object
best_model <- switch(best_model_name,
                     "Simple (Top 5)" = lm_hi_scaled_simp,
                     "Stepwise" = lm_step,
                     "Full Model" = lm_full,
                     "Ridge" = ridge_model,
                     "Lasso" = lasso_model,
                     "Elastic Net" = elastic_model,
                     "Random Forest" = rf_model,
                     "GBM" = gbm_model
)

# Best model summary
best_model_summary <- data.frame(
  Metric = c("Model", "RMSE", "R-squared", "MAE", "Model Type"),
  Value = c(best_model_name, best_model_rmse, best_model_r2,
            all_models_comparison$MAE[1], all_models_comparison$Type[1])
)

write.csv(best_model_summary, "outputs/tables/best_model_summary.csv", row.names = FALSE)

# 5 Model Evaluation on Test Sets ####
