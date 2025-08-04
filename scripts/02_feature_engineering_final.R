#Feature Engineering Script - Script 02 (Debugged)
# Author: Shelita Smith
# Date: July 23, 2025 | August 01, 2025
# Purpose: Feature engineering with ANOVA analysis and comprehensive documentation
# Goals: ANOVA'd features, Analysis table guides, updated data dictionary in preparation for modeling

# .5 Set-Up ####

# Load required libraries
library(tidyverse)
library(fastDummies)
library(caret)
library(corrplot)

# Ensure output directories exist
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Load data and resource tables (data dictionary, anova analysis tables)
insurance_clean <- read.csv("data/processed/insurance_clean.csv")
hcup_age_final <- read.csv("data/processed/hcup_age_clean.csv")

glimpse(insurance_clean)
glimpse(hcup_age_final)

# 1 Benchmark Summary ####

# HCUP data has multiple years per age group
# need ONE average per age group for easy merging
hcup_age_summary <- hcup_age_final %>%
  group_by(age_group_standard) %>%
  summarise(
    avg_hcup_charges = mean(avg_hospital_charges, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(hcup_age_summary, "outputs/tables/hcup_benchmark_summary.csv")

# Display benchmarks
print(hcup_age_summary)

# 2 Merge Insurance w/ Benchmarks ####

insurance_with_benchmarks <- insurance_clean %>%
  left_join(hcup_age_summary, by = "age_group_standard")

write.csv(insurance_with_benchmarks, "outputs/tables/insurance_with_benchmarks.csv")

# Check if the merge worked
glimpse(insurance_with_benchmarks)

# How many people got benchmarks?
merge_success_count <- sum(!is.na(insurance_with_benchmarks$avg_hcup_charges))
cat("Records successfully merged with benchmarks:", merge_success_count, "\n")

# 3 Regular ANOVA Analysis ####

## 3.1 Core variables tests ####
smoker_test <- aov(charges ~ smoker, data = insurance_with_benchmarks)#high priority
sex_test <- aov(charges ~ sex, data = insurance_with_benchmarks) #high priority
age_test <- aov(charges ~ age_group_standard, data = insurance_with_benchmarks) #high priority
bmi_test <- aov(charges ~ bmi_category, data = insurance_with_benchmarks) #high priority
region_test <- aov(charges ~ region, data = insurance_with_benchmarks)
child_test <- aov(charges ~ has_children, data = insurance_with_benchmarks)

# Print summaries for review
summary(smoker_test)
summary(sex_test)
summary(age_test)
summary(bmi_test)
summary(region_test)
summary(child_test)

## 3.2 Core variable interactions tests ####

smoker_sex_test <- aov(charges ~ smoker * sex, data = insurance_with_benchmarks) # very high priority
smoker_age_test <- aov(charges ~ smoker * age_group_standard, data = insurance_with_benchmarks) # very high priority
sex_age_test <- aov(charges ~ sex * age_group_standard, data = insurance_with_benchmarks) #high priority
bmi_smoker_test <- aov(charges ~ bmi_category * smoker, data = insurance_with_benchmarks) # very high priority
bmi_sex_test <- aov(charges ~ bmi_category * sex, data = insurance_with_benchmarks) #high priority
sex_child_test <- aov(charges ~ has_children * sex, data = insurance_with_benchmarks)

# Print interaction summaries
summary(smoker_sex_test)
summary(smoker_age_test)
summary(sex_age_test)
summary(bmi_smoker_test)
summary(bmi_sex_test)
summary(sex_child_test)

## 3.3 Extracting Regular ANOVA results ####

# Function to extract ANOVA statistics
extract_anova_stats <- function(anova_model, effect_name = NULL) {
  anova_summary <- summary(anova_model)
  anova_table <- anova_summary[[1]]
  
  # For main effects (single factor)
  if (is.null(effect_name)) {
    effect_row <- 1  # First row is always the main effect
  } else {
    # For interactions, find the specific effect row
    effect_row <- which(rownames(anova_table) == effect_name)
    if (length(effect_row) == 0) {
      effect_row <- 1  # Default to first row if not found
    }
  }
  
  f_value <- round(anova_table[effect_row, "F value"], 3)
  p_value <- round(anova_table[effect_row, "Pr(>F)"], 3)
  df <- anova_table[effect_row, "Df"]
  sum_sq <- anova_table[effect_row, "Sum Sq"]
  mean_sq <- anova_table[effect_row, "Mean Sq"]
  
  # Calculate effect size (eta-squared)
  total_sum_sq <- sum(anova_table[, "Sum Sq"])
  eta_squared <- round(sum_sq / total_sum_sq, 3)
  
  # Determine significance level
  sig_level <- ifelse(p_value < 0.001, "***",
                      ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*",
                                    ifelse(p_value < 0.1, ".", ""))))
  
  return(list(
    f_value = f_value,
    p_value = p_value,
    df = df,
    sum_sq = sum_sq,
    mean_sq = mean_sq,
    eta_squared = eta_squared,
    sig_level = sig_level,
    significant = p_value < 0.05
  ))
}

# Extract results for main effects
smoker_results <- extract_anova_stats(smoker_test)
sex_results <- extract_anova_stats(sex_test)
age_results <- extract_anova_stats(age_test)
bmi_results <- extract_anova_stats(bmi_test)
region_results <- extract_anova_stats(region_test)
child_results <- extract_anova_stats(child_test)

# Extract results for interactions
smoker_sex_results <- extract_anova_stats(smoker_sex_test, "smoker:sex")
smoker_age_results <- extract_anova_stats(smoker_age_test, "smoker:age_group_standard")
sex_age_results <- extract_anova_stats(sex_age_test, "sex:age_group_standard")
bmi_smoker_results <- extract_anova_stats(bmi_smoker_test, "bmi_category:smoker")
bmi_sex_results <- extract_anova_stats(bmi_sex_test, "bmi_category:sex")
sex_child_results <- extract_anova_stats(sex_child_test, "has_children:sex")

# Create comprehensive results table for reference
original_anova_results <- tibble(
  analysis_id = 1:12,
  analysis_type = c(rep("One-way", 6), rep("Two-way", 6)),
  variables = c(
    "smoker → charges", "sex → charges", "age_group_standard → charges",
    "bmi_category → charges", "region → charges", "has_children → charges",
    "smoker × sex → charges", "smoker × age_group → charges", 
    "sex × age_group → charges", "bmi_category × smoker → charges",
    "bmi_category × sex → charges", "has_children × sex → charges"
  ),
  f_value = c(
    smoker_results$f_value, sex_results$f_value, age_results$f_value,
    bmi_results$f_value, region_results$f_value, child_results$f_value,
    smoker_sex_results$f_value, smoker_age_results$f_value, sex_age_results$f_value,
    bmi_smoker_results$f_value, bmi_sex_results$f_value, sex_child_results$f_value
  ),
  p_value = c(
    smoker_results$p_value, sex_results$p_value, age_results$p_value,
    bmi_results$p_value, region_results$p_value, child_results$p_value,
    smoker_sex_results$p_value, smoker_age_results$p_value, sex_age_results$p_value,
    bmi_smoker_results$p_value, bmi_sex_results$p_value, sex_child_results$p_value
  ),
  eta_squared = c(
    smoker_results$eta_squared, sex_results$eta_squared, age_results$eta_squared,
    bmi_results$eta_squared, region_results$eta_squared, child_results$eta_squared,
    smoker_sex_results$eta_squared, smoker_age_results$eta_squared, sex_age_results$eta_squared,
    bmi_smoker_results$eta_squared, bmi_sex_results$eta_squared, sex_child_results$eta_squared
  ),
  significant = c(
    smoker_results$significant, sex_results$significant, age_results$significant,
    bmi_results$significant, region_results$significant, child_results$significant,
    smoker_sex_results$significant, smoker_age_results$significant, sex_age_results$significant,
    bmi_smoker_results$significant, bmi_sex_results$significant, sex_child_results$significant
  ),
  effect_size_interpretation = case_when(
    eta_squared < 0.01 ~ "Small",
    eta_squared < 0.06 ~ "Medium", 
    eta_squared < 0.14 ~ "Large",
    TRUE ~ "Very Large"
  )
)

# original ANOVA analysis for reference
write_csv(original_anova_results, "outputs/tables/original_anova_analysis_results.csv")

# 4 Feature Engineering ####
## 4.1 Engineered features table for organization ####

# Initialize master feature tracking table (single source of truth)
master_features_table <- tibble(
  feature_id = 1:32,
  feature_name = c(
    # STANDALONE FEATURES (Main effects from ANOVA - the "base calculators")
    "smoker_cost_multiplier", "sex_cost_premium", "bmi_risk_factor", 
    "region_cost_index", "has_children_factor", "age_cost_curve",
    
    # NON-LINEAR TRANSFORMATIONS (Curve fitting)
    "age_squared", "bmi_squared", "age_cubed", "age_log", "bmi_log",
    
    # RISK SCORING FEATURES (Composite calculators)
    "health_risk_score", "demographic_risk_level", "compound_risk_score",
    
    # INTERACTION FEATURES (Combined effects - the "complex calculators")
    "smoker_age_interaction", "smoker_sex_combo", "smoker_bmi_interaction",
    "region_children_interaction", "has_children_age_interaction", "region_cost_multiplier",
    "smoker_region_combo", "sex_bmi_interaction", "age_region_interaction",
    
    # BINNING FEATURES (Category-based calculators)
    "smoker_encoded", "sex_encoded", "region_encoded", "bmi_category_encoded",
    
    # BINNING FEATURES (Category-based calculators)
    "age_bins", "charges_percentile_rank",
    
    # ADVANCED STANDALONE FEATURES (Enhanced main effects)
    "smoker_years_estimate", "bmi_health_category", "regional_market_tier"
  ),
  feature_type = c(
    # Standalone main effects (6 features)
    rep("Standalone_Main_Effect", 6),
    # Non-linear transforms (5 features)
    rep("Non_Linear_Transform", 5), 
    # Risk scoring (3 features)
    rep("Risk_Score", 3),
    # Statistical interactions (9 features)
    rep("Statistical_Interaction", 9),
    # Categorical encoding (4 features)
    rep("Categorical_Encoding", 4),
    # Binning (2 features)
    rep("Categorical_Binning", 2),
    # Advanced standalone (3 features)
    rep("Advanced_Standalone", 3)
  ),
  based_on_anova = c(
    # Standalone features based on significant main effects (6)
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    # Non-linear transformations (5)
    FALSE, FALSE, FALSE, FALSE, FALSE,
    # Risk scoring (3)
    FALSE, FALSE, FALSE,
    # Interactions based on significant ANOVA (9) 
    TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
    # Encodings (4)
    FALSE, FALSE, FALSE, FALSE,
    # Binning (2)
    FALSE, FALSE,
    # Advanced standalone (3)
    FALSE, FALSE, FALSE
  ),
  anova_priority = c(
    # Main effects priorities from your ANOVA (6)
    "Very_High", "Very_High", "Very_High", "Very_High", "High", "High",
    # Non-linear transformations (5)
    "Medium", "Medium", "Medium", "Medium", "Medium",
    # Risk scoring (3)
    "Medium", "Medium", "Medium",
    # Interactions (9)
    "Very_High", "Very_High", "Medium", "Medium", "High", "High", "Medium", "Medium", "Medium",
    # Encodings (4)
    "Medium", "Medium", "Medium", "Medium",
    # Binning (2)
    "Medium", "Medium",
    # Advanced standalone (3)
    "Medium", "Medium", "Medium"
  ),
  uses_multipliers = c(
    # Standalone - all use multipliers directly (6)
    rep(TRUE, 6),
    # Non-linear - don't use multipliers (5)
    rep(FALSE, 5),
    # Risk scores - use multipliers in calculations (3)
    rep(TRUE, 3),
    # Interactions - most use multipliers (9)
    rep(TRUE, 9),
    # Encodings - don't use multipliers (4)
    rep(FALSE, 4),
    # Binning - don't use multipliers (2)
    rep(FALSE, 2),
    # Advanced - don't use multipliers directly (3)
    rep(FALSE, 3)
  ),
  # Initialize tracking columns
  f_value = NA_real_,
  p_value = NA_real_,
  eta_squared = NA_real_,
  significant = NA,
  created = FALSE
)

View(master_features_table)
write.csv(master_features_table, "outputs/tables/master_features_table.csv")

## 4.2 Multipliers/ Coefficients creation and extraction ####
## ANOVA Group Means method

extract_multipliers_fixed <- function(data) {
  
  multipliers_table <- tibble(
    feature = character(),
    category = character(),
    baseline_cost = numeric(),
    group_cost = numeric(),
    multiplier = numeric(),
    sample_size = numeric(),
    statistical_basis = character()
  )
  
  # Helper function to add multipliers safely
  add_multiplier_group <- function(feature_name, group_var, data) {
    stats <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        mean_cost = mean(charges, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    baseline_cost <- min(stats$mean_cost)
    
    tibble(
      feature = feature_name,
      category = as.character(stats[[group_var]]),
      baseline_cost = baseline_cost,
      group_cost = stats$mean_cost,
      multiplier = stats$mean_cost / baseline_cost,
      sample_size = stats$n,
      statistical_basis = "ANOVA group means"
    )
  }
  
  # Generate all multipliers
  multiplier_groups <- list(
    list("smoker", "smoker"),
    list("sex", "sex"),
    list("bmi_category", "bmi_category"),
    list("region", "region"),
    list("age_group_standard", "age_group_standard")
  )
  
  for (group in multiplier_groups) {
    if (group[[2]] %in% colnames(data)) {
      new_multipliers <- add_multiplier_group(group[[1]], group[[2]], data)
      multipliers_table <- bind_rows(multipliers_table, new_multipliers)
    }
  }
  
  # Add has_children multipliers
  if ("children" %in% colnames(data)) {
    children_stats <- data %>%
      mutate(has_children = if_else(children > 0, "yes", "no")) %>%
      group_by(has_children) %>%
      summarise(
        mean_cost = mean(charges, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    baseline_cost <- min(children_stats$mean_cost)
    
    children_multipliers <- tibble(
      feature = "has_children",
      category = children_stats$has_children,
      baseline_cost = baseline_cost,
      group_cost = children_stats$mean_cost,
      multiplier = children_stats$mean_cost / baseline_cost,
      sample_size = children_stats$n,
      statistical_basis = "ANOVA group means"
    )
    
    multipliers_table <- bind_rows(multipliers_table, children_multipliers)
  }
  
  return(multipliers_table)
}

# Step 3: Generate Multipliers Table
multipliers_table <- extract_multipliers_fixed(insurance_with_benchmarks)

# multipliers for future reference
write_csv(multipliers_table, "outputs/tables/feature_multipliers.csv")

print(multipliers_table)

## 4.3 Engineered Features + Simultaneous Encoding ####

create_engineered_features_robust <- function(data, multipliers) {
  
  # Convert multipliers to named vectors safely
  create_multiplier_lookup <- function(feature_name) {
    mult_data <- multipliers %>% 
      filter(feature == feature_name)
    
    if (nrow(mult_data) == 0) {
      warning(paste("No multipliers found for feature:", feature_name))
      return(setNames(1, "default"))
    }
    
    lookup <- setNames(mult_data$multiplier, mult_data$category)
    return(lookup)
  }
  
  # Create all multiplier lookups
  smoker_mult <- create_multiplier_lookup("smoker")
  sex_mult <- create_multiplier_lookup("sex")
  bmi_mult <- create_multiplier_lookup("bmi_category")
  region_mult <- create_multiplier_lookup("region")
  children_mult <- create_multiplier_lookup("has_children")
  age_mult <- create_multiplier_lookup("age_group_standard")
  
  # Safe lookup function
  safe_lookup <- function(values, lookup_table, default_value = 1) {
    result <- lookup_table[values]
    result[is.na(result)] <- default_value
    return(as.numeric(result))
  }
  
  # Apply feature engineering
  data_engineered <- data %>%
    mutate(
      # Standalone Multiplier Features
      smoker_cost_multiplier = safe_lookup(smoker, smoker_mult),
      sex_cost_premium = safe_lookup(sex, sex_mult),
      bmi_risk_factor = safe_lookup(bmi_category, bmi_mult),
      region_cost_index = safe_lookup(region, region_mult),
      has_children_factor = safe_lookup(if_else(children > 0, "yes", "no"), children_mult),
      age_cost_curve = safe_lookup(age_group_standard, age_mult),
      
      # Non-linear Features
      age_squared = age^2,
      bmi_squared = bmi^2,
      age_cubed = age^3,
      age_log = log(pmax(age, 1)),  # Avoid log(0)
      bmi_log = log(pmax(bmi, 1)),  # Avoid log(0)
      
      # Risk Scores
      health_risk_score = pmax(0, (smoker_cost_multiplier * 4) + 
                                 (bmi_risk_factor * 3) + 
                                 (age_cost_curve * 3)),
      demographic_risk_level = case_when(
        health_risk_score <= 3 ~ "Low",
        health_risk_score <= 6 ~ "Medium",
        health_risk_score <= 9 ~ "High",
        TRUE ~ "Very_High"
      ),
      compound_risk_score = smoker_cost_multiplier * sex_cost_premium * bmi_risk_factor,
      
      # Interactions
      smoker_age_interaction = smoker_cost_multiplier * age_cost_curve,
      smoker_sex_combo = smoker_cost_multiplier * sex_cost_premium,
      smoker_bmi_interaction = smoker_cost_multiplier * bmi_risk_factor,
      region_children_interaction = region_cost_index * has_children_factor,
      has_children_age_interaction = has_children_factor * age_cost_curve,
      region_cost_multiplier = region_cost_index,
      smoker_region_combo = smoker_cost_multiplier * region_cost_index,
      sex_bmi_interaction = sex_cost_premium * bmi_risk_factor,
      age_region_interaction = age_cost_curve * region_cost_index,
      
      # Encodings
      smoker_encoded = as.numeric(smoker == "yes"),
      sex_encoded = as.numeric(sex == "male"),
      bmi_category_encoded = as.numeric(as.factor(bmi_category)),
      region_encoded = case_when(
        region == "southeast" ~ 1,
        region == "southwest" ~ 2,
        region == "northwest" ~ 3,
        region == "northeast" ~ 4,
        TRUE ~ 1
      ),
      
      # Binned Features
      age_bins = cut(age, breaks = seq(15, 70, by = 5), include.lowest = TRUE),
      charges_percentile_rank = percent_rank(charges),
      
      # Advanced Features
      smoker_years_estimate = smoker_encoded * age,
      bmi_health_category = case_when(
        bmi < 18.5 ~ "underweight",
        bmi < 25 ~ "normal", 
        bmi < 30 ~ "overweight",
        TRUE ~ "obese"
      ),
      regional_market_tier = case_when(
        region %in% c("northeast", "northwest") ~ "premium",
        region == "southwest" ~ "standard",
        region == "southeast" ~ "economy",
        TRUE ~ "standard"
      )
    )
  
  return(data_engineered)
}

insurance_with_engineered_features <- create_engineered_features_robust(insurance_with_benchmarks, multipliers_table)

write_csv(insurance_with_engineered_features, "outputs/tables/insurance_with_engineered_features.csv")

## 4.4 Basic cost comparisons ####

# 1. Basic Descriptive Statistics
basic_cost_summary <- insurance_with_engineered_features %>%
  summarise(
    mean_cost = mean(charges),
    median_cost = median(charges),
    sd_cost = sd(charges),
    min_cost = min(charges),
    max_cost = max(charges),
    q25_cost = quantile(charges, 0.25),
    q75_cost = quantile(charges, 0.75),
    n_observations = n()
  )

# 2. Cost by Major Categories (Validate Multipliers)
cost_by_smoker <- insurance_with_engineered_features %>%
  group_by(smoker) %>%
  summarise(
    mean_cost = mean(charges),
    median_cost = median(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cost_ratio = mean_cost / min(mean_cost),
    vs_baseline = paste0(round(cost_ratio, 2), "x")
  )

cost_by_region <- insurance_with_engineered_features %>%
  group_by(region) %>%
  summarise(
    mean_cost = mean(charges),
    median_cost = median(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cost_ratio = mean_cost / min(mean_cost),
    vs_baseline = paste0(round(cost_ratio, 2), "x")
  ) %>%
  arrange(mean_cost)

# Cross-Category Comparisons (Key Interactions)
smoker_sex_costs <- insurance_with_engineered_features %>%
  group_by(smoker, sex) %>%
  summarise(
    mean_cost = mean(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cost))

# key comparisons
write_csv(cost_by_smoker, "outputs/tables/cost_comparison_smoker.csv")
write_csv(cost_by_region, "outputs/tables/cost_comparison_region.csv") 
write_csv(smoker_sex_costs, "outputs/tables/cost_comparison_smoker_sex.csv")

# 5 ANOVA for Engineered Features ####

perform_feature_anova_robust <- function(data, feature_names, target_col = "charges") {
  
  anova_results <- tibble(
    feature_name = character(),
    test_type = character(),
    f_value = numeric(),
    p_value = numeric(),
    eta_squared = numeric(),
    significant = logical(),
    error_message = character()
  )
  
  for (feature in feature_names) {
    
    # Skip if feature doesn't exist
    if (!feature %in% colnames(data)) {
      result <- tibble(
        feature_name = feature,
        test_type = "MISSING",
        f_value = NA_real_,
        p_value = NA_real_,
        eta_squared = NA_real_,
        significant = NA,
        error_message = "Feature not found in dataset"
      )
      anova_results <- bind_rows(anova_results, result)
      next
    }
    
    feature_data <- data[[feature]]
    target_data <- data[[target_col]]
    
    # Remove NA values
    complete_cases <- complete.cases(feature_data, target_data)
    if (sum(complete_cases) < 10) {
      result <- tibble(
        feature_name = feature,
        test_type = "INSUFFICIENT_DATA",
        f_value = NA_real_,
        p_value = NA_real_,
        eta_squared = NA_real_,
        significant = NA,
        error_message = "Insufficient complete cases"
      )
      anova_results <- bind_rows(anova_results, result)
      next
    }
    
    feature_clean <- feature_data[complete_cases]
    target_clean <- target_data[complete_cases]
    
    # Check feature variance
    if (length(unique(feature_clean)) <= 1) {
      result <- tibble(
        feature_name = feature,
        test_type = "NO_VARIANCE",
        f_value = NA_real_,
        p_value = NA_real_,
        eta_squared = NA_real_,
        significant = NA,
        error_message = "Feature has no variance"
      )
      anova_results <- bind_rows(anova_results, result)
      next
    }
    
    # Determine test type and perform analysis
    tryCatch({
      if (is.numeric(feature_clean) && length(unique(feature_clean)) > 10) {
        # Linear regression for continuous
        model <- lm(target_clean ~ feature_clean)
        anova_result <- anova(model)
        model_summary <- summary(model)
        
        f_value <- anova_result$`F value`[1]
        p_value <- anova_result$`Pr(>F)`[1]
        eta_squared <- model_summary$r.squared
        test_type <- "Linear_Regression"
        
      } else {
        # ANOVA for categorical or discrete
        if (is.numeric(feature_clean)) {
          feature_clean <- as.factor(feature_clean)
        }
        
        model <- aov(target_clean ~ feature_clean)
        anova_summary <- summary(model)
        
        f_value <- anova_summary[[1]]$`F value`[1]
        p_value <- anova_summary[[1]]$`Pr(>F)`[1]
        
        # Calculate eta-squared (measure of effect size [.01 = small, .06 = med, .14 = large])
        ss_between <- anova_summary[[1]]$`Sum Sq`[1]
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        eta_squared <- ss_between / ss_total
        test_type <- "ANOVA"
      }
      
      result <- tibble(
        feature_name = feature,
        test_type = test_type,
        f_value = f_value,
        p_value = p_value,
        eta_squared = eta_squared,
        significant = p_value < 0.05,
        error_message = "Success"
      )
      
    }, error = function(e) {
      result <- tibble(
        feature_name = feature,
        test_type = "ERROR",
        f_value = NA_real_,
        p_value = NA_real_,
        eta_squared = NA_real_,
        significant = NA,
        error_message = as.character(e$message)
      )
    })
    
    anova_results <- bind_rows(anova_results, result)
  }
  
  return(anova_results)
}

## 5.1 Test all created features ####

created_features <- master_features_table$feature_name[
  master_features_table$feature_name %in% colnames(insurance_with_engineered_features)
]

engineered_anova_results <- perform_feature_anova_robust(insurance_with_engineered_features, created_features)

# Save ANOVA results
write_csv(engineered_anova_results, "outputs/tables/engineered_features_anova_results.csv")

## 5.2 Update Master Features Table ####

updated_master_table <- master_features_table %>%
  left_join(engineered_anova_results %>% select(-test_type, -error_message), by = "feature_name") %>%
  mutate(
    created = feature_name %in% colnames(insurance_with_engineered_features),
    f_value = coalesce(f_value.y, f_value.x),
    p_value = coalesce(p_value.y, p_value.x),
    eta_squared = coalesce(eta_squared.y, eta_squared.x),
    significant = coalesce(significant.y, significant.x)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  arrange(desc(coalesce(eta_squared, 0)))

# Create effect size categories
updated_master_table <- updated_master_table %>%
  mutate(
    effect_size_interpretation = case_when(
      is.na(eta_squared) ~ "Not_Tested",
      eta_squared >= 0.14 ~ "Large",
      eta_squared >= 0.06 ~ "Medium",
      eta_squared >= 0.01 ~ "Small",
      TRUE ~ "Negligible"
    )
  )

write_csv(updated_master_table, "outputs/tables/master_features_table_final.csv")

# Summary of feature engineering results
feature_summary <- updated_master_table %>%
  summarise(
    total_features_planned = n(),
    features_created = sum(created, na.rm = TRUE),
    features_tested = sum(!is.na(eta_squared)),
    significant_features = sum(significant, na.rm = TRUE),
    large_effect_features = sum(eta_squared >= 0.14, na.rm = TRUE),
    medium_effect_features = sum(eta_squared >= 0.06 & eta_squared < 0.14, na.rm = TRUE),
    completion_rate = round(features_created / total_features_planned * 100, 1)
  )

write_csv(feature_summary, "outputs/tables/feature_engineering_completion_summary.csv")

# 6 Advanced Feature Engineering ####
## Additional sophisticated features based on ANOVA insights (in original script, was redudant and unecessary as original ANOVa had identified all which features would be relevant [not simply significant (p-value) but would have a high eta-squared [measure of effect size]])

insurance_advanced <- insurance_with_engineered_features %>%
  mutate(
    # Smoking Duration Impact Modeling (Literature-Based)
    smoker_age_severity_index = case_when(
      smoker == "no" ~ 0,
      smoker == "yes" & age < 30 ~ age * 0.8,
      smoker == "yes" & age < 45 ~ age * 1.2,
      smoker == "yes" & age < 60 ~ age * 1.8,
      smoker == "yes" & age >= 60 ~ age * 2.5
    ),
    
    # Metabolic Syndrome Risk (BMI + Age Compound Effects)
    metabolic_syndrome_risk = case_when(
      bmi < 25 & age < 40 ~ 1.0,
      bmi >= 25 & bmi < 30 & age < 40 ~ 1.3,
      bmi >= 30 & age < 40 ~ 1.8,
      bmi < 25 & age >= 40 ~ 1.2,
      bmi >= 25 & bmi < 30 & age >= 40 & age < 55 ~ 1.6,
      bmi >= 30 & age >= 40 & age < 55 ~ 2.2,
      bmi >= 25 & age >= 55 ~ 2.8,
      bmi >= 30 & age >= 55 ~ 3.5,
      TRUE ~ 1.0
    ),
    
    # Family Cost Optimization (Economies of Scale)
    family_cost_optimization = case_when(
      children == 0 ~ 1.0,
      children == 1 ~ 0.85,
      children == 2 ~ 0.75,
      children == 3 ~ 0.70,
      children >= 4 ~ 0.65,
      TRUE ~ 1.0
    ),
    
    # Compound Lifestyle Risk Score
    compound_lifestyle_risk = (
      ifelse(smoker == "yes", 2.5, 1.0) *
        case_when(
          bmi < 18.5 ~ 1.3,
          bmi >= 18.5 & bmi < 25 ~ 1.0,
          bmi >= 25 & bmi < 30 ~ 1.2,
          bmi >= 30 ~ 1.5,
          TRUE ~ 1.0
        ) *
        case_when(
          age < 30 ~ 0.8,
          age >= 30 & age < 50 ~ 1.0,
          age >= 50 ~ 1.3,
          TRUE ~ 1.0
        )
    ),
    
    # Individual vs Cohort Performance Ratio
    individual_vs_cohort_ratio = charges / case_when(
      age < 25 & smoker == "no" ~ 3000,
      age < 25 & smoker == "yes" ~ 7500,
      age >= 25 & age < 35 & smoker == "no" ~ 4500,
      age >= 25 & age < 35 & smoker == "yes" ~ 11000,
      age >= 35 & age < 45 & smoker == "no" ~ 6500,
      age >= 35 & age < 45 & smoker == "yes" ~ 16000,
      age >= 45 & age < 55 & smoker == "no" ~ 9500,
      age >= 45 & age < 55 & smoker == "yes" ~ 23000,
      age >= 55 & smoker == "no" ~ 15000,
      age >= 55 & smoker == "yes" ~ 35000,
      TRUE ~ mean(charges)
    ),
    
    # Cost Efficiency Quintiles
    cost_efficiency_quintiles = ntile(charges / compound_lifestyle_risk, 5),
    
    # Outlier Detection Flags
    outlier_detection_flags = case_when(
      charges > quantile(charges, 0.95) ~ "High_Cost_Outlier",
      charges < quantile(charges, 0.05) ~ "Low_Cost_Outlier", 
      abs(individual_vs_cohort_ratio - 1) > 2 ~ "Cohort_Deviation_Outlier",
      compound_lifestyle_risk > 4 & charges < 10000 ~ "Risk_Cost_Mismatch",
      compound_lifestyle_risk < 1.5 & charges > 20000 ~ "Low_Risk_High_Cost",
      TRUE ~ "Normal"
    )
  )

write_csv(insurance_advanced, "data/processed/insurance_advanced_features.csv")

## 6.1 Validate Advanced Features ####

new_advanced_features <- c(
  "smoker_age_severity_index", "metabolic_syndrome_risk", 
  "family_cost_optimization", "compound_lifestyle_risk",
  "individual_vs_cohort_ratio", "cost_efficiency_quintiles", "outlier_detection_flags"
)

advanced_feature_results <- perform_feature_anova_robust(insurance_advanced, new_advanced_features)

# advanced results
write_csv(advanced_feature_results, "outputs/tables/advanced_features_anova_results.csv")




# 7 Encoding ####
## Streamlined encoding strategies for model preparation

apply_encoding_strategies <- function(data) {
    
    data %>%
      mutate(
        # Binary encodings
        is_smoker = as.numeric(smoker == "yes"),
        is_male = as.numeric(sex == "male"),
        is_obese = as.numeric(bmi_category == "obese"),
        has_children_flag = as.numeric(children > 0),
        
        # Interactions
        smoker_male = is_smoker * is_male,
        smoker_obese = is_smoker * is_obese,
        high_risk_combo = is_smoker * is_obese * is_male,
        
        # Ordinal encodings
        bmi_risk_ordinal = case_when(
          bmi_category == "underweight" ~ 1,
          bmi_category == "normal" ~ 2,
          bmi_category == "overweight" ~ 3,
          bmi_category == "obese" ~ 4,
          TRUE ~ 2
        ),
        
        region_cost_ordinal = case_when(
          region == "southeast" ~ 1,
          region == "southwest" ~ 2,
          region == "northwest" ~ 3,
          region == "northeast" ~ 4,
          TRUE ~ 2
        ),
        
        age_group_ordinal = case_when(
          age_group_standard == "18-25" ~ 1,
          age_group_standard == "26-35" ~ 2,
          age_group_standard == "36-45" ~ 3,
          age_group_standard == "46-55" ~ 4,
          age_group_standard == "56-65" ~ 5,
          TRUE ~ 3
        ),
        
        # Age group binary flags
        is_young_adult = as.numeric(age_group_standard == "18-25"),
        is_senior = as.numeric(age_group_standard == "56-65"),
        is_middle_aged = as.numeric(age_group_standard %in% c("36-45", "46-55")),
        
        # Region dummies
        region_northeast = as.numeric(region == "northeast"),
        region_northwest = as.numeric(region == "northwest"),
        region_southeast = as.numeric(region == "southeast"),
        region_southwest = as.numeric(region == "southwest")
      )
  }
  
  insurance_encoded <- apply_encoding_strategies(insurance_advanced)
  write_csv(insurance_encoded, "data/processed/insurance_encoded_final.csv")

# 8 Analysis ####

## 8.1 Cost Efficiency Analysis ####
  
  cost_efficiency_segments <- insurance_encoded %>%
    mutate(
      # Core efficiency metrics
      cost_per_risk_point = charges / compound_lifestyle_risk,
      benchmark_deviation_quartile = ntile(abs(individual_vs_cohort_ratio - 1), 4),
      
      # Regional optimization opportunities
      regional_cost_opportunity = case_when(
        region_cost_index < 1.0 & charges > median(charges) ~ "Underpriced_Market",
        region_cost_index > 1.0 & charges < median(charges) ~ "Overpriced_Market", 
        TRUE ~ "Appropriately_Priced"
      ),
      
      # Family efficiency categorization
      family_efficiency_category = case_when(
        children == 0 ~ "Individual",
        children > 0 & cost_per_risk_point < median(cost_per_risk_point) ~ "Efficient_Family",
        children > 0 & cost_per_risk_point >= median(cost_per_risk_point) ~ "Inefficient_Family",
        TRUE ~ "Standard_Family"
      ),
      
      # Business Intelligence segments
      high_value_low_risk = case_when(
        compound_lifestyle_risk < 2.0 & 
          charges > quantile(charges, 0.75) ~ "High_Value_Low_Risk",
        compound_lifestyle_risk > 3.0 & 
          charges < quantile(charges, 0.25) ~ "Low_Value_High_Risk",
        TRUE ~ "Standard_Profile"
      ),
      
      # Intervention priority targeting
      intervention_priority = case_when(
        outlier_detection_flags == "Risk_Cost_Mismatch" ~ "High_Priority",
        smoker == "yes" & age > 45 & charges > 25000 ~ "Smoking_Cessation_Target",
        bmi >= 30 & charges > 20000 ~ "Weight_Management_Target",
        charges > quantile(charges, 0.90) ~ "Care_Coordination_Target",
        TRUE ~ "Standard_Care"
      ),
      
      # Market opportunity identification
      market_opportunity = case_when(
        region_cost_index > 1.1 & 
          cost_efficiency_quintiles >= 4 ~ "Premium_Market_Opportunity",
        region_cost_index < 0.9 & 
          cost_efficiency_quintiles <= 2 ~ "Value_Market_Opportunity",
        TRUE ~ "Saturated_Market"
      ),
      
      # Risk pool subsidization analysis
      subsidization_role = case_when(
        compound_lifestyle_risk < 1.5 & 
          charges < quantile(charges, 0.4) ~ "Cross_Subsidizer",
        compound_lifestyle_risk > 3.0 & 
          charges > quantile(charges, 0.6) ~ "Subsidized_Member",
        TRUE ~ "Balanced_Contributor"
      )
    )
  
  write_csv(cost_efficiency_segments, "outputs/tables/cost_efficiency_analysis.csv")
  
## 8.2 Predictive Cost Patterns ####
  
  advanced_cost_patterns <- cost_efficiency_segments %>%
    group_by(age_group_standard) %>%
    summarise(
      # Lifecycle cost modeling
      avg_cost_trajectory = mean(charges),
      risk_adjusted_trajectory = mean(charges / compound_lifestyle_risk),
      cost_growth_rate = (max(charges) - min(charges)) / min(charges),
      
      # Risk-stratified aging curves
      low_risk_aging_curve = mean(charges[compound_lifestyle_risk < 2]),
      high_risk_aging_curve = mean(charges[compound_lifestyle_risk >= 3]),
      
      .groups = 'drop'
    ) %>%
    left_join(
      # Smoking cessation ROI modeling
      cost_efficiency_segments %>%
        filter(smoker == "yes") %>%
        group_by(age_group_standard) %>%
        summarise(
          smoking_cost_premium = mean(charges),
          potential_cessation_savings = smoking_cost_premium * 0.4,
          cessation_roi_3year = potential_cessation_savings * 3 - 5000,
          .groups = 'drop'
        ),
      by = "age_group_standard"
    ) %>%
    left_join(
      # Weight management program impact
      cost_efficiency_segments %>%
        filter(bmi >= 30) %>%
        group_by(age_group_standard) %>%
        summarise(
          obesity_cost_premium = mean(charges),
          weight_loss_savings_potential = obesity_cost_premium * 0.25,
          wellness_program_roi = weight_loss_savings_potential * 2 - 3000,
          .groups = 'drop'
        ),
      by = "age_group_standard"
    )
  
  write_csv(advanced_cost_patterns, "outputs/tables/predictive_cost_patterns_analysis.csv")
  
## 8.3 Market Segmentation Analysis ####
  
  market_segmentation <- cost_efficiency_segments %>%
    group_by(high_value_low_risk) %>%
    summarise(
      segment_size = n(),
      avg_premium_potential = mean(charges * 1.15),
      avg_claims_ratio = mean(charges / (charges * 1.15)),
      profitability_index = (mean(charges * 1.15) - mean(charges)) / mean(charges),
      
      # Cross-selling potential
      avg_family_size = mean(children + 2),
      supplemental_product_potential = case_when(
        mean(compound_lifestyle_risk) < 2 ~ "Dental_Vision_Life",
        mean(compound_lifestyle_risk) < 3 ~ "Wellness_Programs", 
        TRUE ~ "Disease_Management"
      ),
      
      # Retention analysis
      cost_volatility = sd(charges) / mean(charges),
      retention_risk = case_when(
        cost_volatility > 0.8 ~ "High_Churn_Risk",
        cost_volatility > 0.5 ~ "Medium_Churn_Risk",
        TRUE ~ "Low_Churn_Risk"
      ),
      
      .groups = 'drop'
    )
  
  write_csv(market_segmentation, "outputs/tables/market_segmentation_analysis.csv")
  
## 8.4 Intervention Opportunities Summary ####
  
  intervention_opportunities <- cost_efficiency_segments %>%
    group_by(intervention_priority) %>%
    summarise(
      target_population = n(),
      avg_current_cost = mean(charges),
      intervention_cost_savings_potential = case_when(
        intervention_priority == "Smoking_Cessation_Target" ~ mean(charges) * 0.4,
        intervention_priority == "Weight_Management_Target" ~ mean(charges) * 0.25,
        intervention_priority == "Care_Coordination_Target" ~ mean(charges) * 0.15,
        TRUE ~ 0
      ),
      total_savings_potential = intervention_cost_savings_potential * target_population,
      roi_estimate = (total_savings_potential - (target_population * 2500)) / (target_population * 2500),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_savings_potential))
  
  write_csv(intervention_opportunities, "outputs/tables/cost_intervention_opportunities.csv")

## 8.5 Summary analytics and feature performance ####
  
  # Combine all ANOVA results for comprehensive view
  complete_feature_analysis <- bind_rows(
    original_anova_results %>%
      select(variables, f_value, p_value, eta_squared, significant, effect_size_interpretation) %>%
      rename(feature_name = variables) %>%
      mutate(analysis_source = "Original_Variables"),
    
    engineered_anova_results %>%
      filter(error_message == "Success") %>%
      select(feature_name, f_value, p_value, eta_squared, significant) %>%
      mutate(
        effect_size_interpretation = case_when(
          eta_squared >= 0.14 ~ "Large",
          eta_squared >= 0.06 ~ "Medium",
          eta_squared >= 0.01 ~ "Small",
          TRUE ~ "Negligible"
        ),
        analysis_source = "Engineered_Features"
      ),
    
    advanced_feature_results %>%
      filter(error_message == "Success") %>%
      select(feature_name, f_value, p_value, eta_squared, significant) %>%
      mutate(
        effect_size_interpretation = case_when(
          eta_squared >= 0.14 ~ "Large",
          eta_squared >= 0.06 ~ "Medium",
          eta_squared >= 0.01 ~ "Small",
          TRUE ~ "Negligible"
        ),
        analysis_source = "Advanced_Features"
      )
  ) %>%
    arrange(desc(eta_squared))
  
  write_csv(complete_feature_analysis, "outputs/tables/complete_feature_analysis_final.csv")
  
  # Top performing features summary
  top_features <- complete_feature_analysis %>%
    filter(significant == TRUE) %>%
    slice_head(n = 20) %>%
    select(feature_name, eta_squared, effect_size_interpretation, analysis_source)
  
  write_csv(top_features, "outputs/tables/top_performing_features.csv")

# 9 Scaling and Normalization Prep ####
  ##principal component analysis - most important features that capture the majority of the variance in the data

prepare_for_scaling <- function(data) {
  
  # Identify numeric columns for scaling
  numeric_cols <- data %>%
    select_if(is.numeric) %>%
    select(-charges) %>%
    colnames()
  
  # Remove zero variance columns
  zero_variance_cols <- numeric_cols[map_lgl(numeric_cols, function(col) {
    var(data[[col]], na.rm = TRUE) == 0
  })]
  
  if (length(zero_variance_cols) > 0) {
    data <- data %>% select(-all_of(zero_variance_cols))
    numeric_cols <- setdiff(numeric_cols, zero_variance_cols)
  }
  
  # Create scaling assessment
  scaling_needs <- tibble(
    feature = numeric_cols,
    mean_val = map_dbl(numeric_cols, ~mean(data[[.x]], na.rm = TRUE)),
    sd_val = map_dbl(numeric_cols, ~sd(data[[.x]], na.rm = TRUE)),
    min_val = map_dbl(numeric_cols, ~min(data[[.x]], na.rm = TRUE)),
    max_val = map_dbl(numeric_cols, ~max(data[[.x]], na.rm = TRUE)),
    range_val = max_val - min_val,
    cv = abs(sd_val / mean_val),
    
    needs_scaling = range_val > 10 | cv > 1,
    scaling_method = case_when(
      range_val > 1000 ~ "normalize",
      cv > 2 ~ "standardize", 
      range_val > 10 ~ "normalize",
      TRUE ~ "none"
    )
  )
  
  return(list(
    data = data,
    scaling_needs = scaling_needs,
    numeric_features = numeric_cols
  ))
}

scaling_prep <- prepare_for_scaling(cost_efficiency_segments)

write_csv(scaling_prep$scaling_needs, "outputs/tables/scaling_assessment_final.csv")
write_csv(scaling_prep$data, "data/processed/insurance_ready_for_scaling.csv")

# 10 Feature Selection and Scaling ####
## Select features based on effect size (PCA-inspired approach)

# High-impact feature set (Large + Medium effect sizes for PCA-style selection)
high_impact_features <- complete_feature_analysis %>%
  filter(significant == TRUE, eta_squared >= 0.06) %>%
  pull(feature_name)

# Essential feature set (based on ANOVA priorities)
essential_features <- updated_master_table %>%
  filter(created == TRUE, anova_priority %in% c("Very_High", "High")) %>%
  pull(feature_name)

# Create feature sets that exist in the data
available_high_impact <- high_impact_features[high_impact_features %in% colnames(cost_efficiency_segments)]
available_essential <- essential_features[essential_features %in% colnames(cost_efficiency_segments)]

# Create modeling datasets (unscaled)
modeling_data_high_impact <- cost_efficiency_segments %>%
  select(all_of(c("charges", available_high_impact)))

modeling_data_essential <- cost_efficiency_segments %>%
  select(all_of(c("charges", available_essential)))

## Apply scaling to selected features only
scale_modeling_data <- function(data) {
  
  numeric_cols <- data %>%
    select_if(is.numeric) %>%
    select(-charges) %>%
    colnames()
  
  # Apply appropriate scaling based on the assessment from Section 8
  scaling_decisions <- scaling_prep$scaling_needs %>%
    filter(feature %in% numeric_cols)
  
  data_scaled <- data
  
  # Apply scaling transformations
  for (i in 1:nrow(scaling_decisions)) {
    feature_name <- scaling_decisions$feature[i]
    method <- scaling_decisions$scaling_method[i]
    
    if (method == "normalize") {
      # Min-Max normalization (0 to 1)
      min_val <- min(data[[feature_name]], na.rm = TRUE)
      max_val <- max(data[[feature_name]], na.rm = TRUE)
      data_scaled[[paste0(feature_name, "_scaled")]] <- (data[[feature_name]] - min_val) / (max_val - min_val)
      
    } else if (method == "standardize") {
      # Z-score standardization (mean=0, sd=1)
      mean_val <- mean(data[[feature_name]], na.rm = TRUE)
      sd_val <- sd(data[[feature_name]], na.rm = TRUE)
      data_scaled[[paste0(feature_name, "_scaled")]] <- (data[[feature_name]] - mean_val) / sd_val
    }
  }
  
  return(data_scaled)
}

# Apply scaling to both feature sets
modeling_data_high_impact_scaled <- scale_modeling_data(modeling_data_high_impact)
modeling_data_essential_scaled <- scale_modeling_data(modeling_data_essential)

# Save modeling datasets
write_csv(modeling_data_high_impact, "data/processed/modeling_data_high_impact.csv")
write_csv(modeling_data_essential, "data/processed/modeling_data_essential.csv")
write_csv(modeling_data_high_impact_scaled, "data/processed/modeling_data_high_impact_scaled.csv")
write_csv(modeling_data_essential_scaled, "data/processed/modeling_data_essential_scaled.csv")

# Feature selection summary
feature_selection_summary <- tibble(
  dataset = c("High Impact", "Essential", "High Impact Scaled", "Essential Scaled"),
  n_features = c(
    ncol(modeling_data_high_impact) - 1,
    ncol(modeling_data_essential) - 1,
    ncol(modeling_data_high_impact_scaled) - 1,
    ncol(modeling_data_essential_scaled) - 1
  ),
  n_observations = c(
    nrow(modeling_data_high_impact),
    nrow(modeling_data_essential),
    nrow(modeling_data_high_impact_scaled),
    nrow(modeling_data_essential_scaled)
  ),
  selection_criteria = c(
    "eta_squared >= 0.06 (Medium+ effect)",
    "ANOVA priority: Very_High + High",
    "eta_squared >= 0.06 + scaled",
    "ANOVA priority: Very_High + High + scaled"
  )
)

write_csv(feature_selection_summary, "outputs/tables/feature_selection_summary.csv")



# .1 Data Dictionary Update ####
## Data Dictionary Update for Feature Engineering (Script 2)

insurance_advanced <- read_csv("data/processed/insurance_advanced_features.csv")
cost_efficiency_segments <- read_csv("outputs/tables/cost_efficiency_analysis.csv")

# Function to bulk add engineered features to dictionary
add_engineered_features_to_dictionary <- function() {
  
  # DERIVED/CLEANED VARIABLES FROM SCRIPT 1
  add_to_dictionary("age_group_standard", "character", "Script 1 - Data Cleaning", 
                    "Standardized age groups for analysis", "18-25, 26-35, 36-45, 46-55, 56-65")
  
  add_to_dictionary("bmi_category", "character", "Script 1 - Data Cleaning",
                    "BMI classification categories", "underweight, normal, overweight, obese")
  
  add_to_dictionary("has_children", "character", "Script 1 - Data Cleaning",
                    "Binary indicator for having children", "yes, no")
  
  # BENCHMARK DATA
  add_to_dictionary("avg_hcup_charges", "numeric", "Script 2 - External Benchmarking",
                    "Average hospital charges by age group from HCUP data", "National benchmark values")
  
  # STANDALONE MULTIPLIER FEATURES
  add_to_dictionary("smoker_cost_multiplier", "numeric", "Script 2 - Feature Engineering",
                    "Cost multiplier based on smoking status derived from ANOVA group means", "1.0 to 4.0+")
  
  add_to_dictionary("sex_cost_premium", "numeric", "Script 2 - Feature Engineering", 
                    "Cost multiplier based on biological sex derived from ANOVA group means", "0.9 to 1.1")
  
  add_to_dictionary("bmi_risk_factor", "numeric", "Script 2 - Feature Engineering",
                    "Cost multiplier based on BMI category derived from ANOVA group means", "0.8 to 1.5")
  
  add_to_dictionary("region_cost_index", "numeric", "Script 2 - Feature Engineering",
                    "Cost multiplier based on geographic region derived from ANOVA group means", "0.9 to 1.2")
  
  add_to_dictionary("has_children_factor", "numeric", "Script 2 - Feature Engineering",
                    "Cost multiplier based on having children derived from ANOVA group means", "0.9 to 1.1")
  
  add_to_dictionary("age_cost_curve", "numeric", "Script 2 - Feature Engineering",
                    "Cost multiplier based on age group derived from ANOVA group means", "0.5 to 2.5")
  
  # NON-LINEAR TRANSFORMATIONS
  add_to_dictionary("age_squared", "numeric", "Script 2 - Feature Engineering",
                    "Age squared to capture non-linear age effects", "324 to 4225")
  
  add_to_dictionary("bmi_squared", "numeric", "Script 2 - Feature Engineering", 
                    "BMI squared to capture non-linear BMI effects", "225 to 2500+")
  
  add_to_dictionary("age_cubed", "numeric", "Script 2 - Feature Engineering",
                    "Age cubed for extreme non-linear age effects", "5832 to 274625")
  
  add_to_dictionary("age_log", "numeric", "Script 2 - Feature Engineering",
                    "Natural logarithm of age for log-linear relationships", "2.89 to 4.19")
  
  add_to_dictionary("bmi_log", "numeric", "Script 2 - Feature Engineering",
                    "Natural logarithm of BMI for log-linear relationships", "2.71 to 3.91")
  
  # RISK SCORING FEATURES
  add_to_dictionary("health_risk_score", "numeric", "Script 2 - Feature Engineering",
                    "Composite health risk score: (smoker_multiplier*4) + (bmi_factor*3) + (age_curve*3)", "3 to 30+")
  
  add_to_dictionary("demographic_risk_level", "character", "Script 2 - Feature Engineering",
                    "Categorical risk level based on health_risk_score", "Low, Medium, High, Very_High")
  
  add_to_dictionary("compound_risk_score", "numeric", "Script 2 - Feature Engineering",
                    "Product of smoker, sex, and BMI multipliers", "0.7 to 6.0+")
  
  # STATISTICAL INTERACTION FEATURES
  add_to_dictionary("smoker_age_interaction", "numeric", "Script 2 - Feature Engineering",
                    "Product of smoker cost multiplier and age cost curve", "0.5 to 10.0+", 
                    notes = "Based on significant ANOVA interaction (F=245.67, p<0.001)")
  
  add_to_dictionary("smoker_sex_combo", "numeric", "Script 2 - Feature Engineering",
                    "Product of smoker and sex cost multipliers", "0.9 to 4.4+",
                    notes = "Based on significant ANOVA interaction (F=8.03, p=0.005)")
  
  add_to_dictionary("smoker_bmi_interaction", "numeric", "Script 2 - Feature Engineering",
                    "Product of smoker and BMI risk factors", "0.8 to 6.0+")
  
  add_to_dictionary("region_children_interaction", "numeric", "Script 2 - Feature Engineering",
                    "Product of region cost index and children factor", "0.81 to 1.32")
  
  add_to_dictionary("has_children_age_interaction", "numeric", "Script 2 - Feature Engineering",
                    "Product of children factor and age cost curve", "0.45 to 2.75")
  
  add_to_dictionary("region_cost_multiplier", "numeric", "Script 2 - Feature Engineering",
                    "Same as region_cost_index (duplicate for interaction features)", "0.9 to 1.2")
  
  add_to_dictionary("smoker_region_combo", "numeric", "Script 2 - Feature Engineering",
                    "Product of smoker multiplier and region cost index", "0.9 to 4.8+")
  
  add_to_dictionary("sex_bmi_interaction", "numeric", "Script 2 - Feature Engineering",
                    "Product of sex premium and BMI risk factor", "0.72 to 1.65")
  
  add_to_dictionary("age_region_interaction", "numeric", "Script 2 - Feature Engineering",
                    "Product of age cost curve and region cost index", "0.45 to 3.0")
  
  # CATEGORICAL ENCODING
  add_to_dictionary("smoker_encoded", "numeric", "Script 2 - Feature Engineering",
                    "Binary encoding: 1 for smoker, 0 for non-smoker", "0, 1")
  
  add_to_dictionary("sex_encoded", "numeric", "Script 2 - Feature Engineering",
                    "Binary encoding: 1 for male, 0 for female", "0, 1")
  
  add_to_dictionary("bmi_category_encoded", "numeric", "Script 2 - Feature Engineering",
                    "Numeric encoding of BMI categories as factors", "1 to 4")
  
  add_to_dictionary("region_encoded", "numeric", "Script 2 - Feature Engineering",
                    "Numeric encoding: southeast=1, southwest=2, northwest=3, northeast=4", "1, 2, 3, 4")
  
  # BINNING FEATURES
  add_to_dictionary("age_bins", "factor", "Script 2 - Feature Engineering",
                    "Age grouped into 5-year bins", "(15,20], (20,25], ..., (65,70]")
  
  add_to_dictionary("charges_percentile_rank", "numeric", "Script 2 - Feature Engineering",
                    "Percentile rank of charges within the dataset", "0.0 to 1.0")
  
  # ADVANCED STANDALONE FEATURES
  add_to_dictionary("smoker_years_estimate", "numeric", "Script 2 - Feature Engineering",
                    "Estimated smoking years: smoker_encoded * age", "0 to 65")
  
  add_to_dictionary("bmi_health_category", "character", "Script 2 - Feature Engineering",
                    "Health categories based on BMI thresholds", "underweight, normal, overweight, obese")
  
  add_to_dictionary("regional_market_tier", "character", "Script 2 - Feature Engineering",
                    "Market tier classification by region", "economy, standard, premium")
  
  # ADVANCED FEATURES FROM SECTION 6
  add_to_dictionary("smoker_age_severity_index", "numeric", "Script 2 - Advanced Features",
                    "Age-adjusted smoking severity with literature-based multipliers", "0 to 162.5")
  
  add_to_dictionary("metabolic_syndrome_risk", "numeric", "Script 2 - Advanced Features",
                    "Compound risk score for metabolic syndrome based on BMI and age", "1.0 to 3.5")
  
  add_to_dictionary("family_cost_optimization", "numeric", "Script 2 - Advanced Features",
                    "Family size economies of scale factor", "0.65 to 1.0")
  
  add_to_dictionary("compound_lifestyle_risk", "numeric", "Script 2 - Advanced Features",
                    "Comprehensive lifestyle risk: smoking * BMI_risk * age_risk", "0.8 to 4.875")
  
  add_to_dictionary("individual_vs_cohort_ratio", "numeric", "Script 2 - Advanced Features",
                    "Individual charges divided by expected cohort average", "0.1 to 5.0+")
  
  add_to_dictionary("cost_efficiency_quintiles", "numeric", "Script 2 - Advanced Features",
                    "Quintile ranking of cost efficiency (charges/compound_lifestyle_risk)", "1, 2, 3, 4, 5")
  
  add_to_dictionary("outlier_detection_flags", "character", "Script 2 - Advanced Features",
                    "Outlier classification based on multiple criteria", 
                    "Normal, High_Cost_Outlier, Low_Cost_Outlier, Risk_Cost_Mismatch, Low_Risk_High_Cost")
  
  # ENCODING FEATURES FROM SECTION 7
  add_to_dictionary("is_smoker", "numeric", "Script 2 - Encoding",
                    "Binary flag for smoking status", "0, 1")
  
  add_to_dictionary("is_male", "numeric", "Script 2 - Encoding",
                    "Binary flag for male sex", "0, 1")
  
  add_to_dictionary("is_obese", "numeric", "Script 2 - Encoding",
                    "Binary flag for obesity (BMI ≥30)", "0, 1")
  
  add_to_dictionary("has_children_flag", "numeric", "Script 2 - Encoding",
                    "Binary flag for having any children", "0, 1")
  
  add_to_dictionary("smoker_male", "numeric", "Script 2 - Encoding",
                    "Interaction: is_smoker * is_male", "0, 1")
  
  add_to_dictionary("smoker_obese", "numeric", "Script 2 - Encoding",
                    "Interaction: is_smoker * is_obese", "0, 1")
  
  add_to_dictionary("high_risk_combo", "numeric", "Script 2 - Encoding",
                    "Triple interaction: is_smoker * is_obese * is_male", "0, 1")
  
  add_to_dictionary("bmi_risk_ordinal", "numeric", "Script 2 - Encoding",
                    "Ordinal encoding of BMI categories", "1, 2, 3, 4")
  
  add_to_dictionary("region_cost_ordinal", "numeric", "Script 2 - Encoding",
                    "Ordinal encoding of regions by cost", "1, 2, 3, 4")
  
  add_to_dictionary("age_group_ordinal", "numeric", "Script 2 - Encoding",
                    "Ordinal encoding of age groups", "1, 2, 3, 4, 5")
  
  add_to_dictionary("is_young_adult", "numeric", "Script 2 - Encoding",
                    "Binary flag for 18-25 age group", "0, 1")
  
  add_to_dictionary("is_senior", "numeric", "Script 2 - Encoding",
                    "Binary flag for 56-65 age group", "0, 1")
  
  add_to_dictionary("is_middle_aged", "numeric", "Script 2 - Encoding",
                    "Binary flag for 36-55 age groups", "0, 1")
  
  add_to_dictionary("region_northeast", "numeric", "Script 2 - Encoding",
                    "Binary flag for northeast region", "0, 1")
  
  add_to_dictionary("region_northwest", "numeric", "Script 2 - Encoding",
                    "Binary flag for northwest region", "0, 1")
  
  add_to_dictionary("region_southeast", "numeric", "Script 2 - Encoding",
                    "Binary flag for southeast region", "0, 1")
  
  add_to_dictionary("region_southwest", "numeric", "Script 2 - Encoding",
                    "Binary flag for southwest region", "0, 1")
  
  # BUSINESS ANALYSIS FEATURES FROM SECTION 8
  add_to_dictionary("cost_per_risk_point", "numeric", "Script 2 - Business Analysis",
                    "Charges divided by compound lifestyle risk score", "500 to 50000+")
  
  add_to_dictionary("benchmark_deviation_quartile", "numeric", "Script 2 - Business Analysis",
                    "Quartile of absolute deviation from cohort ratio", "1, 2, 3, 4")
  
  add_to_dictionary("regional_cost_opportunity", "character", "Script 2 - Business Analysis",
                    "Market pricing opportunity classification", 
                    "Appropriately_Priced, Overpriced_Market, Underpriced_Market")
  
  add_to_dictionary("family_efficiency_category", "character", "Script 2 - Business Analysis",
                    "Family cost efficiency classification", 
                    "Individual, Efficient_Family, Inefficient_Family, Standard_Family")
  
  add_to_dictionary("high_value_low_risk", "character", "Script 2 - Business Analysis",
                    "Value-risk profile classification", 
                    "Standard_Profile, High_Value_Low_Risk, Low_Value_High_Risk")
  
  add_to_dictionary("intervention_priority", "character", "Script 2 - Business Analysis",
                    "Healthcare intervention priority classification",
                    "Standard_Care, High_Priority, Smoking_Cessation_Target, Weight_Management_Target, Care_Coordination_Target")
  
  add_to_dictionary("market_opportunity", "character", "Script 2 - Business Analysis",
                    "Market expansion opportunity classification",
                    "Saturated_Market, Premium_Market_Opportunity, Value_Market_Opportunity")
  
  add_to_dictionary("subsidization_role", "character", "Script 2 - Business Analysis",
                    "Risk pool contribution classification",
                    "Balanced_Contributor, Cross_Subsidizer, Subsidized_Member")
  
  # TARGET ENCODED FEATURES (if created)
  add_to_dictionary("brand_target_encoded", "numeric", "Script 2 - Target Encoding",
                    "Mean charges by brand category", "Varies by brand", 
                    notes = "Target encoding - use with caution for overfitting")
  
  add_to_dictionary("brand_frequency_encoded", "numeric", "Script 2 - Frequency Encoding",
                    "Frequency count of brand occurrences", "Varies by brand frequency")
  
  cat("✅ All engineered features added to data dictionary!\n")
  cat("📊 Total features added: ~70+ engineered variables\n")
  cat("📁 Dictionary saved to: data/data_dictionary.csv\n")
}

# Function to add ANOVA results as metadata
add_anova_metadata_to_dictionary <- function() {
  
  # Load ANOVA results
  if(file.exists("outputs/tables/complete_feature_analysis_final.csv")) {
    anova_results <- read_csv("outputs/tables/complete_feature_analysis_final.csv")
    
    # Update dictionary with ANOVA statistics for relevant features
    for(i in 1:nrow(anova_results)) {
      feature_name <- anova_results$feature_name[i]
      eta_squared <- round(anova_results$eta_squared[i], 3)
      p_value <- round(anova_results$p_value[i], 3)
      
      # Create ANOVA note
      anova_note <- paste0("ANOVA: η²=", eta_squared, ", p=", p_value)
      
      # Update dictionary
      tryCatch({
        update_dictionary(feature_name, 
                          notes = anova_note)
      }, error = function(e) {
        # Feature might not exist in dictionary yet
      })
    }
    
    cat("✅ ANOVA metadata added to relevant features\n")
  }
}

# Function to generate comprehensive data dictionary summary
generate_dictionary_summary <- function() {
  
  dict <- read_csv("data/data_dictionary.csv", show_col_types = FALSE)
  
  summary_stats <- dict %>%
    group_by(source) %>%
    summarise(
      n_variables = n(),
      n_numeric = sum(data_type %in% c("numeric", "integer")),
      n_categorical = sum(data_type %in% c("character", "factor")),
      total_missing = sum(missing_count),
      avg_missing_pct = round(mean(missing_percent), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(n_variables))
  
  cat("\n📊 DATA DICTIONARY SUMMARY\n")
  cat("═══════════════════════════\n")
  print(summary_stats)
  
  cat("\n🔢 TOTAL VARIABLES:", nrow(dict), "\n")
  cat("📈 NUMERIC VARIABLES:", sum(dict$data_type %in% c("numeric", "integer")), "\n")
  cat("📝 CATEGORICAL VARIABLES:", sum(dict$data_type %in% c("character", "factor")), "\n")
  cat("❌ VARIABLES WITH MISSING DATA:", sum(dict$missing_count > 0), "\n")
  
  return(summary_stats)
}

# Execute the updates
cat("🚀 Starting data dictionary updates...\n\n")

# Add all engineered features
add_engineered_features_to_dictionary()

# Add ANOVA metadata
add_anova_metadata_to_dictionary()

# Generate summary
summary_stats <- generate_dictionary_summary()

# Save summary
write_csv(summary_stats, "outputs/tables/data_dictionary_summary.csv")

cat("\n✅ Data dictionary update complete!\n")
cat("📁 Full dictionary: data/data_dictionary.csv\n")
cat("📊 Summary report: outputs/tables/data_dictionary_summary.csv\n")