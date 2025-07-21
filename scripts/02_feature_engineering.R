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

# benchmarks
print(hcup_age_summary)

# 2 Merge Insurance w/ Benchmarks ####

insurance_with_benchmarks <- insurance_clean %>%
  left_join(hcup_age_summary, by = "age_group_standard")

# Check if the merge worked
glimpse(insurance_with_benchmarks)

# How many people got benchmarks?
sum(!is.na(insurance_with_benchmarks$avg_hcup_charges))

# 3 FIRST FEATURE ####

# Create simple ratio: How does each person compare to the national average?
insurance_with_benchmarks <- insurance_with_benchmarks %>%
  mutate(
    cost_vs_national = charges / avg_hcup_charges
  )

# Look at new feature
summary(insurance_with_benchmarks$cost_vs_national)

# What does this mean?
# cost_vs_national = 1.5 means person costs 50% more than national average
# cost_vs_national = 0.8 means person costs 20% less than national average

# Examples
insurance_with_benchmarks %>%
  select(age, age_group_standard, charges, avg_hcup_charges, cost_vs_national) %>%
  head(10)

# Save simple version 
write_csv(insurance_with_benchmarks, "data/processed/insurance_step1.csv")

# 4 Explore cost comparisons ####
insurance_with_benchmarks %>%
  select(age, age_group_standard, charges, avg_hcup_charges, cost_vs_national, smoker) %>%
  arrange(desc(cost_vs_national)) %>%  # Highest ratios first
  head(10)

# Distribution
hist(insurance_with_benchmarks$cost_vs_national, 
     main = "Individual Insurance Costs vs National Hospital Averages",
     xlab = "Cost Ratio")

# Smokers vs non-smokers
insurance_with_benchmarks %>%
  group_by(smoker) %>%
  summarise(
    avg_ratio = mean(cost_vs_national),
    median_ratio = median(cost_vs_national)
  )

# Quick Recap for Understanding ####
  # already knew smokers/non-smokers would have significant differences just from data cleaning/exploration
  # realized ANOVA anaylsis didn't happen and could be helpful in focusing feature egineering; added ANOVA analysis tables to script 01
  # next with Interaction Features (non-linear/complex variables; allows model to consider real world non-linear impacts)- we'd see how age impacts those differences/outcomes (charges/costs)
  # review ANOVA tables

# 5 ANOVA Analysis ####
 ## segue/ regroup to ANOVA analysis to determine relevant features/variables
  ### see ANOVA analysis tables

# 5.1 Core variables tests ####
smoker_test <- aov(charges ~ smoker, data = insurance_with_benchmarks)
sex_test <- aov(charges ~ sex, data = insurance_with_benchmarks) 
age_test <- aov(charges ~ age_group_standard, data = insurance_with_benchmarks)
bmi_test <- aov(charges ~ bmi_category, data = insurance_with_benchmarks)
region_test <- aov(charges ~ region, data = insurance_with_benchmarks)
child_test <- aov(charges ~ has_children, data = insurance_with_benchmarks)

summary(smoker_test)
summary(sex_test)
summary(age_test)
summary(bmi_test)
summary(region_test)
summary(child_test)

# 5.2 Core variable interactions tests ####

smoker_sex_test <- aov(charges ~ smoker * sex, data = insurance_with_benchmarks)
smoker_age_test <- aov(charges ~ smoker * age_group_standard, data = insurance_with_benchmarks)
sex_age_test <- aov(charges ~ sex * age_group_standard, data = insurance_with_benchmarks)
bmi_smoker_test <- aov(charges ~ bmi_category * smoker, data = insurance_with_benchmarks)
bmi_sex_test <- aov(charges ~ bmi_category * sex, data = insurance_with_benchmarks)
sex_child_test <- aov(charges ~ has_children * sex, data = insurance_with_benchmarks)

summary(smoker_sex_test)
summary(smoker_age_test)
summary(sex_age_test)
summary(bmi_smoker_test)
summary(bmi_sex_test)
summary(sex_child_test)

# 5.3 Extracting and Updating ANOVA Tables ####

# Function to extract ANOVA results for better handling
extract_anova_results_enhanced <- function(anova_test, test_name) {
  tryCatch({
    summary_results <- summary(anova_test)
    results_table <- summary_results[[1]]
    
    # Results DF
    results_df <- data.frame(
      test_name = test_name,
      term = rownames(results_table),
      f_value = results_table[, "F value"],
      p_value = results_table[, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
    
    # Remove the residuals row
    results_df <- results_df[results_df$term != "Residuals", ]
    
    return(results_df)
  }, error = function(e) {
    return(data.frame(test_name = test_name, term = NA, f_value = NA, p_value = NA))
  })
}

# Extract results from all completed tests
completed_tests <- rbind(
  extract_anova_results_enhanced(smoker_test, "smoker"),
  extract_anova_results_enhanced(sex_test, "sex"),
  extract_anova_results_enhanced(age_test, "age_group_standard"),
  extract_anova_results_enhanced(bmi_test, "bmi_category"),
  extract_anova_results_enhanced(region_test, "region"),
  extract_anova_results_enhanced(child_test, "has_children"),
  extract_anova_results_enhanced(smoker_sex_test, "smoker_sex"),
  extract_anova_results_enhanced(smoker_age_test, "smoker_age"),
  extract_anova_results_enhanced(sex_age_test, "sex_age"),
  extract_anova_results_enhanced(bmi_smoker_test, "bmi_smoker"),
  extract_anova_results_enhanced(bmi_sex_test, "bmi_sex"),
  extract_anova_results_enhanced(sex_child_test, "sex_child")
)

# Function to match test results to planned analyses
match_test_to_plan <- function(plan_row, test_results) {
  # Mapping rules for different test types
  test_mapping <- list(
    "aov(charges ~ smoker)" = "smoker",
    "aov(charges ~ sex)" = "sex",
    "aov(charges ~ region)" = "region",
    "aov(charges ~ bmi_category)" = "bmi_category",
    "aov(charges ~ age_group_standard)" = "age_group_standard",
    "aov(charges ~ has_children)" = "has_children",
    "aov(charges ~ smoker * sex)" = "smoker_sex",
    "aov(charges ~ smoker * age_group_standard)" = "smoker_age",
    "aov(charges ~ sex * age_group_standard)" = "sex_age",
    "aov(charges ~ bmi_category * smoker)" = "bmi_smoker",
    "aov(charges ~ bmi_category * sex)" = "bmi_sex",
    "aov(charges ~ has_children * sex)" = "sex_child"
  )
  
  test_key <- test_mapping[[plan_row$anova_code]]
  
  if (!is.null(test_key)) {
    # Filter results for this test
    test_data <- test_results[test_results$test_name == test_key, ]
    
    if (nrow(test_data) > 0) {
      # For interaction tests, find the main effect or interaction term
      if (grepl("\\*", plan_row$anova_code)) {
        # Look for interaction term first
        interaction_term <- test_data[grepl(":", test_data$term), ]
        if (nrow(interaction_term) > 0) {
          return(list(
            f_value = round(interaction_term$f_value[1], 3),
            p_value = ifelse(interaction_term$p_value[1] < 0.001, "< 0.001", 
                             round(interaction_term$p_value[1], 4)),
            significance = ifelse(interaction_term$p_value[1] < 0.001, "***",
                                  ifelse(interaction_term$p_value[1] < 0.01, "**",
                                         ifelse(interaction_term$p_value[1] < 0.05, "*",
                                                ifelse(interaction_term$p_value[1] < 0.1, ".", "ns")))),
            status = "Completed"
          ))
        }
      }
      
      # For main effects or if no interaction found
      main_effect <- test_data[1, ]  # Take first row
      return(list(
        f_value = round(main_effect$f_value, 3),
        p_value = ifelse(main_effect$p_value < 0.001, "< 0.001", 
                         round(main_effect$p_value, 4)),
        significance = ifelse(main_effect$p_value < 0.001, "***",
                              ifelse(main_effect$p_value < 0.01, "**",
                                     ifelse(main_effect$p_value < 0.05, "*",
                                            ifelse(main_effect$p_value < 0.1, ".", "ns")))),
        status = "Completed"
      ))
    }
  }
  
  # Return NA values if no match found
  return(list(
    f_value = NA,
    p_value = NA,
    significance = NA,
    status = "Not Completed"
  ))
}

# Existing tables
regular_anova_plan <- read_csv("outputs/tables/regular_anova_analysis_plan.csv", show_col_types = FALSE)
engineered_anova_plan <- read_csv("outputs/tables/engineered_features_anova_plan.csv", show_col_types = FALSE)

# Update the regular ANOVA plan table
regular_anova_plan_updated <- regular_anova_plan %>%
  rowwise() %>%
  mutate(
    results = list(match_test_to_plan(cur_data(), completed_tests))
  ) %>%
  unnest_wider(results) %>%
  select(analysis_type, variables, anova_code, business_question, priority, 
         f_value, p_value, significance, status)

# Update engineered features table (add columns if they don't exist)
if(!"f_value" %in% colnames(engineered_anova_plan)) {
  engineered_anova_plan_updated <- engineered_anova_plan %>%
    mutate(
      f_value = NA,
      p_value = NA,
      significance = NA,
      status = "Not Completed"
    )
} else {
  engineered_anova_plan_updated <- engineered_anova_plan
}

# Save updated tables (overwrite existing)
write_csv(regular_anova_plan_updated, "outputs/tables/regular_anova_analysis_plan.csv")
write_csv(engineered_anova_plan_updated, "outputs/tables/engineered_features_anova_plan.csv")

# Update existing data dictionary
update_existing_data_dictionary <- function() {
  # Load existing dictionary
  dict <- read_csv("data/data_dictionary.csv", show_col_types = FALSE)
  
  # Create summary of ANOVA results for notes
  significant_results <- completed_tests %>%
    filter(p_value < 0.05) %>%
    mutate(
      significance = ifelse(p_value < 0.001, "***",
                            ifelse(p_value < 0.01, "**", "*")),
      result_summary = paste0(term, " (F=", round(f_value, 2), ", p", 
                              ifelse(p_value < 0.001, "<0.001", paste0("=", round(p_value, 3))), 
                              ", ", significance, ")")
    )
  
  # Update notes for variables that were tested
  variable_mappings <- list(
    "smoker" = "smoker",
    "sex" = "sex", 
    "age_group_standard" = "age_group_standard",
    "bmi_category" = "bmi_category",
    "region" = "region",
    "has_children" = "has_children"
  )
  
  for(dict_var in names(variable_mappings)) {
    test_var <- variable_mappings[[dict_var]]
    
    if(dict_var %in% dict$variable_name) {
      # Find all test results for this variable
      var_results <- significant_results %>%
        filter(test_name == test_var | str_detect(test_name, test_var))
      
      if(nrow(var_results) > 0) {
        # Create ANOVA results summary
        anova_note <- paste("ANOVA results:", paste(var_results$result_summary, collapse = "; "))
        
        # Update notes column
        current_notes <- dict[dict$variable_name == dict_var, "notes"]
        if(is.na(current_notes) || current_notes == "NA") {
          dict[dict$variable_name == dict_var, "notes"] <- anova_note
        } else {
          dict[dict$variable_name == dict_var, "notes"] <- paste(current_notes, anova_note, sep = "; ")
        }
      }
    }
  }
  
  # Save updated dictionary
  write_csv(dict, "data/data_dictionary.csv")
  
  return(dict)
}

# Update data dictionary
updated_dictionary <- update_existing_data_dictionary()

# Create summary outputs
completed_summary <- regular_anova_plan_updated %>%
  filter(status == "Completed") %>%
  arrange(desc(f_value)) %>%
  select(variables, business_question, f_value, p_value, significance, priority)

priority_summary <- regular_anova_plan_updated %>%
  filter(status == "Completed") %>%
  count(priority, significance) %>%
  arrange(priority, significance)

missing_high_priority <- regular_anova_plan_updated %>%
  filter(priority %in% c("High", "Very High") & status == "Not Completed") %>%
  select(variables, business_question, anova_code, priority)

# 6 Non-linear transformations (Age and BMI)####

insurance_with_benchmarks <- insurance_with_benchmarks %>%
  mutate(
    # Age non-linearity
    age_squared = age^2,
    age_cubed = age^3,
    
    # BMI non-linearity  
    bmi_squared = bmi^2,
    bmi_cubed = bmi^3,
    
    # Age decade grouping
    age_decade = floor(age/10) * 10,
    
    # Advanced BMI categories
    bmi_detailed = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal",
      bmi < 30 ~ "Overweight",
      bmi < 35 ~ "Obese_I",
      bmi < 40 ~ "Obese_II",
      TRUE ~ "Obese_III"
    )
  )


# 7 Evidence Based Feature Engineering ####

# High-priority interactions (Very High priority from ANOVA)
insurance_with_benchmarks <- insurance_with_benchmarks %>%
  mutate(
    # Smoker interactions
    smoker_age_interaction = ifelse(smoker == "yes", age, 0),
    smoker_bmi_interaction = ifelse(smoker == "yes", bmi, 0),
    smoker_age_numeric = as.numeric(smoker == "yes") * age,
    
    # BMI-Age interaction
    age_bmi_interaction = age * bmi,
    
    # High-risk combinations
    high_risk_combo = case_when(
      smoker == "yes" & bmi >= 30 ~ "Smoker_Obese",
      smoker == "yes" & bmi < 30 ~ "Smoker_Normal",
      smoker == "no" & bmi >= 30 ~ "NonSmoker_Obese", 
      TRUE ~ "NonSmoker_Normal"
    ),
    
    # Complex risk scoring
    risk_score = case_when(
      smoker == "yes" & bmi >= 30 & age >= 50 ~ "Very High",
      smoker == "yes" & (bmi >= 30 | age >= 50) ~ "High",
      smoker == "no" & bmi >= 30 & age >= 50 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# 7.2 Categorical encoding via fastDummies package for modeling

# One-hot encoding for modeling
model_ready_data <- insurance_with_benchmarks %>%
  dummy_cols(
    select_columns = c("sex", "region", "smoker", "bmi_category", "high_risk_combo"),
    remove_first_dummy = TRUE,
    remove_selected_columns = FALSE
  )

## feature scaling/normalization

# numeric features for scaling
numeric_features <- c("age", "bmi", "children", "cost_vs_national", 
                      "age_squared", "bmi_squared", "age_bmi_interaction")

# scaled versions
scaled_features <- model_ready_data %>%
  select(all_of(numeric_features)) %>%
  scale() %>%
  as_tibble() %>%
  rename_with(~paste0(., "_scaled"))

# combine with original data
final_feature_data <- bind_cols(
  model_ready_data,
  scaled_features
)

# 7.3 Feature Egineering - actual ####

# benchmark-derived features
insurance_with_benchmarks <- insurance_with_benchmarks %>%
  mutate(
    # Additional benchmark features
    cost_deviation = charges - avg_hcup_charges,
    cost_percentile = percent_rank(cost_vs_national),
    is_cost_outlier = cost_vs_national > 2 | cost_vs_national < 0.5,
    
    # Regional cost context
    region_cost_rank = case_when(
      region == "southeast" ~ 1,  # Adjust based on your data
      region == "southwest" ~ 2,
      region == "northwest" ~ 3,
      region == "northeast" ~ 4
    )
  )

# 7.4 Feature selection and validation ####

correlation_matrix <- final_feature_data %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# Remove highly correlated features (>0.9)
high_corr_pairs <- findCorrelation(correlation_matrix, cutoff = 0.9)

# Variance analysis
near_zero_var <- nearZeroVar(final_feature_data)

# Save for modeling
write_csv(final_feature_data, "data/processed/engineered_features.csv")

# Create feature documentation
feature_summary <- final_feature_data %>%
  summarise(across(where(is.numeric), 
                   list(mean = mean, sd = sd, min = min, max = max),
                   na.rm = TRUE))

write_csv(feature_summary, "outputs/tables/feature_summary_stats.csv")

# 8 Script 3 (Modeling) Agenda: ####
# Data Splitting: Create train/validation/test sets
# Baseline Models: Linear regression, basic tree models
# Model Selection: Choose 4-5 algorithms to compare
# Run ANOVA on new engineered features:
