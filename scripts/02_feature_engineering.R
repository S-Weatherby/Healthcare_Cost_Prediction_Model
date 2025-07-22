# Complete Feature Engineering Script - Script 02
# Author: [Your Name]
# Date: [Current Date]
# Purpose: Feature engineering with ANOVA analysis and comprehensive documentation

# Load required libraries
library(tidyverse)
library(fastDummies)
library(caret)
library(corrplot)

# Ensure output directories exist
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Load data
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

# Save benchmark summary
write_csv(hcup_age_summary, "outputs/tables/hcup_benchmark_summary.csv")

# Display benchmarks
print(hcup_age_summary)

# 2 Merge Insurance w/ Benchmarks ####

insurance_with_benchmarks <- insurance_clean %>%
  left_join(hcup_age_summary, by = "age_group_standard")

# Check if the merge worked
glimpse(insurance_with_benchmarks)

# How many people got benchmarks?
merge_success_count <- sum(!is.na(insurance_with_benchmarks$avg_hcup_charges))
cat("Records successfully merged with benchmarks:", merge_success_count, "\n")

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
cost_comparison_examples <- insurance_with_benchmarks %>%
  select(age, age_group_standard, charges, avg_hcup_charges, cost_vs_national) %>%
  head(10)

# Save examples table
write_csv(cost_comparison_examples, "outputs/tables/cost_comparison_examples.csv")

# Save step 1 data
write_csv(insurance_with_benchmarks, "data/processed/insurance_step1.csv")

# 4 Explore cost comparisons ####
highest_cost_ratios <- insurance_with_benchmarks %>%
  select(age, age_group_standard, charges, avg_hcup_charges, cost_vs_national, smoker) %>%
  arrange(desc(cost_vs_national)) %>%  # Highest ratios first
  head(10)

# Save highest cost ratios table
write_csv(highest_cost_ratios, "outputs/tables/highest_cost_ratios.csv")

# Distribution analysis
cost_ratio_stats <- insurance_with_benchmarks %>%
  summarise(
    mean_ratio = mean(cost_vs_national, na.rm = TRUE),
    median_ratio = median(cost_vs_national, na.rm = TRUE),
    sd_ratio = sd(cost_vs_national, na.rm = TRUE),
    min_ratio = min(cost_vs_national, na.rm = TRUE),
    max_ratio = max(cost_vs_national, na.rm = TRUE),
    q25 = quantile(cost_vs_national, 0.25, na.rm = TRUE),
    q75 = quantile(cost_vs_national, 0.75, na.rm = TRUE)
  )

write_csv(cost_ratio_stats, "outputs/tables/cost_ratio_distribution_stats.csv")

# Create histogram
png("outputs/plots/cost_ratio_histogram.png", width = 800, height = 600)
hist(insurance_with_benchmarks$cost_vs_national, 
     main = "Individual Insurance Costs vs National Hospital Averages",
     xlab = "Cost Ratio",
     breaks = 30,
     col = "lightblue",
     border = "black")
dev.off()

# Smokers vs non-smokers comparison
smoker_cost_comparison <- insurance_with_benchmarks %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    avg_ratio = mean(cost_vs_national, na.rm = TRUE),
    median_ratio = median(cost_vs_national, na.rm = TRUE),
    sd_ratio = sd(cost_vs_national, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(smoker_cost_comparison, "outputs/tables/smoker_cost_comparison.csv")

# 5 ANOVA Analysis ####

# 5.1 Core variables tests ####
smoker_test <- aov(charges ~ smoker, data = insurance_with_benchmarks)
sex_test <- aov(charges ~ sex, data = insurance_with_benchmarks) 
age_test <- aov(charges ~ age_group_standard, data = insurance_with_benchmarks)
bmi_test <- aov(charges ~ bmi_category, data = insurance_with_benchmarks)
region_test <- aov(charges ~ region, data = insurance_with_benchmarks)
child_test <- aov(charges ~ has_children, data = insurance_with_benchmarks)

# Print summaries for review
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

# Print interaction summaries
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

# Save all ANOVA results
write_csv(completed_tests, "outputs/tables/anova_results_detailed.csv")

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

# Create ANOVA plan tables if they don't exist
create_anova_plans <- function() {
  # Regular ANOVA plan
  regular_anova_plan <- data.frame(
    analysis_type = c(rep("Main Effects", 6), rep("Interactions", 6)),
    variables = c("smoker", "sex", "age_group_standard", "bmi_category", "region", "has_children",
                  "smoker * sex", "smoker * age_group_standard", "sex * age_group_standard",
                  "bmi_category * smoker", "bmi_category * sex", "has_children * sex"),
    anova_code = c("aov(charges ~ smoker)", "aov(charges ~ sex)", "aov(charges ~ age_group_standard)",
                   "aov(charges ~ bmi_category)", "aov(charges ~ region)", "aov(charges ~ has_children)",
                   "aov(charges ~ smoker * sex)", "aov(charges ~ smoker * age_group_standard)",
                   "aov(charges ~ sex * age_group_standard)", "aov(charges ~ bmi_category * smoker)",
                   "aov(charges ~ bmi_category * sex)", "aov(charges ~ has_children * sex)"),
    business_question = c("Do smokers have significantly different charges?",
                          "Do males and females have different charges?",
                          "Do different age groups have different charges?",
                          "Do BMI categories affect charges?",
                          "Do regional differences exist in charges?",
                          "Do people with children have different charges?",
                          "Is the smoking effect different for males vs females?",
                          "Does smoking impact vary by age group?",
                          "Do charge differences between genders vary by age?",
                          "Does BMI impact differ between smokers and non-smokers?",
                          "Does BMI impact differ between males and females?",
                          "Does having children affect charges differently for males vs females?"),
    priority = c("Very High", "Medium", "High", "High", "Low", "Medium",
                 "High", "Very High", "Medium", "Very High", "Medium", "Low"),
    stringsAsFactors = FALSE
  )
  
  # Engineered features ANOVA plan
  engineered_anova_plan <- data.frame(
    analysis_type = c(rep("Engineered Features", 8)),
    variables = c("cost_vs_national", "high_risk_combo", "risk_score", "age_squared",
                  "bmi_squared", "age_bmi_interaction", "smoker_age_interaction", "cost_deviation"),
    anova_code = c("aov(charges ~ cost_vs_national)", "aov(charges ~ high_risk_combo)",
                   "aov(charges ~ risk_score)", "aov(charges ~ age_squared)",
                   "aov(charges ~ bmi_squared)", "aov(charges ~ age_bmi_interaction)",
                   "aov(charges ~ smoker_age_interaction)", "aov(charges ~ cost_deviation)"),
    business_question = c("Does relative cost vs national average predict actual charges?",
                          "Do high-risk combinations significantly affect charges?",
                          "Does our risk scoring system correlate with charges?",
                          "Does age-squared capture non-linear age effects?",
                          "Does BMI-squared capture non-linear BMI effects?",
                          "Does age-BMI interaction significantly affect charges?",
                          "Does smoker-age interaction significantly affect charges?",
                          "Does deviation from national average predict charges?"),
    priority = c("High", "Very High", "High", "Medium", "Medium", "High", "Very High", "Medium"),
    stringsAsFactors = FALSE
  )
  
  return(list(regular = regular_anova_plan, engineered = engineered_anova_plan))
}

# Create or load existing ANOVA plans
if (file.exists("outputs/tables/regular_anova_analysis_plan.csv")) {
  regular_anova_plan <- read_csv("outputs/tables/regular_anova_analysis_plan.csv", show_col_types = FALSE)
} else {
  plans <- create_anova_plans()
  regular_anova_plan <- plans$regular
  write_csv(regular_anova_plan, "outputs/tables/regular_anova_analysis_plan.csv")
}

if (file.exists("outputs/tables/engineered_features_anova_plan.csv")) {
  engineered_anova_plan <- read_csv("outputs/tables/engineered_features_anova_plan.csv", show_col_types = FALSE)
} else {
  plans <- create_anova_plans()
  engineered_anova_plan <- plans$engineered
  write_csv(engineered_anova_plan, "outputs/tables/engineered_features_anova_plan.csv")
}

# Update the regular ANOVA plan table
regular_anova_plan_updated <- regular_anova_plan %>%
  select(-any_of(c("f_value", "p_value", "significance", "status"))) %>%
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

# 6 Non-linear transformations (Age and BMI) ####

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
    ),
    
    # Additional benchmark features
    cost_deviation = charges - avg_hcup_charges,
    cost_percentile = percent_rank(cost_vs_national),
    is_cost_outlier = cost_vs_national > 2 | cost_vs_national < 0.5,
    
    # Regional cost context (adjust based on your data)
    region_cost_rank = case_when(
      region == "southeast" ~ 1,
      region == "southwest" ~ 2,
      region == "northwest" ~ 3,
      region == "northeast" ~ 4,
      TRUE ~ 2  # default
    )
  )

# 7.2 Categorical encoding via fastDummies package for modeling ####

# One-hot encoding for modeling
model_ready_data <- insurance_with_benchmarks %>%
  dummy_cols(
    select_columns = c("sex", "region", "smoker", "bmi_category", "high_risk_combo", "risk_score", "bmi_detailed"),
    remove_first_dummy = TRUE,
    remove_selected_columns = FALSE
  )

# Feature scaling/normalization
# Numeric features for scaling
numeric_features <- c("age", "bmi", "children", "cost_vs_national", 
                      "age_squared", "bmi_squared", "age_bmi_interaction",
                      "smoker_age_interaction", "smoker_bmi_interaction", 
                      "cost_deviation", "cost_percentile")

# Create scaled versions
scaled_features <- model_ready_data %>%
  select(all_of(numeric_features)) %>%
  scale() %>%
  as_tibble() %>%
  rename_with(~paste0(., "_scaled"))

# Combine with original data
final_feature_data <- bind_cols(
  model_ready_data,
  scaled_features
)

# 7.4 Feature selection and validation ####

# Correlation analysis
numeric_only_data <- final_feature_data %>%
  select(where(is.numeric))

correlation_matrix <- cor(numeric_only_data, use = "complete.obs")

# Save correlation matrix
write_csv(as.data.frame(correlation_matrix), "outputs/tables/correlation_matrix.csv")

# Find highly correlated features (>0.9)
high_corr_pairs <- findCorrelation(correlation_matrix, cutoff = 0.9, names = TRUE)

# Variance analysis
near_zero_var_indices <- nearZeroVar(final_feature_data)
near_zero_var_names <- names(final_feature_data)[near_zero_var_indices]

# Create feature selection summary
feature_selection_summary <- data.frame(
  metric = c("Total Features", "Numeric Features", "High Correlation Features", "Near Zero Variance Features"),
  count = c(ncol(final_feature_data), ncol(numeric_only_data), length(high_corr_pairs), length(near_zero_var_names)),
  details = c("All features in dataset", "Features suitable for correlation analysis", 
              paste(high_corr_pairs, collapse = ", "), paste(near_zero_var_names, collapse = ", "))
)

write_csv(feature_selection_summary, "outputs/tables/feature_selection_summary.csv")

# Save final data for modeling
write_csv(final_feature_data, "data/processed/engineered_features.csv")

# Create comprehensive feature documentation
feature_summary <- final_feature_data %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE), 
                        min = ~min(.x, na.rm = TRUE), 
                        max = ~max(.x, na.rm = TRUE),
                        missing = ~sum(is.na(.x))),
                   .names = "{.col}_{.fn}"))

# Transpose for better readability
feature_summary_t <- feature_summary %>%
  pivot_longer(everything(), names_to = "feature_stat", values_to = "value") %>%
  separate(feature_stat, into = c("feature", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  arrange(feature)

write_csv(feature_summary_t, "outputs/tables/feature_summary_stats.csv")

# Update data dictionary with all new features
# Replace the existing update_data_dictionary function 
update_data_dictionary <- function() {
  # Create or load existing dictionary
  if (file.exists("data/data_dictionary.csv")) {
    dict <- read_csv("data/data_dictionary.csv", show_col_types = FALSE)
  } else {
    dict <- data.frame(
      variable_name = character(),
      data_type = character(),
      description = character(),
      source = character(),
      created_date = character(),
      notes = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Ensure dict has all required columns
  required_cols <- c("variable_name", "data_type", "description", "source", "created_date", "notes")
  for(col in required_cols) {
    if(!col %in% names(dict)) {
      dict[[col]] <- NA_character_
    }
  }
  
  # Reorder columns to match expected structure
  dict <- dict %>% select(all_of(required_cols))
  
  # New features to add
  new_features <- data.frame(
    variable_name = c(
      "cost_vs_national", "age_squared", "age_cubed", "bmi_squared", "bmi_cubed",
      "age_decade", "bmi_detailed", "smoker_age_interaction", "smoker_bmi_interaction",
      "smoker_age_numeric", "age_bmi_interaction", "high_risk_combo", "risk_score",
      "cost_deviation", "cost_percentile", "is_cost_outlier", "region_cost_rank"
    ),
    data_type = c(
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "categorical", "numeric", "numeric",
      "numeric", "numeric", "categorical", "categorical",
      "numeric", "numeric", "logical", "numeric"
    ),
    description = c(
      "Ratio of individual charges to national average hospital charges for age group",
      "Age squared - captures non-linear age effects",
      "Age cubed - captures additional non-linear age effects",
      "BMI squared - captures non-linear BMI effects", 
      "BMI cubed - captures additional non-linear BMI effects",
      "Age grouped by decades (20s, 30s, 40s, etc.)",
      "Detailed BMI categories including obesity classifications",
      "Age value for smokers, 0 for non-smokers - captures smoking-age interaction",
      "BMI value for smokers, 0 for non-smokers - captures smoking-BMI interaction",
      "Numeric smoking indicator multiplied by age",
      "Age multiplied by BMI - captures age-BMI interaction effects",
      "Combined smoking and obesity risk categories",
      "Multi-factor risk score combining smoking, BMI, and age",
      "Difference between individual charges and national average",
      "Percentile rank of cost_vs_national ratio",
      "Flag for cost outliers (>2x or <0.5x national average)",
      "Numeric ranking of regions by typical cost levels"
    ),
    source = "Feature Engineering Script 02",
    created_date = as.character(Sys.Date()),
    notes = "Created for predictive modeling",
    stringsAsFactors = FALSE
  )
  
  # Add ANOVA results to existing variables
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
        
        # Update notes column - fix indexing
        row_index <- which(dict$variable_name == dict_var)
        if(length(row_index) > 0) {
          current_notes <- dict$notes[row_index]
          if(is.na(current_notes) || current_notes == "NA" || current_notes == "" || current_notes == "NULL") {
            dict$notes[row_index] <- anova_note
          } else {
            dict$notes[row_index] <- paste(current_notes, anova_note, sep = "; ")
          }
        }
      }
    }
  }
  
  # Add new features to dictionary - ensure same column structure
  for(i in 1:nrow(new_features)) {
    feature <- new_features[i, ]
    if(!feature$variable_name %in% dict$variable_name) {
      # Ensure feature has all required columns in correct order
      feature_to_add <- data.frame(
        variable_name = feature$variable_name,
        data_type = feature$data_type,
        description = feature$description,
        source = feature$source,
        created_date = feature$created_date,
        notes = feature$notes,
        stringsAsFactors = FALSE
      )
      dict <- rbind(dict, feature_to_add)
    }
  }
  
  # Save updated dictionary
  write_csv(dict, "data/data_dictionary.csv")
  
  return(dict)
}
# Update data dictionary
updated_dictionary <- update_data_dictionary()

# Create analysis summary outputs
completed_summary <- regular_anova_plan_updated %>%
  filter(status == "Completed" & !is.na(f_value)) %>%
  arrange(desc(f_value)) %>%
  select(variables, business_question, f_value, p_value, significance, priority)

write_csv(completed_summary, "outputs/tables/anova_completed_summary.csv")

priority_summary <- regular_anova_plan_updated %>%
  filter(status == "Completed" & !is.na(significance)) %>%
  count(priority, significance) %>%
  arrange(priority, significance)

write_csv(priority_summary, "outputs/tables/anova_priority_summary.csv")

missing_high_priority <- regular_anova_plan_updated %>%
  filter(priority %in% c("High", "Very High") & status == "Not Completed") %>%
  select(variables, business_question, anova_code, priority)

write_csv(missing_high_priority, "outputs/tables/missing_high_priority_tests.csv")

# 8  ANOVA on Engineered Features ####

# Test the new engineered features that we created
engineered_feature_tests <- list()

# Test the features we actually created
if("cost_vs_national" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["cost_vs_national"]] <- aov(charges ~ cost_vs_national, data = insurance_with_benchmarks)
}

if("high_risk_combo" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["high_risk_combo"]] <- aov(charges ~ high_risk_combo, data = insurance_with_benchmarks)
}

if("risk_score" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["risk_score"]] <- aov(charges ~ risk_score, data = insurance_with_benchmarks)
}

if("age_squared" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["age_squared"]] <- aov(charges ~ age_squared, data = insurance_with_benchmarks)
}

if("bmi_squared" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["bmi_squared"]] <- aov(charges ~ bmi_squared, data = insurance_with_benchmarks)
}

if("age_bmi_interaction" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["age_bmi_interaction"]] <- aov(charges ~ age_bmi_interaction, data = insurance_with_benchmarks)
}

if("smoker_age_interaction" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["smoker_age_interaction"]] <- aov(charges ~ smoker_age_interaction, data = insurance_with_benchmarks)
}

if("cost_deviation" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["cost_deviation"]] <- aov(charges ~ cost_deviation, data = insurance_with_benchmarks)
}

# Extract results from engineered feature tests
engineered_test_results <- data.frame()

for(test_name in names(engineered_feature_tests)) {
  result <- extract_anova_results_enhanced(engineered_feature_tests[[test_name]], test_name)
  engineered_test_results <- rbind(engineered_test_results, result)
}

# Save engineered feature ANOVA results
write_csv(engineered_test_results, "outputs/tables/engineered_features_anova_results.csv")

# Update engineered features ANOVA plan with results
update_engineered_plan <- function(plan, results) {
  plan_updated <- plan %>%
    mutate(
      f_value = NA,
      p_value = NA,
      significance = NA,
      status = "Not Completed"
    )
  
  for(i in 1:nrow(plan_updated)) {
    # Extract variable name from ANOVA code
    var_pattern <- "aov\\(charges ~ ([^)]+)\\)"
    var_match <- str_extract(plan_updated$anova_code[i], "(?<=~ )[^)]+")
    
    if(!is.na(var_match)) {
      matching_result <- results[results$test_name == var_match, ]
      
      if(nrow(matching_result) > 0 && !is.na(matching_result$f_value[1])) {
        plan_updated$f_value[i] <- round(matching_result$f_value[1], 3)
        plan_updated$p_value[i] <- ifelse(matching_result$p_value[1] < 0.001, "< 0.001", 
                                          round(matching_result$p_value[1], 4))
        plan_updated$significance[i] <- ifelse(matching_result$p_value[1] < 0.001, "***",
                                               ifelse(matching_result$p_value[1] < 0.01, "**",
                                                      ifelse(matching_result$p_value[1] < 0.05, "*",
                                                             ifelse(matching_result$p_value[1] < 0.1, ".", "ns"))))
        plan_updated$status[i] <- "Completed"
      }
    }
  }
  
  return(plan_updated)
}

# Update the engineered features plan
engineered_anova_plan_final <- update_engineered_plan(engineered_anova_plan_updated, engineered_test_results)

# Save the updated engineered features plan
write_csv(engineered_anova_plan_final, "outputs/tables/engineered_features_anova_plan.csv")

# 9 Final Data Quality Checks ####

# Check for missing values in final dataset
missing_summary <- final_feature_data %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

write_csv(missing_summary, "outputs/tables/final_missing_values_summary.csv")

# Data type summary
data_types_summary <- final_feature_data %>%
  summarise(across(everything(), class)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "data_type") %>%
  count(data_type, sort = TRUE)

write_csv(data_types_summary, "outputs/tables/data_types_summary.csv")

# Feature count summary
feature_counts <- data.frame(
  category = c("Original Features", "Benchmark Features", "Non-linear Transformations", 
               "Interaction Features", "Risk Score Features", "One-hot Encoded Features",
               "Scaled Features", "Total Features"),
  count = c(
    ncol(insurance_clean),
    1,  # avg_hcup_charges
    6,  # age_squared, age_cubed, bmi_squared, bmi_cubed, age_decade, bmi_detailed
    4,  # smoker_age_interaction, smoker_bmi_interaction, smoker_age_numeric, age_bmi_interaction
    6,  # high_risk_combo, risk_score, cost_deviation, cost_percentile, is_cost_outlier, region_cost_rank
    sum(str_detect(names(final_feature_data), "^(sex|region|smoker|bmi_category|high_risk_combo|risk_score|bmi_detailed)_")),
    sum(str_detect(names(final_feature_data), "_scaled$")),
    ncol(final_feature_data)
  )
)

write_csv(feature_counts, "outputs/tables/feature_engineering_summary.csv")

# 10 Create Final Summary Report ####

# completed_summary with error checking
completed_summary <- regular_anova_plan_updated %>%
  filter(status == "Completed" & !is.na(f_value)) %>%
  arrange(desc(f_value)) %>%
  select(variables, business_question, f_value, p_value, significance, priority)

# Check for completed results
if(nrow(completed_summary) == 0) {
  anova_findings <- "No ANOVA tests completed successfully"
} else {
  anova_findings <- completed_summary %>%
    filter(significance %in% c("***", "**", "*")) %>%
    slice_head(n = 5) %>%
    mutate(finding = paste0(variables, ": F=", f_value, ", p", p_value, " (", significance, ")")) %>%
    pull(finding)
  
  if(length(anova_findings) == 0) {
    anova_findings <- "No statistically significant ANOVA results found"
  }
}

# create engineered features summary
if(exists("engineered_anova_plan_final")) {
  engineered_completed <- engineered_anova_plan_final %>%
    filter(status == "Completed" & !is.na(f_value))
  
  if(nrow(engineered_completed) > 0) {
    top_engineered <- engineered_completed %>%
      arrange(desc(f_value)) %>%
      slice_head(n = 3) %>%
      mutate(finding = paste0(variables, ": F=", f_value, ", p", p_value, " (", significance, ")")) %>%
      pull(finding)
  } else {
    top_engineered <- "No engineered features tested successfully"
  }
} else {
  top_engineered <- "Engineered features ANOVA not completed"
}

# check missing values
if(exists("missing_summary") && nrow(missing_summary) > 0) {
  missing_count <- nrow(missing_summary)
} else {
  missing_count <- 0
}

# check other variables
if(exists("near_zero_var_names")) {
  nzv_count <- length(near_zero_var_names)
} else {
  nzv_count <- 0
}

if(exists("high_corr_pairs")) {
  corr_count <- length(high_corr_pairs)
} else {
  corr_count <- 0
}

# Count scaled features
scaled_features_count <- sum(str_detect(names(final_feature_data), "_scaled$"))

# Compile all key findings
final_summary_report <- list(
  "Dataset Overview" = paste0("Final dataset contains ", nrow(final_feature_data), " observations and ", 
                              ncol(final_feature_data), " features."),
  
  "ANOVA Key Findings" = if(is.character(anova_findings)) {
    anova_findings
  } else {
    paste(anova_findings, collapse = "; ")
  },
  
  "Top Engineered Features" = if(is.character(top_engineered)) {
    top_engineered
  } else {
    paste(top_engineered, collapse = "; ")
  },
  
  "Data Quality" = paste0("Missing values in ", missing_count, " features. ",
                          "Near-zero variance in ", nzv_count, " features. ",
                          "High correlation in ", corr_count, " feature pairs."),
  
  "Ready for Modeling" = paste0("Dataset saved as 'data/processed/engineered_features.csv' with ",
                                scaled_features_count, " scaled features ready for ML algorithms.")
)

# Convert to data frame for saving
final_report_df <- data.frame(
  section = names(final_summary_report),
  content = sapply(final_summary_report, function(x) {
    if(length(x) > 1) paste(x, collapse = "; ") else as.character(x)
  }),
  stringsAsFactors = FALSE
)

write_csv(final_report_df, "outputs/tables/feature_engineering_final_report.csv")

# Completion status checks
completion_status <- list(
  benchmark_merged = exists("completed_summary"),
  anova_completed = exists("completed_summary") && nrow(completed_summary) > 0,
  features_engineered = exists("feature_counts"),
  data_quality_checked = TRUE,
  files_saved = TRUE,
  dictionary_updated = exists("updated_dictionary")
)