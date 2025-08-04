# Feature Engineering Script - Script 02
# Author: Shelita Smith
# Date: July 23, 2025
# Purpose: Feature engineering with ANOVA analysis and comprehensive documentation, setup for 
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
regular_anova_plan <- read.csv("outputs/tables/regular_anova_analysis_plan.csv")
engineered_anova_plan <- read.csv("outputs/tables/engineered_features_anova_plan.csv")

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

# Create comprehensive results table
anova_results <- tibble(
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
  df = c(
    smoker_results$df, sex_results$df, age_results$df,
    bmi_results$df, region_results$df, child_results$df,
    smoker_sex_results$df, smoker_age_results$df, sex_age_results$df,
    bmi_smoker_results$df, bmi_sex_results$df, sex_child_results$df
  ),
  eta_squared = c(
    smoker_results$eta_squared, sex_results$eta_squared, age_results$eta_squared,
    bmi_results$eta_squared, region_results$eta_squared, child_results$eta_squared,
    smoker_sex_results$eta_squared, smoker_age_results$eta_squared, sex_age_results$eta_squared,
    bmi_smoker_results$eta_squared, bmi_sex_results$eta_squared, sex_child_results$eta_squared
  ),
  sig_level = c(
    smoker_results$sig_level, sex_results$sig_level, age_results$sig_level,
    bmi_results$sig_level, region_results$sig_level, child_results$sig_level,
    smoker_sex_results$sig_level, smoker_age_results$sig_level, sex_age_results$sig_level,
    bmi_smoker_results$sig_level, bmi_sex_results$sig_level, sex_child_results$sig_level
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

# Update the original regular_anova_plan with results
regular_anova_plan_updated <- regular_anova_plan %>%
  as_tibble() %>%  # Convert to tibble first
  slice(1:12) %>%

# Save reg ANOVA tables w/ results/values (overwriting original)
write_csv(regular_anova_plan_updated, "outputs/tables/regular_anova_analysis_plan.csv")

# Create summary of significant findings
significant_findings <- anova_results %>%
  filter(significant == TRUE) %>%
  arrange(desc(eta_squared)) %>%
  select(variables, f_value, p_value, eta_squared, effect_size_interpretation, sig_level)

# Create effect size ranking
effect_size_ranking <- anova_results %>%
  arrange(desc(eta_squared)) %>%
  mutate(rank = row_number()) %>%
  select(rank, variables, eta_squared, effect_size_interpretation, significant)

## 3.4 Add ANOVA results to data dictionary ####

add_anova_to_dictionary <- function() {
  
  # Add F-values
  add_to_dictionary("f_value", "numeric", "ANOVA Analysis Results", 
                    "F-statistic from ANOVA test - measures ratio of between-group to within-group variance",
                    paste0(min(anova_results$f_value), " to ", max(anova_results$f_value)), 0, 0,
                    "Higher F-values indicate stronger effects. Rounded to 3 decimal places.")
  
  # Add p-values  
  add_to_dictionary("p_value", "numeric", "ANOVA Analysis Results", 
                    "P-value from ANOVA test - probability of observing this result by chance",
                    paste0(min(anova_results$p_value), " to ", max(anova_results$p_value)), 0, 0,
                    "Values < 0.05 considered statistically significant. Rounded to 3 decimal places.")
  
  # Add degrees of freedom
  add_to_dictionary("df", "integer", "ANOVA Analysis Results", 
                    "Degrees of freedom for the effect being tested",
                    paste0(min(anova_results$df), " to ", max(anova_results$df)), 0, 0,
                    "Higher df indicates more categories in the factor being tested")
  
  # Add eta-squared
  add_to_dictionary("eta_squared", "numeric", "ANOVA Analysis Results", 
                    "Effect size measure - proportion of total variance explained by the factor",
                    paste0(min(anova_results$eta_squared), " to ", max(anova_results$eta_squared)), 0, 0,
                    "0.01=small, 0.06=medium, 0.14=large effect sizes. Rounded to 3 decimal places.")
  
  # Add significance level
  add_to_dictionary("sig_level", "character", "ANOVA Analysis Results", 
                    "Significance level notation using standard statistical symbols",
                    "*** (p<0.001), ** (p<0.01), * (p<0.05), . (p<0.1), '' (p>=0.1)", 0, 0,
                    "Standard statistical notation for significance levels")
  
  # Add significant flag
  add_to_dictionary("significant", "logical", "ANOVA Analysis Results", 
                    "Boolean indicator of statistical significance (p < 0.05)",
                    "TRUE, FALSE", 0, 0,
                    "TRUE indicates statistically significant result at alpha = 0.05")
  
  # Add effect size interpretation
  add_to_dictionary("effect_size_interpretation", "character", "ANOVA Analysis Results", 
                    "Qualitative interpretation of eta-squared effect size",
                    "Small, Medium, Large, Very Large", 0, 0,
                    "Based on Cohen's conventions: <0.01=Small, 0.01-0.06=Medium, 0.06-0.14=Large, >0.14=Very Large")
}

add_anova_to_dictionary()

# Create summary of significant findings
significant_findings <- anova_results %>%
  filter(significant == TRUE) %>%
  arrange(desc(eta_squared)) %>%
  select(variables, f_value, p_value, eta_squared, effect_size_interpretation, sig_level)

# Create effect size ranking
effect_size_ranking <- anova_results %>%
  arrange(desc(eta_squared)) %>%
  mutate(rank = row_number()) %>%
  select(rank, variables, eta_squared, effect_size_interpretation, significant)

# 4 Feature Engineering ####
## 4.1 Engineered features table for organization ####

engineered_features_table <- tibble(
  feature_id = 1:34,  
  feature_name = c(
    # STANDALONE FEATURES (Main effects from ANOVA - the "base calculators")
    "smoker_cost_multiplier", "sex_cost_premium", "bmi_risk_factor", 
    "region_cost_index", "age_cost_curve", "children_cost_factor",
    
    # INTERACTION FEATURES (Combined effects - the "complex calculators")
    "smoker_age_interaction", "smoker_sex_combo", "smoker_bmi_interaction",
    "smoker_region_interaction", "region_children_interaction", "has_children_age_interaction",
    
    # NON-LINEAR TRANSFORMATIONS (Curve fitting)
    "age_squared", "age_cubed", "bmi_squared", "bmi_log", "age_log",
    
    # POLYNOMIAL INTERACTIONS (Multi-variable curves)
    "age_bmi_interaction", "age_children_interaction", "bmi_children_interaction",
    
    # RISK SCORING FEATURES (Composite calculators)
    "health_risk_score", "demographic_risk_level", "compound_risk_score",
    
    # BENCHMARK COMPARISONS (Relative position calculators)
    "cost_vs_benchmark_ratio", "cost_deviation_category", "benchmark_outlier_flag",
    
    # FAMILY & LIFESTYLE (Behavioral calculators)
    "family_cost_efficiency", "large_family_flag", "cost_per_family_member",
    
    # BINNING FEATURES (Category-based calculators)
    "age_risk_bins", "bmi_clinical_categories",
    
    # ADVANCED STANDALONE FEATURES (Enhanced main effects)
    "smoker_years_estimate", "bmi_health_category", "regional_market_tier"
  ),
  feature_type = c(
    # Standalone main effects (6 features)
    rep("Standalone_Main_Effect", 6),
    # Statistical interactions (6 features)
    rep("Statistical_Interaction", 6),
    # Non-linear transforms (5 features)
    rep("Non_Linear_Transform", 5), 
    # Polynomial interactions (3 features)
    rep("Polynomial_Interaction", 3),
    # Risk scoring (3 features)
    rep("Risk_Score", 3),
    # Benchmark features (3 features)
    rep("Benchmark_Comparison", 3),
    # Family features (3 features)
    rep("Family_Lifestyle", 3),
    # Binning (2 features)
    rep("Categorical_Binning", 2),
    # Advanced standalone (3 features)
    rep("Advanced_Standalone", 3)
  ),
  based_on_anova = c(
    # Standalone features based on significant main effects (6)
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    # Interactions based on significant ANOVA (6) 
    TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    # Non-linear transformations (5)
    FALSE, FALSE, FALSE, FALSE, FALSE,
    # Polynomial interactions (3)
    FALSE, FALSE, FALSE,
    # Risk scoring (3)
    FALSE, FALSE, FALSE,
    # Benchmark features (3)
    FALSE, FALSE, FALSE,
    # Family features (3)
    FALSE, FALSE, FALSE,
    # Binning (2)
    FALSE, FALSE,
    # Advanced standalone (3)
    FALSE, FALSE, FALSE
  ),
  anova_priority = c(
    # Main effects priorities from your ANOVA (6)
    "Very_High", "Very_High", "Very_High", "Very_High", "High", "High",
    # Interactions (6)
    "Very_High", "Very_High", "Medium", "Medium", "Medium", "High",
    # Non-linear transformations (5)
    "Medium", "Medium", "Medium", "Medium", "Medium",
    # Polynomial interactions (3)
    "Medium", "Medium", "Medium",
    # Risk scoring (3)
    "Medium", "Medium", "Medium",
    # Benchmark features (3)
    "Medium", "Medium", "Medium",
    # Family features (3)
    "Medium", "Medium", "Medium",
    # Binning (2)
    "Medium", "Medium",
    # Advanced standalone (3)
    "Medium", "Medium", "Medium"
  ),
  description = c(
    # STANDALONE FEATURES - The "Base Calculators" (6)
    "Multiplier effect of being a smoker (e.g., 1.0 for non-smoker, 2.3 for smoker)",
    "Premium/discount for sex (captures actuarial differences)",
    "Risk factor based on BMI health categories (medical cost correlation)",
    "Regional cost index (captures geographic healthcare cost differences)", 
    "Age-based cost progression (captures lifecycle health cost patterns)",
    "Cost adjustment factor for number of children (family effect)",
    
    # INTERACTION FEATURES - The "Complex Calculators" (6)
    "Smoker status × age: older smokers cost exponentially more",
    "Smoker-sex combination: captures differential smoking health impacts",
    "Smoker status × BMI: compounds cardiovascular/diabetes risks",
    "Smoker-region combination: regional smoking health outcome patterns",
    "Region × children: regional family healthcare utilization patterns",
    "Having children × age: older parents have different healthcare needs",
    
    # NON-LINEAR TRANSFORMATIONS - The "Curve Fitters" (5)
    "Age squared: captures accelerating healthcare costs with age",
    "Age cubed: captures extreme elderly cost acceleration", 
    "BMI squared: captures exponential obesity health risks",
    "Log BMI: captures diminishing returns of BMI increases",
    "Log age: captures diminishing age effects in certain ranges",
    
    # POLYNOMIAL INTERACTIONS - The "Multi-Variable Curves" (3)
    "Age × BMI: joint effect of aging and weight on health costs",
    "Age × children: how having children affects costs at different life stages", 
    "BMI × children: weight-related health costs in parents vs non-parents",
    
    # RISK SCORING - The "Composite Calculators" (3)
    "Composite health risk score (0-10 scale) combining multiple risk factors",
    "Categorical risk level based on multiple demographic factors",
    "Weighted composite score using ANOVA effect sizes as weights",
    
    # BENCHMARK COMPARISONS - The "Relative Position Calculators" (3)
    "Individual's cost relative to national benchmark for their age group",
    "Categorical position relative to benchmark (low/normal/high cost)",
    "Flag for extreme outliers vs national benchmarks",
    
    # FAMILY & LIFESTYLE - The "Behavioral Calculators" (3)
    "Cost efficiency metric for families (economies/diseconomies of scale)",
    "Binary flag for large families (3+ children)",
    "Per-person cost within family unit",
    
    # BINNING FEATURES - The "Category-Based Calculators" (2)
    "Age grouped into actuarial risk categories",
    "BMI grouped by clinical/medical risk categories",
    
    # ADVANCED STANDALONE - The "Enhanced Base Calculators" (3)
    "Estimated smoking duration effect (age-dependent smoking impact)",
    "Clinical health category based on BMI with medical thresholds",
    "Healthcare market tier based on regional cost structures"
  ),
  what_it_calculates = c(
    # What each "calculator" actually computes (34 total)
    "Baseline cost multiplier: non-smoker=1.0, smoker=2.0-2.5x",
    "Sex-based adjustment: typically male costs ~10% more than female",
    "BMI health penalty: normal=1.0, overweight=1.15, obese=1.4x", 
    "Geographic cost adjustment: Northeast=1.2x, Southeast=0.9x baseline",
    "Age cost progression: linear increase ~$200-500 per year of age",
    "Family size effect: 0 kids=1.0x, 1-2 kids=1.1x, 3+ kids=0.95x",
    
    "Smoking age penalty: non-smoker age effect × 1.0, smoker age effect × 2.0+", 
    "Combined smoking-sex risk: smoker males have highest costs",
    "Compound health risk: smoking + obesity = exponential cost increase",
    "Regional smoking patterns: some regions have worse smoking outcomes",
    "Regional family costs: some regions more/less family-friendly healthcare",
    "Older parent premium: healthcare costs for 40+ parents with kids",
    
    "Quadratic age effect: captures exponential elderly cost increases",
    "Cubic age effect: captures extreme acceleration after 70+",
    "Quadratic BMI effect: captures obesity threshold effects", 
    "Logarithmic BMI effect: diminishing returns above certain BMI",
    "Logarithmic age effect: captures childhood vs adult cost differences",
    
    "Joint age-BMI risk: being old AND overweight compounds exponentially",
    "Age-children interaction: young parents vs older parents cost patterns",
    "Parent weight effect: overweight parents have different cost patterns",
    
    "0-10 composite score: 0=lowest risk, 10=highest risk patient",
    "Low/Medium/High/Very High categorical risk assignment",
    "Statistically-weighted risk score using your ANOVA effect sizes",
    
    "Ratio: individual_cost / national_average_for_age_group",
    "Categorical: Very Low (<0.5x), Low (0.5-0.8x), Normal (0.8-1.2x), etc.",
    "Binary flag: 1 if >2.5x national average or <0.3x national average",
    
    "Family efficiency: lower values = more efficient family healthcare",
    "Large family flag: 1 if 3+ children, 0 otherwise",
    "Individual cost ÷ family size (economies of scale detection)",
    
    "Age risk buckets: Young (18-25), Adult (26-45), Senior (46-65), Elder (65+)",
    "Clinical BMI: Underweight, Normal, Overweight, Obese Class 1/2/3",
    
    "Smoking duration estimate: age × smoking_status for cumulative effect",
    "Medical BMI categories with specific health risk thresholds",
    "Healthcare market classification: Premium/Standard/Economy regional markets"
  ),
  f_value = NA_real_,
  p_value = NA_real_, 
  eta_squared = NA_real_,
  significant = NA,
  created = FALSE
)

View(engineered_features_table)

## 4.2 Multipliers/ Coefficients creation and extraction ####
  ## ANOVA Group Means method

extract_multipliers <- function(data) {
  
  # Create multipliers table to store all results
  multipliers_table <- tibble(
    feature = character(),
    category = character(),
    baseline_cost = numeric(),
    group_cost = numeric(),
    multiplier = numeric(),
    sample_size = numeric(),
    statistical_basis = character()
  )
  
  # 1. SMOKER MULTIPLIERS (Highest ANOVA significance)
  smoker_stats <- data %>%
    group_by(smoker) %>%
    summarise(
      mean_cost = mean(charges, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  nonsmoker_baseline <- smoker_stats$mean_cost[smoker_stats$smoker == "no"]
  smoker_multiplier <- smoker_stats$mean_cost[smoker_stats$smoker == "yes"] / nonsmoker_baseline
  
  multipliers_table <- multipliers_table %>%
    bind_rows(
      tibble(
        feature = "smoker",
        category = c("no", "yes"),
        baseline_cost = nonsmoker_baseline,
        group_cost = smoker_stats$mean_cost,
        multiplier = c(1.0, smoker_multiplier),
        sample_size = smoker_stats$n,
        statistical_basis = "ANOVA group means"
      )
    )
  
  # 2. SEX MULTIPLIERS (Very high ANOVA significance)
  sex_stats <- data %>%
    group_by(sex) %>%
    summarise(
      mean_cost = mean(charges, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  female_baseline <- sex_stats$mean_cost[sex_stats$sex == "female"]
  male_multiplier <- sex_stats$mean_cost[sex_stats$sex == "male"] / female_baseline
  
  multipliers_table <- multipliers_table %>%
    bind_rows(
      tibble(
        feature = "sex",
        category = c("female", "male"),
        baseline_cost = female_baseline,
        group_cost = sex_stats$mean_cost,
        multiplier = c(1.0, male_multiplier),
        sample_size = sex_stats$n,
        statistical_basis = "ANOVA group means"
      )
    )
  
  # 3. BMI CATEGORY MULTIPLIERS (Very high ANOVA significance)
  bmi_stats <- data %>%
    group_by(bmi_category) %>%
    summarise(
      mean_cost = mean(charges, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(mean_cost)  # Order by cost for logical baseline
  
  bmi_baseline <- min(bmi_stats$mean_cost)  # Lowest cost category as baseline
  
  multipliers_table <- multipliers_table %>%
    bind_rows(
      tibble(
        feature = "bmi_category",
        category = bmi_stats$bmi_category,
        baseline_cost = bmi_baseline,
        group_cost = bmi_stats$mean_cost,
        multiplier = bmi_stats$mean_cost / bmi_baseline,
        sample_size = bmi_stats$n,
        statistical_basis = "ANOVA group means"
      )
    )
  
  # 4. REGION MULTIPLIERS (Very high ANOVA significance)
  region_stats <- data %>%
    group_by(region) %>%
    summarise(
      mean_cost = mean(charges, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(mean_cost)
  
  region_baseline <- min(region_stats$mean_cost)  # lowest cost region as baseline
  
  multipliers_table <- multipliers_table %>%
    bind_rows(
      tibble(
        feature = "region",
        category = region_stats$region,
        baseline_cost = region_baseline,
        group_cost = region_stats$mean_cost,
        multiplier = region_stats$mean_cost / region_baseline,
        sample_size = region_stats$n,
        statistical_basis = "ANOVA group means"
      )
    )
  
  # 5. HAS_CHILDREN MULTIPLIERS (High ANOVA significance)
  children_stats <- data %>%
    mutate(has_children = if_else(children > 0, "yes", "no")) %>%
    group_by(has_children) %>%
    summarise(
      mean_cost = mean(charges, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  no_children_baseline <- children_stats$mean_cost[children_stats$has_children == "no"]
  children_multiplier <- children_stats$mean_cost[children_stats$has_children == "yes"] / no_children_baseline
  
  multipliers_table <- multipliers_table %>%
    bind_rows(
      tibble(
        feature = "has_children",
        category = c("no", "yes"),
        baseline_cost = no_children_baseline,
        group_cost = children_stats$mean_cost,
        multiplier = c(1.0, children_multiplier),
        sample_size = children_stats$n,
        statistical_basis = "ANOVA group means"
      )
    )
  
  # 6. AGE MULTIPLIERS (High ANOVA significance) - Using age groups
  age_stats <- data %>%
    group_by(age_group_standard) %>%
    summarise(
      mean_cost = mean(charges, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(mean_cost)
  
  age_baseline <- min(age_stats$mean_cost)  # Youngest group as baseline
  
  multipliers_table <- multipliers_table %>%
    bind_rows(
      tibble(
        feature = "age_group_standard",
        category = age_stats$age_group_standard,
        baseline_cost = age_baseline,
        group_cost = age_stats$mean_cost,
        multiplier = age_stats$mean_cost / age_baseline,
        sample_size = age_stats$n,
        statistical_basis = "ANOVA group means"
      )
    )
  
  return(multipliers_table)
}

# Step 3: Generate Multipliers Table
multipliers_table <- extract_multipliers(insurance_with_benchmarks)

# Save multipliers for future reference
write_csv(multipliers_table, "outputs/tables/feature_multipliers.csv")


## 4.3 Update Engineered features table ####

engineered_features_table <- tibble(
  feature_id = 1:32,  
  
  feature_name = c(
    # Standalone Features (multipliers) - 6 features
    "smoker_cost_multiplier", "sex_cost_premium", "bmi_risk_factor", 
    "region_cost_index", "has_children_factor", "age_cost_curve",
    
    # Non-linear/Polynomial features - 5 features
    "age_squared", "bmi_squared", "age_cubed", "age_log", "bmi_log",
    
    # Risk Scores - 3 features
    "health_risk_score", "demographic_risk_level", "compound_risk_score",
    
    # High/Medium Priority Interactions - 9 features
    "smoker_age_interaction", "smoker_sex_combo", "smoker_bmi_interaction",
    "region_children_interaction", "has_children_age_interaction", "region_cost_multiplier",
    "smoker_region_combo", "sex_bmi_interaction", "age_region_interaction",
    
    # Encoded features - 4 features
    "smoker_encoded", "sex_encoded", "region_encoded", "bmi_category_encoded",
    
    # Binned Features - 2 features
    "age_bins", "charges_percentile_rank",
    
    # Advanced Standalone - 3 features
    "smoker_years_estimate", "bmi_health_category", "regional_market_tier"
  ),
  
  feature_type = c(
    # Standalone Features (6)
    rep("Standalone_Main_Effect", 6),
    # Non-linear (5)
    rep("Non_Linear_Transform", 5),
    # Risk scores (3)
    rep("Risk_Score", 3),
    # Interactions (9)
    rep("Statistical_Interaction", 9),
    # Encodings (4)
    rep("Categorical_Encoding", 4),
    # Binning (2)
    rep("Categorical_Binning", 2),
    # Advanced (3)
    rep("Advanced_Standalone", 3)
  ),
  
  based_on_anova = c(
    # Standalone - all based on ANOVA (6)
    rep(TRUE, 6),            
    # Non-linear - not directly from ANOVA (5)
    rep(FALSE, 5),           
    # Risk scores - composite features (3)
    rep(FALSE, 3),           
    # Interactions - mixed ANOVA basis (9)
    c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),  
    # Encodings - not ANOVA based (4)
    rep(FALSE, 4),           
    # Binning - not ANOVA based (2)
    rep(FALSE, 2),           
    # Advanced - not ANOVA based (3)
    rep(FALSE, 3)            
  ),
  
  anova_priority = c(
    # Standalone - high priority from ANOVA results (6)
    "Very_High", "Very_High", "Very_High", "Very_High", "High", "High",  
    # Non-linear - medium priority (5)
    rep("Medium", 5),          
    # Risk scores - medium priority (3)
    rep("Medium", 3),          
    # Interactions - mixed priority (9)
    c("Very_High", "Very_High", "Medium", "Medium", "Medium", "High", "Medium", "Medium", "Medium"),  
    # Encodings - medium priority (4)
    rep("Medium", 4),          
    # Binning - medium priority (2)
    rep("Medium", 2),          
    # Advanced - medium priority (3)
    rep("Medium", 3)           
  ),
  
  uses_multipliers = c(
    # Standalone - all use multipliers directly (6)
    rep(TRUE, 6),
    # Non-linear - don't use multipliers (5)
    rep(FALSE, 5),
    # Risk scores - use multipliers in calculations (3)
    rep(TRUE, 3),
    # Interactions - most use multipliers (9)
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    # Encodings - don't use multipliers (4)
    rep(FALSE, 4),
    # Binning - don't use multipliers (2)
    rep(FALSE, 2),
    # Advanced - don't use multipliers directly (3)
    rep(FALSE, 3)
  ),
  
  multiplier_source = c(
    # Standalone - specify which multiplier table (6)
    "smoker_mult", "sex_mult", "bmi_mult", "region_mult", "children_mult", "age_mult",
    # Non-linear - no multipliers (5)
    rep(NA_character_, 5),
    # Risk scores - multiple multipliers (3)
    "smoker_mult + bmi_mult + age_mult", "health_risk_score", "smoker_mult * sex_mult * bmi_mult",
    # Interactions - combination of multipliers (9)
    "smoker_mult * age_mult", "smoker_mult * sex_mult", "smoker_mult * bmi_mult",
    "region_mult * children_mult", "children_mult * age_mult", "region_mult",
    "smoker_mult * region_mult", "sex_mult * bmi_mult", "age_mult * region_mult",
    # Encodings - no multipliers (4)
    rep(NA_character_, 4),
    # Binning - no multipliers (2)
    rep(NA_character_, 2),
    # Advanced - no direct multipliers (3)
    rep(NA_character_, 3)
  ),
  
  description = c(
    # Standalone (6)
    "Direct smoker cost multiplier from ANOVA group means (non-smoker=1.0)",
    "Sex-based cost premium using male/female ratio from group means", 
    "BMI category risk factor from ANOVA group cost analysis",
    "Regional cost index using cheapest region as baseline from ANOVA",
    "Children factor using no-children as baseline from group means",
    "Age group cost curve using youngest group as baseline",
    
    # Non-linear (5)
    "Age squared for capturing non-linear age effects on healthcare costs",
    "BMI squared for capturing obesity threshold effects and exponential risks", 
    "Age cubed for capturing extreme aging effects in elderly populations",
    "Log-transformed age to capture diminishing age effects",
    "Log-transformed BMI for improved distribution and curve fitting",
    
    # Risk scores (3)
    "Composite 0-10 health risk score using ANOVA-derived multiplier weights",
    "Categorical version of health risk score (Low/Medium/High/Very High)",
    "ANOVA effect-size weighted compound risk using multiple multipliers",
    
    # Interactions (9)
    "Smoker effect amplified by age using both ANOVA-derived multipliers",
    "Smoker-sex combination using joint multipliers from group means",
    "Smoker-BMI interaction using combined risk factors and multipliers",
    "Regional differences in family healthcare costs using multipliers",
    "Children effect varies by age group using combined multipliers",
    "Region-specific cost multipliers (alias for region_cost_index)",
    "Smoker effects vary by region using combined multipliers",
    "Sex-BMI interaction effects using both multipliers", 
    "Age effects vary by region using combined multipliers",
    
    # Encodings (4)
    "Binary encoding of smoker status (0/1)",
    "Binary encoding of sex (0=female, 1=male)",
    "One-hot encoding of region categories", 
    "Ordinal encoding of BMI categories",
    
    # Binning (2)
    "Age grouped into 5-year bins for risk modeling",
    "Charges converted to percentile ranks (0-1 scale)",
    
    # Advanced (3)
    "Estimated smoking duration effect using age and smoking interaction",
    "Medical BMI risk category with clinical thresholds",
    "Healthcare market tier classification by regional cost patterns"
  ),
  
  calculation_method = c(
    # Standalone (6)
    "Direct lookup from smoker multipliers table",
    "Direct lookup from sex multipliers table",
    "Direct lookup from BMI category multipliers table",
    "Direct lookup from region multipliers table",
    "Direct lookup from children multipliers table",
    "Direct lookup from age group multipliers table",
    
    # Non-linear (5)
    "age^2",
    "bmi^2",
    "age^3",
    "log(age)",
    "log(bmi)",
    
    # Risk scores (3)
    "(smoker_mult * 4) + (bmi_mult * 3) + (age_mult * 3)",
    "case_when logic based on health_risk_score thresholds",
    "smoker_mult * sex_mult * bmi_mult",
    
    # Interactions (9)
    "smoker_cost_multiplier * age_cost_curve",
    "smoker_cost_multiplier * sex_cost_premium",
    "smoker_cost_multiplier * bmi_risk_factor",
    "region_cost_index * has_children_factor",
    "has_children_factor * age_cost_curve",
    "region_cost_index (direct copy)",
    "smoker_cost_multiplier * region_cost_index",
    "sex_cost_premium * bmi_risk_factor",
    "age_cost_curve * region_cost_index",
    
    # Encodings (4)
    "as.numeric(smoker == 'yes')",
    "as.numeric(sex == 'male')",
    "model.matrix approach or factor conversion",
    "as.numeric(as.factor(bmi_category))",
    
    # Binning (2)
    "cut(age, breaks = seq(15, 70, by = 5))",
    "percent_rank(charges)",
    
    # Advanced (3)
    "smoker_encoded * age (proxy for smoking duration)",
    "Clinical BMI categorization with medical thresholds",
    "Regional cost tier assignment based on multiplier ranges"
  ),
  
  # Initialize tracking columns
  f_value = NA_real_,
  p_value = NA_real_,
  eta_squared = NA_real_,
  significant = NA,
  created = FALSE
)

# Save to file
write_csv(engineered_features_table, "outputs/tables/engineered_features_tracking.csv")

## 4.4 Engineered Features + Simultaneous Encoding ####

create_engineered_features <- function(data, multipliers) {
  
  # Convert multipliers to named vectors for easy lookup
  smoker_mult <- multipliers %>% 
    filter(feature == "smoker") %>% 
    select(category, multiplier) %>%
    deframe()
  
  sex_mult <- multipliers %>% 
    filter(feature == "sex") %>% 
    select(category, multiplier) %>%
    deframe()
  
  bmi_mult <- multipliers %>% 
    filter(feature == "bmi_category") %>% 
    select(category, multiplier) %>%
    deframe()
  
  region_mult <- multipliers %>% 
    filter(feature == "region") %>% 
    select(category, multiplier) %>%
    deframe()
  
  children_mult <- multipliers %>% 
    filter(feature == "has_children") %>% 
    select(category, multiplier) %>%
    deframe()
  
  age_mult <- multipliers %>% 
    filter(feature == "age_group_standard") %>% 
    select(category, multiplier) %>%
    deframe()
  
  # Apply multipliers to create engineered features
  data_engineered <- data %>%
    mutate(
      # Standalone Multiplier Features
      smoker_cost_multiplier = smoker_mult[smoker],
      sex_cost_premium = sex_mult[sex],
      bmi_risk_factor = bmi_mult[bmi_category],
      region_cost_index = region_mult[region],
      has_children_factor = children_mult[if_else(children > 0, "yes", "no")],
      age_cost_curve = age_mult[age_group_standard],
      
      # Non-linear transformation/ Polynomial Features
      age_squared = age^2,
      bmi_squared = bmi^2, 
      age_cubed = age^3,
      age_log = log(age),
      bmi_log = log(bmi),
      
      # Risk Score Features (multiplier-weighted)
      health_risk_score = (smoker_cost_multiplier * 4) + 
        (bmi_risk_factor * 3) + 
        (age_cost_curve * 3),
      demographic_risk_level = case_when(
        health_risk_score <= 3 ~ "Low",
        health_risk_score <= 6 ~ "Medium", 
        health_risk_score <= 9 ~ "High",
        TRUE ~ "Very_High"
      ),
      compound_risk_score = smoker_cost_multiplier * sex_cost_premium * bmi_risk_factor,
      
      # High-Priority Interactions (using multipliers)
      smoker_age_interaction = smoker_cost_multiplier * age_cost_curve,
      smoker_sex_combo = smoker_cost_multiplier * sex_cost_premium,
      smoker_bmi_interaction = smoker_cost_multiplier * bmi_risk_factor,
      
      # Medium-Priority Interactions  
      region_children_interaction = region_cost_index * has_children_factor,
      has_children_age_interaction = has_children_factor * age_cost_curve,
      region_cost_multiplier = region_cost_index,  # Alias for clarity
      
      # Lower-Priority Interactions
      smoker_region_combo = smoker_cost_multiplier * region_cost_index,
      sex_bmi_interaction = sex_cost_premium * bmi_risk_factor,
      age_region_interaction = age_cost_curve * region_cost_index,
      
      # Categorical Encodings - CORRECTED
      smoker_encoded = as.numeric(smoker == "yes"),
      sex_encoded = as.numeric(sex == "male"),
      bmi_category_encoded = as.numeric(as.factor(bmi_category)),
      
      # Region Encoding - ADDED (One-hot encoding)
      region_northeast = as.numeric(region == "northeast"),
      region_northwest = as.numeric(region == "northwest"), 
      region_southeast = as.numeric(region == "southeast"),
      region_southwest = as.numeric(region == "southwest"),
      
      # Alternative: Single ordinal encoding for region (by cost level)
      region_encoded = case_when(
        region == "southeast" ~ 1,    # Lowest cost region
        region == "southwest" ~ 2,
        region == "northwest" ~ 3, 
        region == "northeast" ~ 4     # Highest cost region
      ),
      
      # Binned Features
      age_bins = cut(age, breaks = seq(15, 70, by = 5), include.lowest = TRUE),
      charges_percentile_rank = percent_rank(charges)
    )
  
  return(data_engineered)
}

insurance_with_engineered_features <- create_engineered_features(insurance_with_benchmarks, multipliers_table)

write_csv(insurance_with_engineered_features, "outputs/tables/insurance_with_engineered_features.csv")


## 4.5 Basic cost comparisons ####

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

cost_by_sex <- insurance_with_engineered_features %>%
  group_by(sex) %>%
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

cost_by_bmi_category <- insurance_with_engineered_features %>%
  group_by(bmi_category) %>%
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

cost_by_children <- insurance_with_engineered_features %>%
  mutate(has_children = if_else(children > 0, "Has Children", "No Children")) %>%
  group_by(has_children) %>%
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

# 3. Cross-Category Comparisons (Key Interactions)
smoker_sex_costs <- insurance_with_engineered_features %>%
  group_by(smoker, sex) %>%
  summarise(
    mean_cost = mean(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cost))

smoker_bmi_costs <- insurance_with_engineered_features %>%
  group_by(smoker, bmi_category) %>%
  summarise(
    mean_cost = mean(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cost))

region_smoker_costs <- insurance_with_engineered_features %>%
  group_by(region, smoker) %>%
  summarise(
    mean_cost = mean(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cost))

# 4. Age Group Analysis
cost_by_age_group <- insurance_with_engineered_features %>%
  group_by(age_group_standard) %>%
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

# 5. Multiplier Validation Check
multiplier_validation <- tibble(
  category = c("Smoker", "Sex", "BMI", "Region", "Children", "Age"),
  observed_max_ratio = c(
    max(cost_by_smoker$cost_ratio),
    max(cost_by_sex$cost_ratio), 
    max(cost_by_bmi_category$cost_ratio),
    max(cost_by_region$cost_ratio),
    max(cost_by_children$cost_ratio),
    max(cost_by_age_group$cost_ratio)
  ),
  multiplier_max_ratio = c(
    max(multipliers_table$multiplier[multipliers_table$feature == "smoker"]),
    max(multipliers_table$multiplier[multipliers_table$feature == "sex"]),
    max(multipliers_table$multiplier[multipliers_table$feature == "bmi_category"]),
    max(multipliers_table$multiplier[multipliers_table$feature == "region"]),
    max(multipliers_table$multiplier[multipliers_table$feature == "has_children"]),
    max(multipliers_table$multiplier[multipliers_table$feature == "age_group_standard"])
  )
) %>%
  mutate(
    difference = abs(observed_max_ratio - multiplier_max_ratio),
    match_quality = case_when(
      difference < 0.01 ~ "Perfect",
      difference < 0.05 ~ "Excellent", 
      difference < 0.10 ~ "Good",
      TRUE ~ "Needs Review"
    )
  )

# 6. Extreme Values Analysis
high_cost_analysis <- insurance_with_engineered_features %>%
  filter(charges > quantile(charges, 0.9)) %>%
  group_by(smoker, sex, bmi_category) %>%
  summarise(
    mean_cost = mean(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cost))

low_cost_analysis <- insurance_with_engineered_features %>%
  filter(charges < quantile(charges, 0.1)) %>%
  group_by(smoker, sex, bmi_category) %>%
  summarise(
    mean_cost = mean(charges),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(mean_cost)

# Combine all results into summary
basic_cost_comparisons <- list(
  overall_summary = basic_cost_summary,
  by_smoker = cost_by_smoker,
  by_sex = cost_by_sex,
  by_region = cost_by_region,
  by_bmi = cost_by_bmi_category,
  by_children = cost_by_children,
  by_age_group = cost_by_age_group,
  smoker_sex_cross = smoker_sex_costs,
  smoker_bmi_cross = smoker_bmi_costs,
  region_smoker_cross = region_smoker_costs,
  multiplier_validation = multiplier_validation,
  high_cost_profiles = high_cost_analysis,
  low_cost_profiles = low_cost_analysis
)

write_csv(cost_by_smoker, "outputs/tables/cost_comparison_smoker.csv")
write_csv(cost_by_region, "outputs/tables/cost_comparison_region.csv") 
write_csv(cost_by_bmi_category, "outputs/tables/cost_comparison_bmi.csv")
write_csv(multiplier_validation, "outputs/tables/multiplier_validation.csv")
write_csv(smoker_sex_costs, "outputs/tables/cost_comparison_smoker_sex.csv")

# Return the summary for inspection
basic_cost_comparisons

# 5 Advanced/Missing ANOVA for FE ####

engineered_tests <- list(
  # Standalone multiplier features
  smoker_cost_multiplier = aov(charges ~ smoker_cost_multiplier, data = insurance_with_engineered_features),
  sex_cost_premium = aov(charges ~ sex_cost_premium, data = insurance_with_engineered_features),
  bmi_risk_factor = aov(charges ~ bmi_risk_factor, data = insurance_with_engineered_features),
  region_cost_index = aov(charges ~ region_cost_index, data = insurance_with_engineered_features),
  has_children_factor = aov(charges ~ has_children_factor, data = insurance_with_engineered_features),
  age_cost_curve = aov(charges ~ age_cost_curve, data = insurance_with_engineered_features),
  
  # Non-linear features (use lm for continuous)
  age_squared = lm(charges ~ age_squared, data = insurance_with_engineered_features),
  bmi_squared = lm(charges ~ bmi_squared, data = insurance_with_engineered_features),
  age_cubed = lm(charges ~ age_cubed, data = insurance_with_engineered_features),
  
  # Risk scores (use lm for continuous)
  health_risk_score = lm(charges ~ health_risk_score, data = insurance_with_engineered_features),
  compound_risk_score = lm(charges ~ compound_risk_score, data = insurance_with_engineered_features),
  
  # Interaction features
  smoker_age_interaction = aov(charges ~ smoker_age_interaction, data = insurance_with_engineered_features),
  smoker_sex_combo = aov(charges ~ smoker_sex_combo, data = insurance_with_engineered_features),
  smoker_bmi_interaction = aov(charges ~ smoker_bmi_interaction, data = insurance_with_engineered_features),
  region_children_interaction = aov(charges ~ region_children_interaction, data = insurance_with_engineered_features),
  has_children_age_interaction = aov(charges ~ has_children_age_interaction, data = insurance_with_engineered_features),
  smoker_region_combo = aov(charges ~ smoker_region_combo, data = insurance_with_engineered_features),
  sex_bmi_interaction = aov(charges ~ sex_bmi_interaction, data = insurance_with_engineered_features),
  age_region_interaction = aov(charges ~ age_region_interaction, data = insurance_with_engineered_features),
  
  # Encoded features
  smoker_encoded = aov(charges ~ smoker_encoded, data = insurance_with_engineered_features),
  sex_encoded = aov(charges ~ sex_encoded, data = insurance_with_engineered_features),
  
  # Binned/continuous features
  charges_percentile_rank = lm(charges ~ charges_percentile_rank, data = insurance_with_engineered_features)
)

# Function to extract statistics from both aov and lm objects
extract_unified_stats <- function(model) {
  if (inherits(model, "aov")) {
    # Handle ANOVA objects
    summary_result <- summary(model)
    f_value <- summary_result[[1]]$`F value`[1]
    p_value <- summary_result[[1]]$`Pr(>F)`[1]
    
    # Calculate eta-squared for ANOVA
    ss_effect <- summary_result[[1]]$`Sum Sq`[1]
    ss_total <- sum(summary_result[[1]]$`Sum Sq`)
    eta_squared <- ss_effect / ss_total
    
  } else if (inherits(model, "lm")) {
    # Handle linear model objects
    anova_result <- anova(model)
    f_value <- anova_result$`F value`[1]
    p_value <- anova_result$`Pr(>F)`[1]
    
    # Calculate eta-squared for lm
    ss_effect <- anova_result$`Sum Sq`[1]
    ss_total <- sum(anova_result$`Sum Sq`)
    eta_squared <- ss_effect / ss_total
  }
  
  # Determine significance and effect size
  significant <- p_value < 0.05
  effect_size_interpretation <- case_when(
    eta_squared >= 0.14 ~ "Large",
    eta_squared >= 0.06 ~ "Medium", 
    eta_squared >= 0.01 ~ "Small",
    TRUE ~ "Negligible"
  )
  
  return(list(
    f_value = f_value,
    p_value = p_value,
    eta_squared = eta_squared,
    significant = significant,
    effect_size_interpretation = effect_size_interpretation
  ))
}

# Process the engineered tests to create engineered_anova_results
engineered_anova_results <- map_dfr(names(engineered_tests), function(test_name) {
  model <- engineered_tests[[test_name]]
  result <- extract_unified_stats(model)
  result$feature_name <- test_name
  return(as_tibble(result))
})

# Update engineered features table with ANOVA results  
engineered_features_table_updated <- engineered_features_table %>%
  select(-f_value, -p_value, -eta_squared, -significant) %>%  # Remove existing columns to avoid duplicates
  left_join(
    engineered_anova_results %>% 
      select(feature_name, f_value, p_value, eta_squared, significant),
    by = "feature_name"
  ) %>%
  mutate(
    created = feature_name %in% names(insurance_with_engineered_features)
  ) %>%
  arrange(desc(coalesce(eta_squared, 0)))  # Use coalesce to handle NAs

write_csv(engineered_anova_results, "outputs/tables/engineered_features_anova_results.csv")
write_csv(engineered_features_table_updated, "outputs/tables/engineered_features_table.csv")

engineered_anova_results

## 5.1 Missing ANOVA Tests arggghhh ####

insurance_with_engineered_features <- read_csv("outputs/tables/insurance_with_engineered_features.csv")
existing_anova_results <- read_csv("outputs/tables/engineered_features_anova_results.csv")
engineered_features_table <- read_csv("outputs/tables/engineered_features_table.csv")

# Function to determine appropriate statistical test and extract results
perform_feature_test <- function(data, feature_name, target_var = "charges") {
  
  # Validate feature existence
  if (!feature_name %in% colnames(data)) {
    warning(paste("Feature", feature_name, "not found in dataset"))
    return(NULL)
  }
  
  feature_data <- data[[feature_name]]
  target_data <- data[[target_var]]
  
  # Skip features with only one category (from previous error)
  if (length(unique(feature_data)) <= 1) {
    warning(paste("Feature", feature_name, "has <=1 unique value. Skipping test."))
    return(NULL)
  }
  
  # Determine appropriate statistical test
  if (is.numeric(feature_data)) {
    # Continuous variables: Linear Regression
    model <- lm(target_data ~ feature_data)
    model_summary <- summary(model)
    anova_result <- anova(model)
    
    f_value <- anova_result$`F value`[1]
    p_value <- anova_result$`Pr(>F)`[1]
    effect_size <- model_summary$r.squared  # R-squared for linear regression
    test_type <- "Linear Regression"
    
  } else {
    # Categorical variables: ANOVA
    if (length(unique(feature_data)) <= 10) {
      # Direct ANOVA for reasonable number of categories
      model <- aov(target_data ~ feature_data)
      anova_summary <- summary(model)
      
      f_value <- anova_summary[[1]]$`F value`[1]
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      # Eta-squared for ANOVA
      ss_between <- anova_summary[[1]]$`Sum Sq`[1]
      ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
      effect_size <- ss_between / ss_total
      test_type <- "ANOVA"
      
    } else {
      # Too many categories: bin and use ANOVA
      feature_binned <- cut(as.numeric(as.factor(feature_data)), 
                            breaks = 5, labels = FALSE)
      model <- aov(target_data ~ factor(feature_binned))
      anova_summary <- summary(model)
      
      f_value <- anova_summary[[1]]$`F value`[1]
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      ss_between <- anova_summary[[1]]$`Sum Sq`[1]
      ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
      effect_size <- ss_between / ss_total
      test_type <- "ANOVA (Binned)"
    }
  }
  
  # Effect size interpretation
  effect_size_interpretation <- case_when(
    effect_size >= 0.14 ~ "Large",
    effect_size >= 0.06 ~ "Medium", 
    effect_size >= 0.01 ~ "Small",
    TRUE ~ "Negligible"
  )
  
  return(tibble(
    feature_name = feature_name,
    test_type = test_type,
    f_value = f_value,
    p_value = p_value,
    eta_squared = effect_size,
    effect_size_interpretation = effect_size_interpretation,
    significant = p_value < 0.05,
    highly_significant = p_value < 0.001
  ))
}

## Test missing features

missing_features <- c(
  "demographic_risk_level",
  "age_bins",
  "region_encoded", 
  "bmi_category_encoded",
  "age_log",
  "bmi_log"
)

missing_feature_results <- map_dfr(missing_features, 
                                   ~perform_feature_test(insurance_with_engineered_features, .x))

## 5.2 Update Tables ####
### 5.2.1 Update ANOVA Results Table ####

# Combine existing and new results
updated_anova_results <- bind_rows(
  existing_anova_results,
  missing_feature_results %>% select(-test_type)  # Remove test_type to match existing structure
) %>%
  distinct(feature_name, .keep_all = TRUE) %>%  # Remove any duplicates
  arrange(desc(eta_squared))

# Save updated ANOVA results
write_csv(updated_anova_results, "outputs/tables/engineered_features_anova_results.csv")

### 5.2.2 Update FE Tracking Tables ####

updated_features_table <- engineered_features_table %>%
  left_join(
    updated_anova_results %>% 
      select(feature_name, f_value, p_value, eta_squared, significant),
    by = "feature_name"
  ) %>%
  mutate(
    # Update statistical columns - use new values where available, keep existing otherwise
    f_value = coalesce(f_value.y, f_value.x),
    p_value = coalesce(p_value.y, p_value.x),
    eta_squared = coalesce(eta_squared.y, eta_squared.x),
    significant = coalesce(significant.y, significant.x),
    
    # Update creation status
    created = feature_name %in% names(insurance_with_engineered_features)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%  # Clean up join artifacts (yay, yay, yay!!)
  arrange(desc(coalesce(eta_squared, 0)))

# Save updated features table
write_csv(updated_features_table, "outputs/tables/engineered_features_table.csv")

## 5.3 Analysis Summary ####

# Summary of all feature analysis results
complete_feature_analysis <- updated_anova_results %>%
  mutate(
    test_method = case_when(
      feature_name %in% missing_features ~ "Missing Feature Test",
      TRUE ~ "Original ANOVA Analysis"
    )
  ) %>%
  select(feature_name, f_value, p_value, eta_squared, effect_size_interpretation, 
         significant, highly_significant, test_method) %>%
  arrange(desc(eta_squared))

write_csv(complete_feature_analysis, "outputs/tables/complete_feature_analysis_results.csv")

# Summary statistics for feature engineering completion
feature_summary <- tibble(
  total_features_planned = nrow(updated_features_table),
  features_created = sum(updated_features_table$created, na.rm = TRUE),
  features_tested = sum(!is.na(updated_features_table$eta_squared)),
  significant_features = sum(updated_features_table$significant, na.rm = TRUE),
  large_effect_features = sum(updated_features_table$eta_squared >= 0.14, na.rm = TRUE),
  medium_effect_features = sum(updated_features_table$eta_squared >= 0.06 & 
                                 updated_features_table$eta_squared < 0.14, na.rm = TRUE),
  completion_rate = round(features_created / total_features_planned * 100, 1)
)

write_csv(feature_summary, "outputs/tables/feature_engineering_completion_summary.csv")

## 5.4 Analysis Notes ####
### Second round of ANOVA analysis and FE didn't result in more large effect-size engineered features; did result in a few more medium effect size features - adding those to the insurance w/ engineered feature table and then going to proceed to encoding, normalization, & scaling

# 6 Advanced FE ####
## Mostly unnecessary (see 5.4), continuity/ organization

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
    
    # Regional Market Adjustment (Healthcare Market Tiers)
    regional_market_adjustment = case_when(
      region == "southeast" ~ 0.85,
      region == "southwest" ~ 0.92,
      region == "northwest" ~ 1.08,
      region == "northeast" ~ 1.15,
      TRUE ~ 1.0
    ),
    
    # Age Cost Acceleration (Exponential Healthcare Costs)
    age_cost_acceleration = case_when(
      age < 25 ~ age^1.5,
      age < 40 ~ age^2.0,
      age < 55 ~ age^2.5,
      age >= 55 ~ age^3.0
    ),
    
    # BMI Threshold Effects (Obesity Tipping Points)
    bmi_threshold_effects = case_when(
      bmi < 18.5 ~ (18.5 - bmi)^2 * 1.2,
      bmi >= 18.5 & bmi < 25 ~ 1.0,
      bmi >= 25 & bmi < 30 ~ (bmi - 25)^1.5 * 0.8,
      bmi >= 30 & bmi < 35 ~ (bmi - 25)^2.0 * 1.5,
      bmi >= 35 & bmi < 40 ~ (bmi - 25)^2.5 * 2.0,
      bmi >= 40 ~ (bmi - 25)^3.0 * 2.5
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
  ) %>%
  
  # Cross-domain interactions and composite scores
  mutate(
    smoker_metabolic_interaction = smoker_age_severity_index * metabolic_syndrome_risk,
    age_bmi_compound_risk = age_cost_acceleration * bmi_threshold_effects,
    regional_lifestyle_adjustment = regional_market_adjustment * compound_lifestyle_risk,
    family_risk_optimization = family_cost_optimization * compound_lifestyle_risk,
    
    comprehensive_health_risk_score = (
      smoker_age_severity_index * 0.4 +
        metabolic_syndrome_risk * 0.3 +
        compound_lifestyle_risk * 0.2 +
        regional_market_adjustment * 0.1
    ),
    
    healthcare_utilization_predictor = (
      age_cost_acceleration * 0.35 +
        smoker_age_severity_index * 0.30 +
        metabolic_syndrome_risk * 0.20 +
        bmi_threshold_effects * 0.15
    ),
    
    market_competitiveness_index = individual_vs_cohort_ratio * regional_market_adjustment,
    value_based_care_score = cost_efficiency_quintiles / compound_lifestyle_risk
  )

write_csv(insurance_advanced, "data/processed/insurance_advanced_features.csv")

## 6.1 Validate Advanced Features ####

new_advanced_features <- c(
  "smoker_age_severity_index", "metabolic_syndrome_risk", 
  "family_cost_optimization", "regional_market_adjustment",
  "age_cost_acceleration", "bmi_threshold_effects", "compound_lifestyle_risk",
  "individual_vs_cohort_ratio", "cost_efficiency_quintiles", "outlier_detection_flags",
  "smoker_metabolic_interaction", "age_bmi_compound_risk", 
  "comprehensive_health_risk_score", "healthcare_utilization_predictor",
  "market_competitiveness_index", "value_based_care_score"
)

advanced_feature_results <- map_dfr(new_advanced_features, 
                                    ~perform_feature_test(insurance_advanced, .x))

# Combine all feature results
final_feature_results <- bind_rows(
  complete_feature_analysis,
  advanced_feature_results
) %>%
  arrange(desc(eta_sqaured))

write_csv(final_feature_results, "outputs/tables/final_complete_feature_analysis.csv")

# 7 Encoding ####
## A good level of encoding took place in step 4.4, post-advanced-FE encoding for continuity and a more "outlined" version of encoding

# Strategy 1: One-Hot Encoding
categorical_vars <- c("sex", "region", "smoker", "bmi_category", 
                      "outlier_detection_flags")

insurance_onehot <- insurance_advanced %>%
  dummy_cols(
    select_columns = categorical_vars,
    remove_first_dummy = TRUE,
    remove_selected_columns = FALSE
  )

# Strategy 2: Ordinal Encoding
insurance_ordinal <- insurance_advanced %>%
  mutate(
    bmi_risk_ordinal = case_when(
      bmi_category == "underweight" ~ 1,
      bmi_category == "normal" ~ 2, 
      bmi_category == "overweight" ~ 3,
      bmi_category == "obese" ~ 4
    ),
    
    region_cost_ordinal = case_when(
      region == "southeast" ~ 1,
      region == "southwest" ~ 2,
      region == "northwest" ~ 3,
      region == "northeast" ~ 4
    ),
    
    cost_efficiency_ordinal = cost_efficiency_quintiles,
    
    outlier_ordinal = case_when(
      outlier_detection_flags == "Normal" ~ 1,
      outlier_detection_flags == "Low_Cost_Outlier" ~ 2,
      outlier_detection_flags == "High_Cost_Outlier" ~ 3,
      outlier_detection_flags == "Risk_Cost_Mismatch" ~ 4,
      outlier_detection_flags == "Low_Risk_High_Cost" ~ 5,
      TRUE ~ 1
    )
  )

# Strategy 3: Binary Encoding
insurance_binary <- insurance_advanced %>%
  mutate(
    is_smoker = as.numeric(smoker == "yes"),
    is_male = as.numeric(sex == "male"),
    is_obese = as.numeric(bmi_category == "obese"),
    has_children = as.numeric(children > 0),
    is_high_risk_region = as.numeric(region %in% c("northeast", "northwest")),
    
    smoker_male = is_smoker * is_male,
    smoker_obese = is_smoker * is_obese,
    older_smoker = as.numeric(age >= 40 & smoker == "yes"),
    high_compound_risk = as.numeric(comprehensive_health_risk_score >= 4),
    is_cost_outlier = as.numeric(outlier_detection_flags != "Normal")
  )

# Strategy 4: Target Encoding
target_encoding_mappings <- insurance_advanced %>%
  group_by(region) %>%
  summarise(region_mean_cost = mean(charges), .groups = 'drop')

insurance_target_encoded <- insurance_advanced %>%
  left_join(target_encoding_mappings, by = "region") %>%
  mutate(
    region_target_encoded = region_mean_cost,
    region_vs_global_ratio = region_mean_cost / mean(charges)
  )

write_csv(insurance_onehot, "data/processed/insurance_onehot_encoded.csv")
write_csv(insurance_ordinal, "data/processed/insurance_ordinal_encoded.csv") 
write_csv(insurance_binary, "data/processed/insurance_binary_encoded.csv")
write_csv(insurance_target_encoded, "data/processed/insurance_target_encoded.csv")

# 8 Scaling & Normalization ####
## Notes 
### one-hot AND ordinal region encoding - Choose one approach for final modeling to avoid redundancy

# Verify  encoded features are numeric
str(insurance_with_engineered_features)

# 9 Analytics ####


# 10 Model Selection ####

# A Final Data Quality Checks ####
# 
# # Check for missing values
# missing_values_summary <- final_feature_data %>%
#   summarise(across(everything(), ~sum(is.na(.x)))) %>%
#   pivot_longer(everything(), names_to = "feature", values_to = "missing_count") %>%
#   filter(missing_count > 0) %>%
#   arrange(desc(missing_count))
# 
# write_csv(missing_values_summary, "outputs/tables/final_missing_values_summary.csv")
# 
# # Data types summary
# data_types_summary <- final_feature_data %>%
#   summarise(across(everything(), ~class(.x)[1])) %>%
#   pivot_longer(everything(), names_to = "feature", values_to = "data_type") %>%
#   count(data_type, sort = TRUE)
# 
# write_csv(data_types_summary, "outputs/tables/final_data_types_summary.csv")
# 
# # Feature count by category
# feature_count_summary <- data.frame(
#   category = c("Original", "Benchmark", "Non-linear", "Interaction", "Risk-related",
#                "Encoded (one-hot)", "Scaled", "Total"),
#   count = c(
#     ncol(insurance_clean),
#     1,
#     6,  # age_squared, age_cubed, bmi_squared, bmi_cubed, age_decade, bmi_detailed
#     4,  # age_smoker_interaction, age_sex_interaction, age_bmi_interaction, smoker_bmi_interaction
#     6,  # high_risk_combo, risk_score, cost_deviation, cost_percentile, is_cost_outlier, region_cost_rank
#     sum(str_detect(names(final_feature_data), "^(sex|region|smoker|bmi_category|high_risk_combo|risk_score|bmi_detailed)_")),
#     sum(str_detect(names(final_feature_data), "_scaled$")),
#     ncol(final_feature_data)
#   )
# )
# 
# write_csv(feature_count_summary, "outputs/tables/final_feature_count_summary.csv")
# 
# B Final Summary Report ####
# 
# # Merge summaries of both original and engineered ANOVA results
# all_completed_tests <- bind_rows(
#   regular_anova_plan_updated %>%
#     select(anova_code, what_it_tests, expected_significance, f_value, p_value, significance, status) %>%
#     mutate(source = "original"),
#   engineered_anova_plan_final %>%
#     select(anova_code, what_it_tests, expected_significance, f_value, p_value, significance, status) %>%
#     mutate(source = "engineered")
# )
# 
# # Top statistically significant features (p < 0.05)
# significant_features <- all_completed_tests %>%
#   filter(significance %in% c("***", "**", "*")) %>%
#   arrange(p_value)
# 
# write_csv(significant_features, "outputs/tables/final_significant_features.csv")
# 
# # Save complete test summary
# write_csv(all_completed_tests, "outputs/tables/final_all_anova_tests_summary.csv")
# 
# # Save final feature dataset (cleaned and processed)
# write_csv(final_feature_data, "data/processed/final_model_ready_data.csv")
