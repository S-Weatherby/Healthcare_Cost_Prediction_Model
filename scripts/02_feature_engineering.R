# Complete Feature Engineering Script - Script 02 (FIXED)
# Author: Shelita Smith
# Date: July 23, 2025
# Purpose: Feature engineering with ANOVA analysis and comprehensive documentation
# Goals: ANOVA'd features, Analysis table guides, updated data dictionary in preparation for modeling

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

# Check if the merge worked
glimpse(insurance_with_benchmarks)

# How many people got benchmarks?
merge_success_count <- sum(!is.na(insurance_with_benchmarks$avg_hcup_charges))
cat("Records successfully merged with benchmarks:", merge_success_count, "\n")

# 3 Regular ANOVA Analysis ####

# 3.1 Core variables tests ####
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

# 3.2 Core variable interactions tests ####

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

# 3.3 Extracting Regular ANOVA results ####

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
  slice(1:12) %>%  # Only keep the analyses we've run
  bind_cols(
    select(anova_results, f_value, p_value, df, eta_squared, sig_level, significant, effect_size_interpretation)
  )

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

# 3.4 Add ANOVA results to data dictionary ####

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

# 4 Engineered Features ####

# 4.1 Engineered features table

engineered_features_table <- tibble(
  feature_id = 1:34,  # Corrected to match actual number of features
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

# View the table
View(engineered_features_table)

# 4.2 Engineered features creation


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

# 4 Engineered feature table for organization 



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



# Save updated tables (overwrite existing)

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
    # ALIGNED interaction features with your table
    age_smoker_interaction = age * as.numeric(smoker == "yes"),
    age_sex_interaction = age * as.numeric(sex == "male"),
    
    # Original smoker interactions
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
                      "age_smoker_interaction", "age_sex_interaction", 
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

# 8 ANOVA on Engineered Features ####

# Test all engineered features that were created - ALIGNED WITH TABLE
engineered_feature_tests <- list()

# Test features that exist in the dataset
if("cost_vs_national" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["cost_vs_national"]] <- aov(charges ~ cost_vs_national, data = insurance_with_benchmarks)
}

if("age_smoker_interaction" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["age_smoker_interaction"]] <- aov(charges ~ age_smoker_interaction, data = insurance_with_benchmarks)
}

if("age_sex_interaction" %in% names(insurance_with_benchmarks)) {
  engineered_feature_tests[["age_sex_interaction"]] <- aov(charges ~ age_sex_interaction, data = insurance_with_benchmarks)
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

# Extract results from engineered feature tests using your existing function
engineered_test_results <- data.frame()

for(test_name in names(engineered_feature_tests)) {
  result <- extract_anova_results_enhanced(engineered_feature_tests[[test_name]], test_name)
  engineered_test_results <- rbind(engineered_test_results, result)
}

# Save engineered feature ANOVA results
write_csv(engineered_test_results, "outputs/tables/engineered_features_anova_results.csv")

# 8.1 Update Engineered Features ANOVA Plan with Results ####

# Function to properly update engineered features plan with actual results - FIXED
update_engineered_plan_with_results <- function(plan, results) {
  # Create a copy of the plan to update
  plan_updated <- plan %>%
    mutate(
      f_value = NA_real_,
      p_value = NA_character_,
      significance = NA_character_,
      status = "Not Completed"
    )
  
  # Create a mapping between plan anova_code and test results
  variable_mapping <- list(
    "cost_vs_national" = "cost_vs_national",
    "age_smoker_interaction" = "age_smoker_interaction",
    "age_sex_interaction" = "age_sex_interaction", 
    "high_risk_combo" = "high_risk_combo",
    "risk_score" = "risk_score",
    "age_squared" = "age_squared",
    "bmi_squared" = "bmi_squared",
    "age_bmi_interaction" = "age_bmi_interaction"
  )
  
  # Loop through each row in the plan
  for(i in 1:nrow(plan_updated)) {
    # Use anova_code column instead of variables
    plan_variable <- plan_updated$anova_code[i]
    
    # Check if plan_variable has a valid value
    if(length(plan_variable) > 0 && !is.na(plan_variable) && plan_variable %in% names(variable_mapping)) {
      test_variable <- variable_mapping[[plan_variable]]
      
      # Find matching result in results data
      matching_result <- results %>% 
        filter(test_name == test_variable & term == test_variable) %>%
        slice_head(n = 1)
      
      if(nrow(matching_result) > 0 && !is.na(matching_result$f_value)) {
        # Update plan with results
        plan_updated$f_value[i] <- round(matching_result$f_value, 3)
        
        # Format p-value
        p_val <- matching_result$p_value
        plan_updated$p_value[i] <- if_else(p_val < 0.001, "< 0.001", 
                                           as.character(round(p_val, 4)))
        
        # Assign significance codes
        plan_updated$significance[i] <- case_when(
          p_val < 0.001 ~ "***",
          p_val < 0.01 ~ "**", 
          p_val < 0.05 ~ "*",
          p_val < 0.1 ~ ".",
          TRUE ~ "ns"
        )
        
        plan_updated$status[i] <- "Completed"
      }
    }
  }
  
  return(plan_updated)
}

# Apply the update function
engineered_anova_plan_final <- update_engineered_plan_with_results(
  engineered_anova_plan_updated, 
  engineered_test_results
)

# Save the updated plan
write_csv(engineered_anova_plan_final, "outputs/tables/engineered_features_anova_plan.csv")

## 8.2 Create Summary Reports for Engineered Features ####

# Summary of completed engineered feature tests
engineered_completed_summary <- engineered_anova_plan_final %>%
  filter(status == "Completed" & !is.na(f_value)) %>%
  arrange(desc(f_value)) %>%
  select(anova_code, what_it_tests, f_value, p_value, significance, expected_significance)

write_csv(engineered_completed_summary, "outputs/tables/engineered_features_completed_summary.csv")

# Summary by significance level
engineered_significance_summary <- engineered_anova_plan_final %>%
  filter(status == "Completed") %>%
  count(significance, expected_significance) %>%
  arrange(expected_significance, significance)

write_csv(engineered_significance_summary, "outputs/tables/engineered_features_significance_summary.csv")

# 9 Final Data Quality Checks ####

# Check for missing values
missing_values_summary <- final_feature_data %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

write_csv(missing_values_summary, "outputs/tables/final_missing_values_summary.csv")

# Data types summary
data_types_summary <- final_feature_data %>%
  summarise(across(everything(), ~class(.x)[1])) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "data_type") %>%
  count(data_type, sort = TRUE)

write_csv(data_types_summary, "outputs/tables/final_data_types_summary.csv")

# Feature count by category
feature_count_summary <- data.frame(
  category = c("Original", "Benchmark", "Non-linear", "Interaction", "Risk-related",
               "Encoded (one-hot)", "Scaled", "Total"),
  count = c(
    ncol(insurance_clean),
    1,
    6,  # age_squared, age_cubed, bmi_squared, bmi_cubed, age_decade, bmi_detailed
    4,  # age_smoker_interaction, age_sex_interaction, age_bmi_interaction, smoker_bmi_interaction
    6,  # high_risk_combo, risk_score, cost_deviation, cost_percentile, is_cost_outlier, region_cost_rank
    sum(str_detect(names(final_feature_data), "^(sex|region|smoker|bmi_category|high_risk_combo|risk_score|bmi_detailed)_")),
    sum(str_detect(names(final_feature_data), "_scaled$")),
    ncol(final_feature_data)
  )
)

write_csv(feature_count_summary, "outputs/tables/final_feature_count_summary.csv")

# 10 Final Summary Report ####

# Merge summaries of both original and engineered ANOVA results
all_completed_tests <- bind_rows(
  regular_anova_plan_updated %>%
    select(anova_code, what_it_tests, expected_significance, f_value, p_value, significance, status) %>%
    mutate(source = "original"),
  engineered_anova_plan_final %>%
    select(anova_code, what_it_tests, expected_significance, f_value, p_value, significance, status) %>%
    mutate(source = "engineered")
)

# Top statistically significant features (p < 0.05)
significant_features <- all_completed_tests %>%
  filter(significance %in% c("***", "**", "*")) %>%
  arrange(p_value)

write_csv(significant_features, "outputs/tables/final_significant_features.csv")

# Save complete test summary
write_csv(all_completed_tests, "outputs/tables/final_all_anova_tests_summary.csv")

# Save final feature dataset (cleaned and processed)
write_csv(final_feature_data, "data/processed/final_model_ready_data.csv")
