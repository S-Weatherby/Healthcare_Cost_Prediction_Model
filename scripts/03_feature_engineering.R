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
  # next with Interaction Features (non-linear/complex variables; allows model to consider real world non-linear impacts)- we'd see how age impacts those differences/outcomes (charges/costs)
  # review ANOVA tables
