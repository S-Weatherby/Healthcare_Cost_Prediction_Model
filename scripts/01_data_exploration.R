library(tidyverse)
library(readr)
library(readxl)
library(here)
library(janitor)
library(skimr)

# 1 Loading main data set ####

insurance_data <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\insurance.csv")

# 2 HCUP hospital data ####        

hcup_age <- read_excel("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\hcup_costs_age_2018_2022.xlsx", sheet = 4)
hcup_race <- read_excel("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\hcup_costs_race_2018_2022.xlsx", sheet = 4)
hcup_gender <- read_excel("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\hcup_costs_gender_2018_2022.xlsx", sheet = 4)
hcup_region <- read_excel("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\hcup__costs_region_2018_2022.xlsx", sheet = 4)
hcup_overall <- read_excel("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\hcup_costs_overall_2018_2022.xlsx", sheet = 4)


#3 CMS Data ####

cms_per_capita <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\US_PER_CAPITA20.CSV")
cms_phi_enrolle <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\PHI_PER_ENROLLEE20.CSV")
cms_medicare <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\MEDICARE_PER_ENROLLEE20.CSV")
cms_medicaid <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\MEDICAID_PER_ENROLLEE20.CSV")
cms_nhe_2023 <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\NHE2023.csv")
cms_age_sex_detailed <-read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\age and Sex percap.csv")
cms_age_sex_major <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Healthcare_Cost_Prediction_Model\\data\\raw\\age and Sex percap major group.csv")

# 4 Exploring, Cleaning, and Data Dictionary ####

# 4.1 Function to create data dictionary #### 
create_data_dictionary <- function(data, source_name) {
  data_dict <- tibble(
    variable_name = names(data),
    data_type = map_chr(data, function(x) class(x)[1]),
    source = source_name,
    description = "",
    range_values = map_chr(data, function(x) {
      if(is.numeric(x)) {
        paste0(round(min(x, na.rm=TRUE), 2), " to ", round(max(x, na.rm=TRUE), 2))
      } else {
        paste(sort(unique(x)), collapse = ", ")
      }
    }),
    missing_count = map_int(data, ~sum(is.na(.))),
    missing_percent = round(map_dbl(data, ~sum(is.na(.)) / length(.) * 100), 2),
    notes = ""
  )
  return(data_dict)
}

# Create initial dictionary for main dataset
insurance_dict <- create_data_dictionary(insurance_data, "Kaggle Medical Insurance Dataset")

# Add descriptions for original variables
insurance_dict$description <- c(
  "Age of primary beneficiary in years",
  "Primary beneficiary biological sex", 
  "Body mass index (weight in kg / (height in meters)^2)",
  "Number of children/dependents covered by health insurance",
  "Smoking status of primary beneficiary",
  "Beneficiary's residential area in the US",
  "Individual medical costs billed by health insurance in USD"
)

#save data dictionary
write_csv(insurance_dict, "data/data_dictionary.csv")

# Function to add new variables to dictionary
add_to_dictionary <- function(variable_name, data_type, source, description, 
                              range_values = "", missing_count = 0, 
                              missing_percent = 0, notes = "") {
  
  # Load existing dictionary
  if(file.exists("data/data_dictionary.csv")) {
    dict <- read_csv("data/data_dictionary.csv", show_col_types = FALSE)
  } else {
    stop("Data dictionary not found. Run data loading script first.")
  }
  
  # Check if variable already exists
  if(variable_name %in% dict$variable_name) {
    cat("⚠️  Variable", variable_name, "already exists in dictionary\n")
    return(invisible())
  }
  
  # Add new row
  new_row <- tibble(
    variable_name = variable_name,
    data_type = data_type,
    source = source,
    description = description,
    range_values = range_values,
    missing_count = missing_count,
    missing_percent = missing_percent,
    notes = notes
  )
  
  # Combine and save
  updated_dict <- bind_rows(dict, new_row)
  write_csv(updated_dict, "data/data_dictionary.csv")
  
  cat("✅ Added", variable_name, "to data dictionary\n")
}

# Function to update existing variables
update_dictionary <- function(variable_name, ...) {
  dict <- read_csv("data/data_dictionary.csv", show_col_types = FALSE)
  
  updates <- list(...)
  for(field in names(updates)) {
    if(field %in% names(dict)) {
      dict[dict$variable_name == variable_name, field] <- updates[[field]]
    }
  }

  write_csv(dict, "data/data_dictionary.csv")
}

# 4.2 Insurance data set cleaning #### 

glimpse(insurance_data)
skim(insurance_data)
summary(insurance_data)

# Insurance missing and dupes
missing_summary <- insurance_data %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0)

if(nrow(missing_summary) > 0) {
  print(missing_summary)
}

duplicates <- insurance_data %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  nrow()

#validate age and BMI
age_summary <- summary(insurance_data$age)
print(age_summary)

age_outliers <- insurance_data %>%
  filter(age < 18 | age > 64)
#none


bmi_summary <- summary(insurance_data$bmi)
print(bmi_summary)

bmi_outliers <- insurance_data %>%
  filter(bmi < 15 | bmi > 50)
# only 3 found (keeping)

# checking categorical variables
unique(insurance_data$sex)
unique(insurance_data$smoker)
unique(insurance_data$region)

# checking for outliers in charges
charges_summary <- summary(insurance_data$charges)
print(charges_summary)

Q1 <- quantile(insurance_data$charges, 0.25)
Q3 <- quantile(insurance_data$charges, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers_count <- sum(insurance_data$charges < lower_bound | insurance_data$charges > upper_bound)

# Check the outlier bounds and count
print(paste("Lower bound: $", round(lower_bound, 2)))
print(paste("Upper bound: $", round(upper_bound, 2)))
print(paste("Number of outliers:", outliers_count))

# Look at the actual outliers
charges_outliers <- insurance_data %>%
  filter(charges < lower_bound | charges > upper_bound)

print(charges_outliers)

# Check what percentage of data are outliers
outlier_percentage <- (outliers_count / nrow(insurance_data)) * 100
print(paste("Percentage of outliers:", round(outlier_percentage, 1), "%"))

# Confirm the smoking pattern
outlier_smokers <- charges_outliers %>%
  count(smoker) %>%
  mutate(percentage = n/sum(n)*100)

print(outlier_smokers)

# Compare to overall smoking rates
overall_smokers <- insurance_data %>%
  count(smoker) %>%
  mutate(percentage = n/sum(n)*100)

print(overall_smokers)

# Create categories
insurance_clean <- insurance_data %>%
  mutate(
    age_group = case_when(
      age < 30 ~ "Young Adult",
      age < 45 ~ "Middle Age",
      age < 60 ~ "Pre-Senior",
      TRUE ~ "Senior"
    ),
    
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal Weight",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obese"
    ),
    
    has_children = factor(ifelse(children > 0, "Yes", "No")),
    high_cost = factor(ifelse(charges > median(charges), "High", "Low"))
  )
# Adding to data dictionary
update_dictionary("sex", data_type = "factor", notes = "Converted to title case and factor")
update_dictionary("smoker", data_type = "factor", notes = "Converted to title case and factor")
update_dictionary("region", data_type = "factor", notes = "Converted to title case and factor")

add_to_dictionary("age_group", "factor", "Derived from age", 
                  "Age categories for demographic analysis",
                  "Young Adult, Middle Age, Pre-Senior, Senior", 0, 0,
                  "Created using 15-year age bands")

add_to_dictionary("bmi_category", "factor", "Derived from bmi", 
                  "WHO BMI categories for health analysis",
                  "Underweight, Normal Weight, Overweight, Obese", 0, 0,
                  "Based on WHO BMI standards")

add_to_dictionary("has_children", "factor", "Derived from children", 
                  "Indicator of whether beneficiary has dependents",
                  "Yes, No", 0, 0,
                  "Binary indicator for family status")

add_to_dictionary("high_cost", "factor", "Derived from charges", 
                  "High vs low cost classification",
                  "High, Low", 0, 0,
                  "Split at median charges value")

# Save cleaned insurance
write_csv(insurance_clean, "data/processed/insurance_clean.csv")

head(insurance_clean)
summary(insurance_clean)