# Create the housekeeping script content
housekeeping_content <- c(
  "# 00_housekeeping.R",
  paste("# Project: Healthcare Cost Prediction"),
  "# Purpose: General housekeeping and project organization",
  paste("# Created:", Sys.Date()),
  "",
  "# Load required libraries",
  "library(tidyverse)",
  "library(here)",
  "",
  "# Set up project paths",
  "project_paths <- list(",
  '  data_raw = here("data", "raw"),',
  '  data_processed = here("data", "processed"),',
  '  data_external = here("data", "external"),',
  '  outputs_tables = here("outputs", "tables"),',
  '  outputs_plots = here("outputs", "plots"),',
  '  outputs_models = here("outputs", "models"),',
  '  tableau_extracts = here("tableau", "data_extracts"),',
  '  tableau_dashboards = here("tableau", "dashboards"),',
  '  docs = here("docs")',
  ")",
  "",
  "# Function to check project structure",
  "check_project_structure <- function() {",
  "  structure_status <- tibble(",
  "    path_name = names(project_paths),",
  "    path = unlist(project_paths),",
  "    exists = map_lgl(project_paths, dir.exists)",
  "  )",
  "  return(structure_status)",
  "}",
  "",
  "# Function to clean up temporary files",
  "cleanup_temp_files <- function() {",
  '  temp_files <- list.files(pattern = "^temp_|_temp\\\\.", recursive = TRUE)',
  "  if (length(temp_files) > 0) {",
  "    file.remove(temp_files)",
  '    return(paste("Cleaned up", length(temp_files), "temporary files"))',
  "  } else {",
  '    return("No temporary files found")',
  "  }",
  "}",
  "",
  "# Function to get project file summary",
  "get_project_summary <- function() {",
  "  summary_data <- tibble(",
  '    directory = c("scripts", "data", "outputs", "tableau", "docs"),',
  '    file_count = map_dbl(c("scripts", "data", "outputs", "tableau", "docs"),',
  "                        ~ length(list.files(.x, recursive = TRUE)))",
  "  )",
  "  return(summary_data)",
  "}",
  "",
  "# Function to update project documentation",
  "update_project_docs <- function() {",
  "  # Get current date for documentation",
  "  last_updated <- Sys.Date()",
  "  ",
  "  # Create or update project log",
  "  project_log <- tibble(",
  "    date = last_updated,",
  '    action = "Housekeeping script run",',
  '    status = "Complete"',
  "  )",
  "  ",
  "  # Save to log file",
  '  if (file.exists("docs/project_log.csv")) {',
  '    existing_log <- read_csv("docs/project_log.csv", show_col_types = FALSE)',
  "    updated_log <- bind_rows(existing_log, project_log)",
  "  } else {",
  "    updated_log <- project_log",
  "  }",
  "  ",
  '  write_csv(updated_log, "docs/project_log.csv")',
  '  return("Documentation updated")',
  "}",
  "",
  "# Main housekeeping function",
  "run_housekeeping <- function() {",
  "  results <- list()",
  "  results$structure_check <- check_project_structure()",
  "  results$cleanup_result <- cleanup_temp_files()",
  "  results$project_summary <- get_project_summary()",
  "  results$doc_update <- update_project_docs()",
  "  ",
  "  return(results)",
  "}"
)

# Write the housekeeping script to file
writeLines(housekeeping_content, "scripts/00_housekeeping.R")


# Check what files currently exist
current_files <- list.files("scripts/", full.names = TRUE)

# Function to safely rename scripts
rename_scripts_safely <- function() {
  
  # Define the renaming plan
  rename_plan <- list(
    # If old script 1 exists, rename to include "cleaning"
    c("scripts/01_data_exploration.R", "scripts/01_data_exploration_cleaning.R"),
    
    # Move script 2 to backup (instead of deleting)
    c("scripts/02_data_cleaning.R", "scripts/02_data_cleaning_BACKUP.R"),
    
    # Rename subsequent scripts to new numbering
    c("scripts/03_feature_engineering.R", "scripts/02_feature_engineering.R"),
    c("scripts/04_modeling.R", "scripts/03_modeling.R"),
    c("scripts/05_model_evaluation.R", "scripts/04_model_evaluation.R"),
    c("scripts/06_final_analysis.R", "scripts/05_final_analysis.R")
  )
  
  # Execute renames
  rename_results <- tibble(
    old_name = character(),
    new_name = character(),
    status = character()
  )
  
  for (rename_pair in rename_plan) {
    old_name <- rename_pair[1]
    new_name <- rename_pair[2]
    
    if (file.exists(old_name)) {
      success <- file.rename(old_name, new_name)
      status <- if (success) "Renamed" else "Failed"
    } else {
      status <- "File not found"
    }
    
    rename_results <- bind_rows(rename_results, 
                                tibble(old_name = old_name, 
                                       new_name = new_name, 
                                       status = status))
  }
  
  return(rename_results)
}

# Run the renaming
rename_results <- rename_scripts_safely()

# Check results
final_files <- list.files("scripts/", full.names = TRUE)