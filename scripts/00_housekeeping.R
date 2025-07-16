# 00_housekeeping.R
# Project: Healthcare Cost Prediction
# Purpose: General housekeeping and project organization
# Created: 2025-07-16

# Load required libraries
library(tidyverse)
library(here)

# Set up project paths
project_paths <- list(
  data_raw = here("data", "raw"),
  data_processed = here("data", "processed"),
  data_external = here("data", "external"),
  outputs_tables = here("outputs", "tables"),
  outputs_plots = here("outputs", "plots"),
  outputs_models = here("outputs", "models"),
  tableau_extracts = here("tableau", "data_extracts"),
  tableau_dashboards = here("tableau", "dashboards"),
  docs = here("docs")
)

# Function to check project structure
check_project_structure <- function() {
  structure_status <- tibble(
    path_name = names(project_paths),
    path = unlist(project_paths),
    exists = map_lgl(project_paths, dir.exists)
  )
  return(structure_status)
}

# Function to clean up temporary files
cleanup_temp_files <- function() {
  temp_files <- list.files(pattern = "^temp_|_temp\\.", recursive = TRUE)
  if (length(temp_files) > 0) {
    file.remove(temp_files)
    return(paste("Cleaned up", length(temp_files), "temporary files"))
  } else {
    return("No temporary files found")
  }
}

# Function to get project file summary
get_project_summary <- function() {
  summary_data <- tibble(
    directory = c("scripts", "data", "outputs", "tableau", "docs"),
    file_count = map_dbl(c("scripts", "data", "outputs", "tableau", "docs"),
                        ~ length(list.files(.x, recursive = TRUE)))
  )
  return(summary_data)
}

# Function to update project documentation
update_project_docs <- function() {
  # Get current date for documentation
  last_updated <- Sys.Date()
  
  # Create or update project log
  project_log <- tibble(
    date = last_updated,
    action = "Housekeeping script run",
    status = "Complete"
  )
  
  # Save to log file
  if (file.exists("docs/project_log.csv")) {
    existing_log <- read_csv("docs/project_log.csv", show_col_types = FALSE)
    updated_log <- bind_rows(existing_log, project_log)
  } else {
    updated_log <- project_log
  }
  
  write_csv(updated_log, "docs/project_log.csv")
  return("Documentation updated")
}

# Main housekeeping function
run_housekeeping <- function() {
  results <- list()
  results$structure_check <- check_project_structure()
  results$cleanup_result <- cleanup_temp_files()
  results$project_summary <- get_project_summary()
  results$doc_update <- update_project_docs()
  
  return(results)
}
