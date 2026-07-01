source("R/analysis_helpers.R")
check_required_packages()

expected_tables <- c(
  "tables/table_1_weighted_baseline_characteristics.csv",
  "tables/table_2_outcome_prevalence_by_anthropometric_categories.csv",
  "tables/table_3_survey_weighted_adjusted_associations.csv",
  "tables/table_4_predictive_performance.csv",
  "tables/supplementary_table_1_variable_definitions.csv",
  "tables/supplementary_table_2_missingness.csv",
  "tables/supplementary_table_3_sex_stratified_regression.csv",
  "tables/supplementary_table_4_sensitivity_analyses.csv"
)

manifest <- tibble::tibble(
  table_file = expected_tables,
  exists = file.exists(expected_tables),
  generated_at = as.character(Sys.time())
)

write_csv_safely(manifest, "tables/table_manifest.csv")
writeLines(c("Table manifest written.", paste(expected_tables, collapse = "\n")), "logs/06_tables_log.txt")
