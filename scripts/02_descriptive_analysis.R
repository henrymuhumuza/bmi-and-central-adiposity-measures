source("R/analysis_helpers.R")
check_required_packages()
options(survey.lonely.psu = "adjust")

analysis <- readRDS("data/processed/analysis_dataset.rds")
design <- svy_design(analysis)

continuous_vars <- tibble::tribble(
  ~variable, ~label,
  "age", "Age, years",
  "pir", "Poverty-income ratio",
  "bmi", "Body mass index, kg/m²",
  "waist_cm", "Waist circumference, cm",
  "waist_hip_ratio", "Waist-hip ratio"
)

continuous_table <- purrr::pmap_dfr(continuous_vars, function(variable, label) {
  mean_se <- weighted_mean_se(design, variable)
  qs <- weighted_quantile(design, variable)
  tibble::tibble(
    characteristic = label,
    level = NA_character_,
    estimate = sprintf("%.1f (SE %.1f)", mean_se$estimate, mean_se$se),
    median_iqr = sprintf("%.1f (%.1f, %.1f)", qs[2], qs[1], qs[3])
  )
})

categorical_vars <- tibble::tribble(
  ~variable, ~label,
  "sex", "Sex",
  "race_ethnicity", "Race and ethnicity",
  "education", "Education",
  "bmi_category", "BMI category",
  "waist_category", "Waist circumference category",
  "whr_category", "Waist-hip ratio category"
)

categorical_table <- purrr::pmap_dfr(categorical_vars, function(variable, label) {
  f <- stats::as.formula(paste0("~", variable))
  est <- survey::svymean(f, design, na.rm = TRUE)
  tibble::tibble(
    characteristic = label,
    level = gsub(paste0("^", variable), "", names(stats::coef(est))),
    estimate = sprintf("%.1f%% (SE %.1f)", 100 * as.numeric(stats::coef(est)), 100 * as.numeric(survey::SE(est))),
    median_iqr = NA_character_
  )
})

outcome_table <- purrr::imap_dfr(outcome_variables, function(label, variable) {
  est <- weighted_binary_percent(design, variable)
  tibble::tibble(
    characteristic = label,
    level = "Present",
    estimate = sprintf("%.1f%% (SE %.1f)", est$percent, est$se),
    median_iqr = NA_character_
  )
})

table1 <- dplyr::bind_rows(continuous_table, categorical_table, outcome_table)
write_csv_safely(table1, "tables/table_1_weighted_baseline_characteristics.csv")

category_sets <- list(
  bmi_category = "BMI category",
  waist_category = "Waist circumference category",
  whr_category = "Waist-hip ratio category"
)

prevalence_by_category <- purrr::imap_dfr(category_sets, function(category_label, category_var) {
  purrr::imap_dfr(outcome_variables, function(outcome_label, outcome_var) {
    f <- stats::as.formula(paste0("~", outcome_var))
    by <- stats::as.formula(paste0("~", category_var))
    est <- survey::svyby(f, by, design, survey::svymean, na.rm = TRUE, vartype = "se")
    se_col <- grep("^se", names(est), value = TRUE)[1]
    tibble::tibble(
      category_type = category_label,
      category = as.character(est[[category_var]]),
      outcome = outcome_label,
      prevalence_percent = 100 * est[[outcome_var]],
      se_percent = 100 * est[[se_col]]
    )
  })
})

write_csv_safely(prevalence_by_category, "tables/table_2_outcome_prevalence_by_anthropometric_categories.csv")

correlation_data <- analysis |>
  dplyr::select(bmi, waist_cm, waist_hip_ratio) |>
  tidyr::drop_na()

correlation_table <- stats::cor(correlation_data, method = "spearman") |>
  as.data.frame() |>
  tibble::rownames_to_column("measure_1") |>
  tidyr::pivot_longer(-measure_1, names_to = "measure_2", values_to = "spearman_r")

write_csv_safely(correlation_table, "results/anthropometric_spearman_correlations.csv")
writeLines(c(paste("Analysis rows:", nrow(analysis))), "logs/02_descriptive_analysis_log.txt")
