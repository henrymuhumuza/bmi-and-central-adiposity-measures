source("R/analysis_helpers.R")
check_required_packages()
ensure_project_dirs()
options(survey.lonely.psu = "adjust")

master <- read_master_data()

required <- c(
  "seqn", "wtmec2yr", "sdmvpsu", "sdmvstra", "age", "sex",
  "race_ethnicity", "education", "pir", "bmi", "waist_cm", "hip_cm",
  names(outcome_variables)
)

missing_required <- setdiff(required, names(master))
if (length(missing_required) > 0) {
  stop("Required columns are missing: ", paste(missing_required, collapse = ", "), call. = FALSE)
}

analysis <- master |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(names(outcome_variables)), as_binary_numeric),
    waist_hip_ratio = dplyr::if_else(!is.na(waist_cm) & !is.na(hip_cm) & hip_cm > 0, waist_cm / hip_cm, NA_real_),
    bmi_5 = bmi / 5,
    waist_10 = waist_cm / 10,
    whr_01 = waist_hip_ratio / 0.1,
    bmi_category = dplyr::case_when(
      is.na(bmi) ~ NA_character_,
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal weight",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obesity"
    ),
    waist_category = dplyr::case_when(
      is.na(waist_cm) | is.na(sex) ~ NA_character_,
      sex == "Female" & waist_cm >= 88 ~ "High waist circumference",
      sex == "Male" & waist_cm >= 102 ~ "High waist circumference",
      sex %in% c("Female", "Male") ~ "Below high-risk cutoff",
      TRUE ~ NA_character_
    ),
    whr_category = dplyr::case_when(
      is.na(waist_hip_ratio) | is.na(sex) ~ NA_character_,
      sex == "Female" & waist_hip_ratio >= 0.85 ~ "High waist-hip ratio",
      sex == "Male" & waist_hip_ratio >= 0.90 ~ "High waist-hip ratio",
      sex %in% c("Female", "Male") ~ "Below high-risk cutoff",
      TRUE ~ NA_character_
    ),
    age_group = dplyr::case_when(
      age >= 20 & age <= 39 ~ "20-39",
      age >= 40 & age <= 59 ~ "40-59",
      age >= 60 ~ ">=60",
      TRUE ~ NA_character_
    ),
    any_anthropometry = !is.na(bmi) | !is.na(waist_cm) | !is.na(waist_hip_ratio),
    any_outcome = rowSums(!is.na(dplyr::pick(dplyr::all_of(names(outcome_variables))))) > 0,
    analytic_eligible = age >= 20 & any_anthropometry & any_outcome
  ) |>
  dplyr::filter(analytic_eligible) |>
  dplyr::mutate(
    bmi_category = factor(bmi_category, levels = c("Underweight", "Normal weight", "Overweight", "Obesity")),
    waist_category = factor(waist_category, levels = c("Below high-risk cutoff", "High waist circumference")),
    whr_category = factor(whr_category, levels = c("Below high-risk cutoff", "High waist-hip ratio")),
    age_group = factor(age_group, levels = c("20-39", "40-59", ">=60"))
  )

key_variables <- c(
  "age", "sex", "race_ethnicity", "education", "pir",
  "bmi", "waist_cm", "hip_cm", "waist_hip_ratio",
  names(outcome_variables)
)

missingness <- tibble::tibble(variable = key_variables) |>
  dplyr::mutate(
    n_missing = purrr::map_int(variable, \(v) sum(is.na(analysis[[v]]))),
    n_nonmissing = nrow(analysis) - n_missing,
    percent_missing = 100 * n_missing / nrow(analysis)
  )

definitions <- tibble::tribble(
  ~variable, ~definition,
  "bmi", "Body mass index in kg/m², analyzed continuously per 5 kg/m².",
  "waist_cm", "Waist circumference in centimeters, analyzed continuously per 10 cm.",
  "waist_hip_ratio", "Waist circumference divided by hip circumference, analyzed continuously per 0.1 unit.",
  "hypertension", "Mean SBP >=140 mmHg, mean DBP >=90 mmHg, self-reported hypertension, or antihypertensive medication use.",
  "diabetes", "Self-reported physician diagnosis, HbA1c >=6.5%, insulin use, or diabetes medication use.",
  "ckd", "Estimated glomerular filtration rate <60 mL/min/1.73 m² or urine albumin-creatinine ratio >=30 mg/g.",
  "cardiometabolic_multimorbidity", "At least two of hypertension, diabetes, and chronic kidney disease.",
  "bmi_category", "Underweight <18.5, normal weight 18.5 to <25, overweight 25 to <30, obesity >=30 kg/m².",
  "waist_category", "High waist circumference defined as >=102 cm in men or >=88 cm in women.",
  "whr_category", "High waist-hip ratio defined as >=0.90 in men or >=0.85 in women."
)

write_csv_safely(analysis, "data/processed/analysis_dataset.csv")
save_rds_safely(analysis, "data/processed/analysis_dataset.rds")
write_csv_safely(missingness, "tables/supplementary_table_2_missingness.csv")
write_csv_safely(definitions, "tables/supplementary_table_1_variable_definitions.csv")

log_lines <- c(
  paste("Master rows:", nrow(master)),
  paste("Analytic eligible rows:", nrow(analysis)),
  paste("Master columns:", ncol(master)),
  paste("Analysis columns:", ncol(analysis)),
  "Excluded only participants missing all key anthropometric predictors or all outcome data."
)
writeLines(log_lines, "logs/01_data_preparation_log.txt")
