source("R/analysis_helpers.R")
check_required_packages()
options(survey.lonely.psu = "adjust")

analysis <- readRDS("data/processed/analysis_dataset.rds")

fit_sensitivity <- function(data, stratum_name, stratum_level, outcome, predictor_row) {
  needed <- c(outcome, predictor_row$scaled_variable, adjustment_terms, "wtmec2yr", "sdmvpsu", "sdmvstra")
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()
  if (nrow(dat) < 50 || sum(dat[[outcome]] == 1) < 10 || sum(dat[[outcome]] == 0) < 10) {
    return(tibble::tibble())
  }
  usable_adjustment_terms <- adjustment_terms[vapply(adjustment_terms, function(v) {
    dplyr::n_distinct(dat[[v]], na.rm = TRUE) >= 2
  }, logical(1))]
  model_terms <- c(predictor_row$scaled_variable, usable_adjustment_terms)
  model <- survey::svyglm(
    stats::as.formula(paste(outcome, "~", paste(model_terms, collapse = " + "))),
    design = svy_design(dat),
    family = quasibinomial()
  )
  outcome_label <- outcome_variables[[outcome]]
  predictor_label <- predictor_row$label[[1]]
  predictor_increment <- predictor_row$increment[[1]]
  event_count <- sum(dat[[outcome]] == 1, na.rm = TRUE)
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == predictor_row$scaled_variable) |>
    dplyr::transmute(
      sensitivity = stratum_name,
      stratum = stratum_level,
      outcome = outcome_label,
      predictor = predictor_label,
      increment = predictor_increment,
      n = nrow(dat),
      events = event_count,
      odds_ratio = exp(estimate),
      conf_low = exp(conf.low),
      conf_high = exp(conf.high),
      p_value = p.value
    )
}

sex_stratified <- purrr::map_dfr(stats::na.omit(unique(analysis$sex)), function(level) {
  dat <- dplyr::filter(analysis, sex == level)
  purrr::map_dfr(names(outcome_variables), function(outcome) {
    purrr::pmap_dfr(anthro_predictors, function(predictor, label, scaled_variable, increment) {
      fit_sensitivity(dat, "Sex stratified", level, outcome, tibble::tibble(predictor, label, scaled_variable, increment))
    })
  })
})

age_stratified <- purrr::map_dfr(stats::na.omit(unique(analysis$age_group)), function(level) {
  dat <- dplyr::filter(analysis, age_group == level)
  purrr::map_dfr(names(outcome_variables), function(outcome) {
    purrr::pmap_dfr(anthro_predictors, function(predictor, label, scaled_variable, increment) {
      fit_sensitivity(dat, "Age stratified", as.character(level), outcome, tibble::tibble(predictor, label, scaled_variable, increment))
    })
  })
})

not_underweight <- purrr::map_dfr(names(outcome_variables), function(outcome) {
  dat <- dplyr::filter(analysis, is.na(bmi) | bmi >= 18.5)
  purrr::pmap_dfr(anthro_predictors, function(predictor, label, scaled_variable, increment) {
    fit_sensitivity(dat, "Excluding underweight", "BMI >=18.5 kg/m²", outcome, tibble::tibble(predictor, label, scaled_variable, increment))
  })
})

sensitivity_results <- dplyr::bind_rows(sex_stratified, age_stratified, not_underweight)

write_csv_safely(sex_stratified, "tables/supplementary_table_3_sex_stratified_regression.csv")
write_csv_safely(sensitivity_results, "tables/supplementary_table_4_sensitivity_analyses.csv")
save_rds_safely(sensitivity_results, "results/sensitivity_results.rds")
writeLines(c("Sensitivity analyses include sex strata, age strata, and exclusion of underweight participants."), "logs/05_sensitivity_analyses_log.txt")
