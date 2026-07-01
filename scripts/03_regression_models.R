source("R/analysis_helpers.R")
check_required_packages()
options(survey.lonely.psu = "adjust")

analysis <- readRDS("data/processed/analysis_dataset.rds")

fit_svy_model <- function(outcome, predictor_row, data = analysis) {
  needed <- c(outcome, predictor_row$scaled_variable, adjustment_terms, "wtmec2yr", "sdmvpsu", "sdmvstra")
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()
  design <- svy_design(dat)
  formula <- stats::as.formula(
    paste(outcome, "~", paste(c(predictor_row$scaled_variable, adjustment_terms), collapse = " + "))
  )
  model <- survey::svyglm(formula, design = design, family = quasibinomial())
  outcome_label <- outcome_variables[[outcome]]
  predictor_label <- predictor_row$label[[1]]
  predictor_increment <- predictor_row$increment[[1]]
  event_count <- sum(dat[[outcome]] == 1, na.rm = TRUE)
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == predictor_row$scaled_variable) |>
    dplyr::transmute(
      outcome = outcome_label,
      predictor = predictor_label,
      increment = predictor_increment,
      n = nrow(dat),
      events = event_count,
      odds_ratio = exp(estimate),
      conf_low = exp(conf.low),
      conf_high = exp(conf.high),
      p_value = p.value,
      p_value_formatted = format_p(p.value)
    )
}

regression_results <- purrr::map_dfr(names(outcome_variables), function(outcome) {
  purrr::pmap_dfr(anthro_predictors, function(predictor, label, scaled_variable, increment) {
    fit_svy_model(outcome, tibble::tibble(
      predictor = predictor,
      label = label,
      scaled_variable = scaled_variable,
      increment = increment
    ))
  })
})

write_csv_safely(regression_results, "tables/table_3_survey_weighted_adjusted_associations.csv")
save_rds_safely(regression_results, "results/regression_results.rds")
writeLines(c("Models adjusted for age, sex, race/ethnicity, education, and poverty-income ratio."), "logs/03_regression_models_log.txt")
