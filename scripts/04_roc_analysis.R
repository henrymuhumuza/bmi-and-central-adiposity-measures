source("R/analysis_helpers.R")
check_required_packages()

analysis <- readRDS("data/processed/analysis_dataset.rds")

fit_roc <- function(outcome, predictor_row, adjusted = FALSE) {
  terms <- if (adjusted) c(adjustment_terms, predictor_row$scaled_variable) else predictor_row$scaled_variable
  needed <- c(outcome, terms)
  dat <- analysis |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()
  formula <- stats::as.formula(paste(outcome, "~", paste(terms, collapse = " + ")))
  model <- stats::glm(formula, data = dat, family = binomial())
  risk <- stats::predict(model, type = "response")
  auc_estimate <- auc_from_scores(dat[[outcome]], risk)
  ci <- bootstrap_auc_ci(dat[[outcome]], risk)
  outcome_label <- outcome_variables[[outcome]]
  predictor_label <- predictor_row$label[[1]]
  predictor_increment <- predictor_row$increment[[1]]
  event_count <- sum(dat[[outcome]] == 1, na.rm = TRUE)
  tibble::tibble(
    outcome = outcome_label,
    predictor = predictor_label,
    increment = predictor_increment,
    model_type = ifelse(adjusted, "Adjusted demographic-clinical", "Anthropometric only"),
    n = nrow(dat),
    events = event_count,
    auc = auc_estimate,
    conf_low = ci[1],
    conf_high = ci[3],
    aic = stats::AIC(model),
    bic = stats::BIC(model)
  )
}

roc_results <- purrr::map_dfr(names(outcome_variables), function(outcome) {
  purrr::pmap_dfr(anthro_predictors, function(predictor, label, scaled_variable, increment) {
    row <- tibble::tibble(predictor, label, scaled_variable, increment)
    dplyr::bind_rows(fit_roc(outcome, row, FALSE), fit_roc(outcome, row, TRUE))
  })
})

write_csv_safely(roc_results, "tables/table_4_predictive_performance.csv")
save_rds_safely(roc_results, "results/roc_results.rds")
writeLines(c(
  "ROC analyses use unweighted logistic models and empirical rank-based AUC estimates.",
  "Interpretation should emphasize that primary inference comes from survey-weighted logistic regression."
), "logs/04_roc_analysis_log.txt")
