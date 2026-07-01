source("R/analysis_helpers.R")
check_required_packages()

analysis <- readRDS("data/processed/analysis_dataset.rds")
design <- svy_design(analysis)

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 11))

master_n <- nrow(read_master_data())
analytic_n <- nrow(analysis)
excluded_n <- master_n - analytic_n

flow <- tibble::tibble(
  step = c(
    "NHANES adult master dataset",
    "Excluded from analytic dataset",
    "Eligible analytic dataset",
    "Outcome-specific complete cases"
  ),
  n = c(master_n, excluded_n, analytic_n, NA_integer_),
  label = c(
    paste0("NHANES adult master dataset\nn = ", master_n),
    paste0("Excluded\nn = ", excluded_n, "\nmissing all key anthropometry\nor all outcome data"),
    paste0("Eligible analytic dataset\nn = ", analytic_n),
    "Outcome-specific complete cases\nreported per model"
  ),
  x = c(1, 2.05, 1, 1),
  y = c(3, 2.45, 2, 1)
)

main_flow <- flow |>
  dplyr::filter(.data$step != "Excluded from analytic dataset")

excluded_flow <- flow |>
  dplyr::filter(.data$step == "Excluded from analytic dataset")

p_flow <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = tibble::tibble(x = 1, xend = 1, y = c(2.75, 1.75), yend = c(2.25, 1.25)),
    ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    arrow = ggplot2::arrow(length = grid::unit(0.16, "inches")),
    linewidth = 0.6,
    color = "#555555",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_segment(
    data = tibble::tibble(x = 1.32, xend = 1.73, y = 2.75, yend = 2.52),
    ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    arrow = ggplot2::arrow(length = grid::unit(0.12, "inches")),
    linewidth = 0.5,
    color = "#777777",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_label(
    data = main_flow,
    ggplot2::aes(x, y, label = label),
    fill = "#F7F8FA",
    color = "#222222",
    label.size = 0.35,
    label.r = grid::unit(0.08, "inches"),
    size = 4.1,
    lineheight = 0.95
  ) +
  ggplot2::geom_label(
    data = excluded_flow,
    ggplot2::aes(x, y, label = label),
    fill = "#FFFFFF",
    color = "#333333",
    label.size = 0.3,
    label.r = grid::unit(0.08, "inches"),
    size = 3.3,
    lineheight = 0.95
  ) +
  ggplot2::coord_cartesian(xlim = c(0.25, 2.6), ylim = c(0.6, 3.4)) +
  ggplot2::theme_void()
ggplot2::ggsave("figures/figure_1_study_flow.png", p_flow, width = 6.8, height = 5.2, dpi = 300, bg = "white")

p_dist <- analysis |>
  dplyr::select(bmi, waist_cm, waist_hip_ratio) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "measure", values_to = "value") |>
  dplyr::mutate(measure = factor(
    measure,
    levels = c("bmi", "waist_cm", "waist_hip_ratio"),
    labels = c("Body mass index (kg/m²)", "Waist circumference (cm)", "Waist-hip ratio")
  )) |>
  ggplot2::ggplot(ggplot2::aes(value)) +
  ggplot2::geom_histogram(bins = 35, fill = "#3B6EA8", color = "white") +
  ggplot2::facet_wrap(~measure, scales = "free", ncol = 1) +
  ggplot2::labs(x = "Value", y = "Participants")
ggplot2::ggsave("figures/figure_2_anthropometric_distributions.png", p_dist, width = 7, height = 8, dpi = 300)

cor_tbl <- readr::read_csv("results/anthropometric_spearman_correlations.csv", show_col_types = FALSE)
p_cor <- cor_tbl |>
  dplyr::mutate(
    measure_1 = dplyr::recode(
      measure_1,
      bmi = "BMI",
      waist_cm = "Waist circumference",
      waist_hip_ratio = "Waist-hip ratio"
    ),
    measure_2 = dplyr::recode(
      measure_2,
      bmi = "BMI",
      waist_cm = "Waist circumference",
      waist_hip_ratio = "Waist-hip ratio"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(measure_1, measure_2, fill = spearman_r)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", spearman_r)), size = 3.5) +
  ggplot2::scale_fill_gradient2(low = "#A23E48", mid = "white", high = "#2E7D68", midpoint = 0, limits = c(-1, 1)) +
  ggplot2::labs(x = NULL, y = NULL, fill = "Correlation coefficient") +
  ggplot2::coord_equal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = ggplot2::element_text(size = 9),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 8),
    plot.margin = ggplot2::margin(8, 12, 8, 8)
  )
ggplot2::ggsave("figures/figure_3_anthropometric_correlation_heatmap.png", p_cor, width = 6.8, height = 4.6, dpi = 300)

prev_tbl <- readr::read_csv("tables/table_2_outcome_prevalence_by_anthropometric_categories.csv", show_col_types = FALSE)
p_prev <- prev_tbl |>
  dplyr::mutate(
    outcome = factor(
      outcome,
      levels = c("Hypertension", "Diabetes", "Chronic kidney disease", "Cardiometabolic multimorbidity")
    ),
    category = factor(
      category,
      levels = c(
        "Underweight", "Normal weight", "Overweight", "Obesity",
        "Below high-risk cutoff", "High waist circumference", "High waist-hip ratio"
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(prevalence_percent, category, fill = outcome)) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge2(width = 0.82, preserve = "single", padding = 0.08),
    width = 0.74
  ) +
  ggplot2::facet_wrap(~category_type, scales = "free_y", ncol = 1) +
  ggplot2::scale_fill_manual(values = c(
    "Hypertension" = "#0B3C5D",
    "Diabetes" = "#1D5F8A",
    "Chronic kidney disease" = "#4D8FBA",
    "Cardiometabolic multimorbidity" = "#9CC3DD"
  )) +
  ggplot2::labs(x = "Weighted prevalence (%)", y = NULL, fill = NULL) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 9),
    axis.text.x = ggplot2::element_text(size = 9),
    strip.text = ggplot2::element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 9),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.spacing.y = grid::unit(0.55, "lines"),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA)
  )
ggplot2::ggsave("figures/figure_4_outcome_prevalence_by_anthropometry.png", p_prev, width = 8.5, height = 7.4, dpi = 300, bg = "white")

reg_tbl <- readr::read_csv("tables/table_3_survey_weighted_adjusted_associations.csv", show_col_types = FALSE)
p_forest <- reg_tbl |>
  ggplot2::ggplot(ggplot2::aes(odds_ratio, predictor, xmin = conf_low, xmax = conf_high)) +
  ggplot2::geom_vline(xintercept = 1, linetype = 2, color = "grey50") +
  ggplot2::geom_errorbarh(height = 0.18, color = "#555555") +
  ggplot2::geom_point(size = 2.2, color = "#2E7D68") +
  ggplot2::facet_wrap(~outcome) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(x = "Adjusted odds ratio (log scale)", y = NULL, title = "Adjusted associations between anthropometric measures and outcomes")
ggplot2::ggsave("figures/figure_6_adjusted_odds_ratios_forest.png", p_forest, width = 9, height = 6, dpi = 300)

box_data <- analysis |>
  dplyr::select(cardiometabolic_multimorbidity, bmi, waist_cm, waist_hip_ratio) |>
  tidyr::pivot_longer(c(bmi, waist_cm, waist_hip_ratio), names_to = "measure", values_to = "value") |>
  dplyr::mutate(
    cardiometabolic_multimorbidity = dplyr::case_when(
      cardiometabolic_multimorbidity == 1 ~ "Multimorbidity",
      cardiometabolic_multimorbidity == 0 ~ "No multimorbidity",
      TRUE ~ NA_character_
    ),
    measure = dplyr::recode(
      measure,
      bmi = "Body mass index (kg/m²)",
      waist_cm = "Waist circumference (cm)",
      waist_hip_ratio = "Waist-hip ratio"
    )
  ) |>
  tidyr::drop_na()

p_box <- ggplot2::ggplot(box_data, ggplot2::aes(cardiometabolic_multimorbidity, value, fill = cardiometabolic_multimorbidity)) +
  ggplot2::geom_boxplot(width = 0.55, outlier.alpha = 0.12, outlier.size = 0.6) +
  ggplot2::facet_wrap(~measure, scales = "free_y", nrow = 1) +
  ggplot2::scale_fill_manual(values = c("No multimorbidity" = "#8FB9A8", "Multimorbidity" = "#C75D4D")) +
  ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )
ggplot2::ggsave("figures/figure_7_anthropometry_by_multimorbidity_boxplots.png", p_box, width = 9, height = 4.8, dpi = 300, bg = "white")

p_scatter <- analysis |>
  dplyr::mutate(
    multimorbidity_status = dplyr::case_when(
      cardiometabolic_multimorbidity == 1 ~ "Multimorbidity",
      cardiometabolic_multimorbidity == 0 ~ "No multimorbidity",
      TRUE ~ NA_character_
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(bmi, waist_cm, color = multimorbidity_status)) +
  ggplot2::geom_point(alpha = 0.28, size = 1.1, na.rm = TRUE) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, na.rm = TRUE) +
  ggplot2::scale_color_manual(values = c("No multimorbidity" = "#3B6EA8", "Multimorbidity" = "#A23E48"), na.value = "grey70") +
  ggplot2::labs(
    x = "Body mass index, kg/m²",
    y = "Waist circumference, cm",
    color = NULL
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA)
  )
ggplot2::ggsave("figures/figure_8_bmi_waist_scatter.png", p_scatter, width = 7.5, height = 5.5, dpi = 300, bg = "white")

age_prevalence <- purrr::imap_dfr(outcome_variables, function(outcome_label, outcome_var) {
  est <- survey::svyby(
    stats::as.formula(paste0("~", outcome_var)),
    ~age_group,
    design,
    survey::svymean,
    na.rm = TRUE,
    vartype = "se"
  )
  se_col <- grep("^se", names(est), value = TRUE)[1]
  tibble::tibble(
    age_group = as.character(est$age_group),
    outcome = outcome_label,
    prevalence_percent = 100 * est[[outcome_var]],
    se_percent = 100 * est[[se_col]]
  )
})
write_csv_safely(age_prevalence, "results/outcome_prevalence_by_age_group.csv")

p_age <- age_prevalence |>
  dplyr::mutate(
    age_group = factor(age_group, levels = c("20-39", "40-59", ">=60")),
    age_group = forcats::fct_recode(age_group, "60+" = ">=60"),
    outcome = factor(
      outcome,
      levels = c("Hypertension", "Diabetes", "Chronic kidney disease", "Cardiometabolic multimorbidity")
    ),
    label_color = dplyr::if_else(prevalence_percent >= 45, "white", "#17212B")
  ) |>
  ggplot2::ggplot(ggplot2::aes(age_group, outcome, fill = prevalence_percent)) +
  ggplot2::geom_tile(color = "white", linewidth = 1.1) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", prevalence_percent), color = label_color), size = 3.6) +
  ggplot2::scale_fill_gradient(low = "#DCEAF4", high = "#0B3C5D", name = "Weighted prevalence (%)") +
  ggplot2::scale_color_identity() +
  ggplot2::labs(x = "Age group, years", y = NULL) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 10),
    axis.text.y = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 8),
    panel.grid = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )
ggplot2::ggsave("figures/figure_9_outcome_prevalence_by_age_group.png", p_age, width = 7.8, height = 4.4, dpi = 300, bg = "white")

roc_curve_data <- purrr::map_dfr(names(outcome_variables), function(outcome) {
  purrr::pmap_dfr(anthro_predictors, function(predictor, label, scaled_variable, increment) {
    dat <- analysis |>
      dplyr::select(dplyr::all_of(c(outcome, scaled_variable))) |>
      tidyr::drop_na()
    model <- stats::glm(stats::as.formula(paste(outcome, "~", scaled_variable)), data = dat, family = binomial())
    risk <- stats::predict(model, type = "response")
    roc_curve_from_scores(dat[[outcome]], risk) |>
      dplyr::mutate(
      outcome = outcome_variables[[outcome]],
      predictor = label
    )
  })
})

p_roc <- roc_curve_data |>
  ggplot2::ggplot(ggplot2::aes(false_positive_rate, sensitivity, color = predictor)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey70") +
  ggplot2::geom_path(linewidth = 0.8) +
  ggplot2::facet_wrap(~outcome) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    x = "1 - specificity",
    y = "Sensitivity",
    color = NULL
  )
ggplot2::ggsave("figures/figure_5_roc_curves.png", p_roc, width = 8, height = 7, dpi = 300, bg = "white")
write_csv_safely(roc_curve_data, "results/roc_curve_coordinates.csv")

write_csv_safely(flow, "results/study_flow_counts.csv")

writeLines(c(
  "Generated figures 1 through 9.",
  "ROC curves use anthropometric-only logistic models."
), "logs/07_figures_log.txt")
