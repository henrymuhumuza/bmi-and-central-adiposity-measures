renv_lib <- file.path("renv", "library", "R-4.4", "x86_64-w64-mingw32")
if (dir.exists(renv_lib)) {
  .libPaths(c(normalizePath(renv_lib), .libPaths()))
}

required_packages <- c(
  "readr", "dplyr", "tidyr", "stringr", "forcats", "purrr",
  "survey", "broom", "ggplot2", "tibble", "haven"
)

check_required_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Install required R packages before running this script: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_project_dirs <- function() {
  dirs <- c(
    "data/raw", "data/interim", "data/processed", "R", "scripts",
    "figures", "tables", "results", "manuscript", "references",
    "outputs", "outputs/manuscript", "logs"
  )
  invisible(lapply(dirs, dir_create))
}

read_master_data <- function() {
  if (file.exists("data/processed/nhanes_master.rds")) {
    readRDS("data/processed/nhanes_master.rds")
  } else if (file.exists("data/processed/nhanes_master.csv")) {
    readr::read_csv("data/processed/nhanes_master.csv", show_col_types = FALSE)
  } else {
    stop("Could not find data/processed/nhanes_master.rds or .csv.", call. = FALSE)
  }
}

write_csv_safely <- function(x, path) {
  dir_create(dirname(path))
  readr::write_csv(x, path, na = "")
  invisible(path)
}

save_rds_safely <- function(x, path) {
  dir_create(dirname(path))
  saveRDS(x, path)
  invisible(path)
}

as_binary_numeric <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) return(ifelse(is.na(x), NA_integer_, as.integer(x != 0)))
  y <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    y %in% c("1", "yes", "y", "true", "present", "case") ~ 1L,
    y %in% c("0", "no", "n", "false", "absent", "noncase") ~ 0L,
    TRUE ~ NA_integer_
  )
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

roc_curve_from_scores <- function(outcome, score) {
  keep <- !is.na(outcome) & !is.na(score)
  outcome <- as.integer(outcome[keep])
  score <- as.numeric(score[keep])
  thresholds <- c(Inf, sort(unique(score), decreasing = TRUE), -Inf)
  positives <- sum(outcome == 1)
  negatives <- sum(outcome == 0)
  if (positives == 0 || negatives == 0) {
    return(tibble::tibble(threshold = numeric(), sensitivity = numeric(), specificity = numeric(), false_positive_rate = numeric()))
  }
  tibble::tibble(threshold = thresholds) |>
    dplyr::mutate(
      predicted = purrr::map(threshold, \(t) as.integer(score >= t)),
      tp = purrr::map_int(predicted, \(p) sum(p == 1 & outcome == 1)),
      fp = purrr::map_int(predicted, \(p) sum(p == 1 & outcome == 0)),
      sensitivity = tp / positives,
      specificity = 1 - fp / negatives,
      false_positive_rate = 1 - specificity
    ) |>
    dplyr::select(threshold, sensitivity, specificity, false_positive_rate)
}

auc_from_roc <- function(roc_data) {
  if (nrow(roc_data) < 2) return(NA_real_)
  dat <- roc_data |>
    dplyr::arrange(false_positive_rate, sensitivity)
  sum(diff(dat$false_positive_rate) * (head(dat$sensitivity, -1) + tail(dat$sensitivity, -1)) / 2)
}

auc_from_scores <- function(outcome, score) {
  keep <- !is.na(outcome) & !is.na(score)
  outcome <- as.integer(outcome[keep])
  score <- as.numeric(score[keep])
  n_pos <- sum(outcome == 1)
  n_neg <- sum(outcome == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[outcome == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

bootstrap_auc_ci <- function(outcome, score, n_boot = 200, seed = 20260630) {
  keep <- !is.na(outcome) & !is.na(score)
  outcome <- as.integer(outcome[keep])
  score <- as.numeric(score[keep])
  if (length(unique(outcome)) < 2) return(c(NA_real_, NA_real_, NA_real_))
  set.seed(seed)
  n <- length(outcome)
  aucs <- replicate(n_boot, {
    idx <- sample.int(n, n, replace = TRUE)
    if (length(unique(outcome[idx])) < 2) return(NA_real_)
    auc_from_scores(outcome[idx], score[idx])
  })
  stats::quantile(aucs, probs = c(0.025, 0.5, 0.975), na.rm = TRUE, names = FALSE)
}

svy_design <- function(data) {
  survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    nest = TRUE,
    data = data
  )
}

weighted_mean_se <- function(design, variable) {
  est <- survey::svymean(stats::as.formula(paste0("~", variable)), design, na.rm = TRUE)
  tibble::tibble(
    estimate = as.numeric(stats::coef(est)),
    se = as.numeric(survey::SE(est))
  )
}

weighted_binary_percent <- function(design, variable) {
  est <- survey::svymean(stats::as.formula(paste0("~", variable)), design, na.rm = TRUE)
  tibble::tibble(
    percent = 100 * as.numeric(stats::coef(est)),
    se = 100 * as.numeric(survey::SE(est))
  )
}

weighted_quantile <- function(design, variable, quantiles = c(0.25, 0.5, 0.75)) {
  q <- survey::svyquantile(
    stats::as.formula(paste0("~", variable)),
    design,
    quantiles = quantiles,
    na.rm = TRUE,
    ci = FALSE
  )
  as.numeric(q[[1]])
}

outcome_variables <- c(
  hypertension = "Hypertension",
  diabetes = "Diabetes",
  ckd = "Chronic kidney disease",
  cardiometabolic_multimorbidity = "Cardiometabolic multimorbidity"
)

anthro_predictors <- tibble::tribble(
  ~predictor, ~label, ~scaled_variable, ~increment,
  "bmi", "Body mass index", "bmi_5", "per 5 kg/m²",
  "waist_cm", "Waist circumference", "waist_10", "per 10 cm",
  "waist_hip_ratio", "Waist-hip ratio", "whr_01", "per 0.1 unit"
)

adjustment_terms <- c("age", "sex", "race_ethnicity", "education", "pir")
