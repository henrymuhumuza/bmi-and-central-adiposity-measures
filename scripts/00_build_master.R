source("R/analysis_helpers.R")
check_required_packages()
ensure_project_dirs()

nhanes_files <- c(
  "DEMO_L", "BMX_L", "BPXO_L", "BPQ_L", "DIQ_L", "GHB_L",
  "BIOPRO_L", "ALB_CR_L", "TCHOL_L", "HDL_L", "TRIGLY_L",
  "SMQ_L", "ALQ_L", "PAQ_L", "SLQ_L", "DR1TOT_L"
)

nhanes_url <- function(file_id) {
  paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/", file_id, ".xpt")
}

download_nhanes_file <- function(file_id) {
  path <- file.path("data/raw", paste0(file_id, ".xpt"))
  if (!file.exists(path)) {
    message("Downloading ", file_id, ".xpt")
    utils::download.file(nhanes_url(file_id), path, mode = "wb", quiet = TRUE)
  }
  path
}

read_component <- function(file_id) {
  out <- haven::read_xpt(download_nhanes_file(file_id))
  names(out) <- tolower(names(out))
  out
}

clean_yes_no <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 0L,
    TRUE ~ NA_integer_
  )
}

row_mean <- function(data, vars) {
  vals <- dplyr::select(data, dplyr::all_of(vars))
  out <- rowMeans(vals, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

egfr_2021 <- function(scr, age, sex) {
  female <- sex == "Female"
  kappa <- dplyr::if_else(female, 0.7, 0.9, missing = 0.9)
  alpha <- dplyr::if_else(female, -0.241, -0.302, missing = -0.302)
  sex_factor <- dplyr::if_else(female, 1.012, 1.0, missing = 1.0)
  142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^(-1.200) * 0.9938^age * sex_factor
}

components <- purrr::map(nhanes_files, read_component) |>
  rlang::set_names(tolower(nhanes_files))

master <- components$demo_l |>
  dplyr::select(
    seqn, sddsrvyr, ridstatr, riagendr, ridageyr, ridagemn, ridreth3,
    dmdeduc2, dmdmartz, wtint2yr, wtmec2yr, sdmvpsu, sdmvstra, indfmpir
  ) |>
  dplyr::left_join(dplyr::select(components$bmx_l, seqn, bmxbmi, bmxwaist, bmxhip), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$bpxo_l, seqn, bpxosy1, bpxosy2, bpxosy3, bpxodi1, bpxodi2, bpxodi3), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$bpq_l, seqn, bpq020, bpq150), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$diq_l, seqn, diq010, diq050, diq070), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$ghb_l, seqn, lbxgh), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$biopro_l, seqn, lbxscr), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$alb_cr_l, seqn, urdact), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$tchol_l, seqn, lbxtc), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$hdl_l, seqn, lbdhdd), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$trigly_l, seqn, lbxtlg, lbdldl), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$smq_l, seqn, smq020, smq040), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$alq_l, seqn, alq111, alq130, alq142), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$paq_l, seqn, pad790q, pad800, pad810q, pad820, pad680), by = "seqn") |>
  dplyr::left_join(dplyr::select(components$slq_l, seqn, sld012, sld013), by = "seqn") |>
  dplyr::left_join(
    dplyr::select(
      components$dr1tot_l, seqn, wtdrd1, dr1tkcal, dr1tprot, dr1tcarb,
      dr1tsugr, dr1tfibe, dr1ttfat, dr1tsfat, dr1tchol, dr1tsodi,
      dr1tpota, dr1talco
    ),
    by = "seqn"
  ) |>
  dplyr::mutate(
    age = ridageyr,
    sex = dplyr::case_when(riagendr == 1 ~ "Male", riagendr == 2 ~ "Female", TRUE ~ NA_character_),
    race_ethnicity = dplyr::case_when(
      ridreth3 == 1 ~ "Mexican American",
      ridreth3 == 2 ~ "Other Hispanic",
      ridreth3 == 3 ~ "Non-Hispanic White",
      ridreth3 == 4 ~ "Non-Hispanic Black",
      ridreth3 == 6 ~ "Non-Hispanic Asian",
      ridreth3 == 7 ~ "Other or multiracial",
      TRUE ~ NA_character_
    ),
    education = dplyr::case_when(
      dmdeduc2 %in% 1:2 ~ "Less than high school",
      dmdeduc2 == 3 ~ "High school/GED",
      dmdeduc2 == 4 ~ "Some college/AA",
      dmdeduc2 == 5 ~ "College graduate or above",
      TRUE ~ NA_character_
    ),
    marital_status = dplyr::case_when(
      dmdmartz == 1 ~ "Married/living with partner",
      dmdmartz %in% c(2, 3) ~ "Formerly married",
      dmdmartz == 4 ~ "Never married",
      TRUE ~ NA_character_
    ),
    pir = indfmpir,
    bmi = bmxbmi,
    waist_cm = bmxwaist,
    hip_cm = bmxhip,
    bmi_category = dplyr::case_when(
      is.na(bmi) ~ NA_character_,
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal weight",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obesity"
    ),
    obesity = as.integer(bmi >= 30),
    central_obesity = dplyr::case_when(
      sex == "Female" & waist_cm >= 88 ~ 1L,
      sex == "Male" & waist_cm >= 102 ~ 1L,
      !is.na(sex) & !is.na(waist_cm) ~ 0L,
      TRUE ~ NA_integer_
    ),
    mean_sbp = row_mean(dplyr::pick(dplyr::everything()), c("bpxosy1", "bpxosy2", "bpxosy3")),
    mean_dbp = row_mean(dplyr::pick(dplyr::everything()), c("bpxodi1", "bpxodi2", "bpxodi3")),
    htn_diagnosis = clean_yes_no(bpq020),
    htn_med = clean_yes_no(bpq150),
    hypertension = dplyr::case_when(
      mean_sbp >= 140 | mean_dbp >= 90 | htn_diagnosis == 1 | htn_med == 1 ~ 1L,
      !is.na(mean_sbp) | !is.na(mean_dbp) | !is.na(htn_diagnosis) | !is.na(htn_med) ~ 0L,
      TRUE ~ NA_integer_
    ),
    hba1c = lbxgh,
    diabetes_diagnosis = clean_yes_no(diq010),
    insulin_use = clean_yes_no(diq050),
    diabetes_pills = clean_yes_no(diq070),
    diabetes = dplyr::case_when(
      hba1c >= 6.5 | diabetes_diagnosis == 1 | insulin_use == 1 | diabetes_pills == 1 ~ 1L,
      !is.na(hba1c) | !is.na(diabetes_diagnosis) | !is.na(insulin_use) | !is.na(diabetes_pills) ~ 0L,
      TRUE ~ NA_integer_
    ),
    serum_creatinine_mgdl = lbxscr,
    egfr = egfr_2021(serum_creatinine_mgdl, age, sex),
    acr_mg_g = urdact,
    albuminuria = dplyr::case_when(
      acr_mg_g >= 30 ~ 1L,
      !is.na(acr_mg_g) ~ 0L,
      TRUE ~ NA_integer_
    ),
    ckd = dplyr::case_when(
      egfr < 60 | albuminuria == 1 ~ 1L,
      !is.na(egfr) | !is.na(albuminuria) ~ 0L,
      TRUE ~ NA_integer_
    ),
    total_cholesterol_mgdl = lbxtc,
    hdl_mgdl = lbdhdd,
    triglycerides_mgdl = lbxtlg,
    ldl_mgdl = lbdldl,
    dyslipidemia = dplyr::case_when(
      total_cholesterol_mgdl >= 240 | hdl_mgdl < 40 | triglycerides_mgdl >= 200 | ldl_mgdl >= 160 ~ 1L,
      !is.na(total_cholesterol_mgdl) | !is.na(hdl_mgdl) | !is.na(triglycerides_mgdl) | !is.na(ldl_mgdl) ~ 0L,
      TRUE ~ NA_integer_
    ),
    smoking_status = dplyr::case_when(
      smq020 == 2 ~ "Never",
      smq020 == 1 & smq040 == 3 ~ "Former",
      smq020 == 1 & smq040 %in% c(1, 2) ~ "Current",
      TRUE ~ NA_character_
    ),
    alcohol_ever = clean_yes_no(alq111),
    alcohol_drinks_per_day = alq130,
    binge_drinking_days = alq142,
    moderate_pa_freq = pad790q,
    moderate_pa_minutes = pad800,
    vigorous_pa_freq = pad810q,
    vigorous_pa_minutes = pad820,
    sitting_minutes_day = pad680,
    sleep_hours_weekday = sld012,
    sleep_hours_weekend = sld013,
    sleep_category = dplyr::case_when(
      sleep_hours_weekday < 7 ~ "Short sleep",
      sleep_hours_weekday <= 9 ~ "Recommended sleep",
      sleep_hours_weekday > 9 ~ "Long sleep",
      TRUE ~ NA_character_
    ),
    total_energy_kcal = dr1tkcal,
    protein_g = dr1tprot,
    carbohydrate_g = dr1tcarb,
    total_sugar_g = dr1tsugr,
    fiber_g = dr1tfibe,
    total_fat_g = dr1ttfat,
    saturated_fat_g = dr1tsfat,
    cholesterol_mg = dr1tchol,
    sodium_mg = dr1tsodi,
    potassium_mg = dr1tpota,
    alcohol_g_diet = dr1talco
  ) |>
  dplyr::mutate(
    cardiometabolic_count_available = rowSums(!is.na(dplyr::pick(hypertension, diabetes, ckd))),
    cardiometabolic_count_present = rowSums(dplyr::pick(hypertension, diabetes, ckd), na.rm = TRUE),
    cardiometabolic_count_absent = rowSums(dplyr::pick(hypertension, diabetes, ckd) == 0, na.rm = TRUE),
    cardiometabolic_missing_count = 3L - cardiometabolic_count_available,
    cardiometabolic_multimorbidity = dplyr::case_when(
      cardiometabolic_count_present >= 2 ~ 1L,
      cardiometabolic_count_available == 3 & cardiometabolic_count_present < 2 ~ 0L,
      cardiometabolic_count_present == 0 & cardiometabolic_count_absent >= 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  dplyr::filter(age >= 20) |>
  dplyr::transmute(
    seqn, wtint2yr, wtmec2yr, wtdrd1, sdmvpsu, sdmvstra, age, sex,
    race_ethnicity, education, marital_status, pir, bmi, waist_cm, hip_cm,
    bmi_category, obesity, central_obesity, mean_sbp, mean_dbp, htn_diagnosis,
    htn_med, hypertension, hba1c, diabetes_diagnosis, insulin_use,
    diabetes_pills, diabetes, serum_creatinine_mgdl, egfr, acr_mg_g,
    albuminuria, ckd, total_cholesterol_mgdl, hdl_mgdl, triglycerides_mgdl,
    ldl_mgdl, dyslipidemia, smoking_status, alcohol_ever,
    alcohol_drinks_per_day, binge_drinking_days, moderate_pa_freq,
    moderate_pa_minutes, vigorous_pa_freq, vigorous_pa_minutes,
    sitting_minutes_day, sleep_hours_weekday, sleep_hours_weekend,
    sleep_category, total_energy_kcal, protein_g, carbohydrate_g,
    total_sugar_g, fiber_g, total_fat_g, saturated_fat_g, cholesterol_mg,
    sodium_mg, potassium_mg, alcohol_g_diet, cardiometabolic_count_available,
    cardiometabolic_missing_count, cardiometabolic_multimorbidity
  )

write_csv_safely(master, "data/processed/nhanes_master.csv")
save_rds_safely(master, "data/processed/nhanes_master.rds")
message("Wrote data/processed/nhanes_master.csv and .rds from public-use NHANES files.")
