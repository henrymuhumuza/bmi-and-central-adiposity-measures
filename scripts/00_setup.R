source("R/analysis_helpers.R")

check_required_packages()
ensure_project_dirs()

options(survey.lonely.psu = "adjust")

message("Project directories checked.")
message("Required R packages are available.")
