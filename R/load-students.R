load_students <- function() {
  here::here(
    "data/raw/cfa-student-data",
    paste0(
      "01_Active Students in Faculty of Science + program info ",
      "September 2024 - August 2025.xlsx"
    )
  ) |>
    readxl::read_xlsx(.name_repair = janitor::make_clean_names)
}
