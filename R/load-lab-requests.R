load_lab_requests <- function() {
  here::here(
    "data/raw/cfa-student-data",
    paste0(
      "02_Students with accommodations in Laboratory sections ",
      "September 2024 - August 2025.xlsx"
    )
  ) |>
    readxl::read_xlsx(.name_repair = janitor::make_clean_names)
}
