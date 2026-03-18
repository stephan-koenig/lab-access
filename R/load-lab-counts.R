load_lab_counts <- function() {
  here::here(
    "data/raw/cfa-student-data",
    "03_CfA Edits - Science_lab_counts_2024W_2025-07-10.xlsx"
  ) |>
    readxl::read_xlsx(
      .name_repair = janitor::make_clean_names,
      range = cellranger::cell_cols("A:H"),
    ) |>
    dplyr::select(!x) |>
    tidyr::separate_wider_regex(
      course,
      c(subject = "[A-Z]+_V", "_", course = "[0-9]+")
    ) |>
    dplyr::rename(
      cfa_students_n = number_cf_a_students_registered_september_2024_august_2025
    )
}
