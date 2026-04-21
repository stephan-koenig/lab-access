#' Load CfA students
#'
#' Load data frame of Faculty of Science students registered with Centre for
#' Accessiblity from September 2024 to August 2025.
#'
#' @returns A data frame.
#'
#' @export
load_students <- function() {
  here::here(
    "data/raw/cfa-student-data",
    paste0(
      "01_Active Students in Faculty of Science + program info ",
      "September 2024 - August 2025.xlsx"
    )
  ) |>
    readxl::read_xlsx(.name_repair = janitor::make_clean_names) |>
    dplyr::filter_out(
      program_of_study_name %in%
        c(
          "Unclassified (Vancouver)",
          # Master of Data Science is not an undergraduate program
          "Master of Data Science (Vancouver)"
        )
    )
}
