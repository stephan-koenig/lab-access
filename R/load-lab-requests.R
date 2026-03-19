#' Load status of CfA requests in lab courses
#'
#' Load data frame of Faculty of Science students registered with Centre for
#' Accessiblity (CfA) and their accomodation request status in lab courses from
#' September 2024 to August 2025.
#'
#' @returns A data frame.
#'
#' @export
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
