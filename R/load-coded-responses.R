load_coded_responses <- function(file, sheet_names) {
  purrr::map(
    sheet_names,
    \(sheet) {
      readxl::read_xlsx(
        file,
        sheet = sheet,
        skip = 1
      ) |>
        janitor::clean_names()
    }
  ) |>
    purrr::reduce(
      \(x, y) {
        dplyr::full_join(
          x,
          y,
          by = dplyr::join_by(
            response_id
          ),
          relationship = "one-to-one"
        )
      }
    )
}
