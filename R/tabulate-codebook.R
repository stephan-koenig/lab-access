tabulate_codebook <- function(for_question) {
  readxl::read_xlsx(
    here::here(
      "data",
      "raw",
      "survey",
      paste0(
        "RAW data_LabAccessibility in Science-Student Survey ",
        "March 2026_April 1 2026.xlsx"
      )
    ),
    sheet = "codebook"
  ) |>
    dplyr::filter(question == for_question) |>
    dplyr::select(!question) |>
    labelled::set_variable_labels(
      theme = "Theme",
      code = "Code",
      definition = "Definition",
      example_quote = "Example quote"
    ) |>
    gt::gt(groupname_col = "theme", row_group_as_column = T) |>
    gt::tab_stubhead("Theme") |>
    gt::sub_missing(missing_text = "") |>
    gt::fmt_markdown(columns = "example_quote") |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(gt::cells_column_labels(), gt::cells_stubhead())
    ) |>
    gt::tab_style(
      style = gt::cell_text(v_align = "top"),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_options(
      quarto.disable_processing = TRUE,
      stub_row_group.border.width = 0,
      table.font.size = gt::pct(75)
    )
}
