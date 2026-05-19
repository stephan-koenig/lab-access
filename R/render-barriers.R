render_barriers <- function(data) {
  data |>
    # TODO: Add N for each question
    labelled::update_variable_labels_with(\(column_name) {
      wrap = 35

      preamble <- paste0(
        "7[abc]. To what extent, if any, the following course elements have ",
        "presented barriers to your full participation in a laboratory course ",
        "in your intended or chosen major\\? *\n\n\n\nPlease use the following ",
        "scale: *\nNo Impact: did not affect my participation\n\nMinor Impact: ",
        "small inconvenience\n\nModerate Impact: some difficulty ",
        "participating\n\nMajor Impact: significant difficulty ",
        "participating\n\nNot Applicable: did not experience this - "
      )

      element <- column_name |>
        stringr::str_remove(preamble)

      category <- element |>
        stringr::str_extract(".*?(?=\\s*\\()") |>
        stringr::str_wrap(width = wrap)
      examples <- element |>
        stringr::str_remove(".*?\\s*(?=\\()") |>
        stringr::str_wrap(width = wrap)

      paste0("**", category, "**\n", examples) |>
        stringr::str_replace_all("\n", "<br>")
    }) |>
    ggstats::gglikert(
      cutoff = 0,
      add_totals = FALSE
    ) +
    ggplot2::labs(
      title = paste0(
        "To what extent, if any, the following course elements ",
        "have presented barriers to your full participation in a laboratory ",
        "course in your intended or chosen major?"
      )
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(),
      plot.title.position = "plot",
      axis.text.y = ggtext::element_markdown(),
      legend.location = "plot"
    )
}
