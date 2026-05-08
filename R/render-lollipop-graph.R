render_lollipop_graph <- function(data, column) {
  data |>
    dplyr::count({{ column }}) |>
    dplyr::mutate(
      proportion = n / sum(n),
      "{{ column }}" := forcats::fct_rev({{ column }})
    ) |>
    ggplot2::ggplot(aes(x = n, y = {{ column }})) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = n, y = {{ column }}, yend = {{ column }}),
      color = "gray",
      lwd = 1
    ) +
    ggplot2::geom_point(size = 5, pch = 21, bg = 4, col = 1) +
    ggplot2::geom_text(
      ggplot2::aes(label = n),
      color = "white",
      size = ggplot2::rel(2)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(proportion, accuracy = 1)),
      color = "black",
      size = ggplot2::rel(2),
      hjust = 0,
      nudge_x = 20
    ) +
    ggplot2::theme_minimal()
}
