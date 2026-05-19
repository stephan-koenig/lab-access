#' Load lab accessibility survey of undergraduates in FoS
#'
#' Load data frame of lab accessibility Qualtrics survey data of undergraduates
#' in the Faculty of Science (FoS) conducted by Skylight in March 2026.
#'
#' @returns A data frame.
#'
#' @export
load_undergraduate_lab_accessibility_survey <- function() {
  survey <- load_survey(
    "Lab Accessibility in Science-Student Survey March 2026",
    here::here(
      "data",
      "raw",
      "survey",
      "lab-accessibility-in-science-undergraduate-student-survey.qs2"
    )
  )

  column_map <- qualtRics::extract_colmap(survey)
  likert_levels <- survey$q4 |> levels()
  impact_levels <- c(
    "No Impact",
    "Minor Impact",
    "Moderate Impact",
    "Major Impact",
    "Not Applicable"
  )

  processed_survey <- survey |>
    # TODO: Not quite sure yet if that is the best cutoff
    dplyr::filter_out(progress < 43) |>
    tidyr::separate_wider_regex(
      q2,
      patterns = c(
        program = ".*",
        ",(?! )",
        program_two = ".*",
        ",(?! )",
        additional_programs = ".*"
      ),
      too_few = "align_start"
    ) |>
    dplyr::mutate(
      q2_10_text = dplyr::replace_when(
        q2_10_text,
        q2_10_text %in%
          c("Biophysics", "Combined major in science") ~ "Combined Major",
        q2_10_text %in% c("COGS", "Cognitive Systems") ~ "Cognitive Systems",
        stringr::str_detect(
          q2_10_text,
          "Geographical Sciences"
        ) ~ "Geographical Sciences",
        stringr::str_detect(
          q2_10_text,
          "Integrated Science"
        ) ~ "Integrated Sciences",
        q2_10_text == "neuroscience" ~ "Biological Sciences",
        q2_10_text == "first year undeclared" ~ "Undecided",
        # TODO: Double-check some of the other programs as they might not be FoS
        !is.na(q2_10_text) ~ "Other"
      ),
      program = dplyr::replace_when(
        program,
        # Interpret answers with more than two programs as undecided
        !is.na(additional_programs) ~ "Undecided",
        # TODO: Decide if to further resolve `Combined Major`
        !is.na(program_two) ~ "Combined Major",
        !is.na(q2_10_text) ~ q2_10_text
      ) |>
        stringr::str_remove(" \\(.*\\)") |>
        forcats::fct_relevel(c("Other", "Undecided"), after = Inf),
      q1_5_text = dplyr::replace_when(
        q1_5_text,
        q1_5_text == "Science One" ~ "Science One",
        q1_5_text ==
          "gap year due to health issues" ~ "Not taking any classes (e.g., Co-Op)",
        !is.na(
          q1_5_text
        ) ~ "Mix (e.g., multiple course levels, courses and Co-Op, etc.)"
      ),
      course_level_focus = dplyr::if_else(
        q1 == "Other, please specify:",
        q1_5_text,
        q1
      ) |>
        stringr::str_replace("taking", "Taking") |>
        forcats::fct_relevel(c(
          "Taking mostly or all 100-level courses",
          "Science One",
          "Taking mostly or all 200-level courses",
          "Taking mostly or all 300- and/or 400-level courses",
          "Not Taking any classes (e.g., Co-Op)",
          "Mix (e.g., multiple course levels, courses and Co-Op, etc.)",
          "Prefer not to say"
        )),
      dplyr::across(
        tidyselect::starts_with("q16"),
        \(column) {
          factor(column, levels = likert_levels, ordered = TRUE)
        }
      ),
      dplyr::across(
        tidyselect::starts_with(c("q11", "q30")),
        \(column) {
          factor(column, levels = impact_levels, ordered = TRUE)
        }
      )
    ) |>
    dplyr::select(!c(program_two, additional_programs)) |>
    labelled::copy_labels_from(survey) |>
    labelled::set_variable_labels(
      program = "Program area of current or intended major",
      course_level_focus = "Course level (a proxy for year in program)"
    )

  attr(processed_survey, "column_map") <- column_map

  processed_survey
}
