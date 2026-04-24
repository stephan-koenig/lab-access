#' Load lab accessibility survey of undergraduates in FoS
#'
#' Load data frame of lab accessibility Qualtrics survey data of undergraduates
#' in the Faculty of Science (FoS) conducted by Skylight in March 2026.
#'
#' @returns A data frame.
#'
#' @export
load_survey <- function() {
  survey_file <- here::here(
    "data",
    "raw",
    "survey",
    "lab-accessibility-in-science-undergraduate-student-survey.qs2"
  )

  if (fs::file_exists(survey_file)) {
    survey <- qs2::qs_read(survey_file) |>
      janitor::clean_names()
    return(survey)
  }

  survey_name <- "Lab Accessibility in Science-Student Survey March 2026"

  qualtRics::qualtrics_api_credentials(
    api_key = keyring::key_get("qualtrics_api_key"),
    base_url = "ubc.yul1.qualtrics.com"
  )

  surveys <- qualtRics::all_surveys()
  survey_id <- surveys$id[surveys$name == survey_name]
  survey <- qualtRics::fetch_survey(
    survey_id,
    breakout_sets = FALSE
  )
  qs2::qs_save(survey, survey_file)
  survey
}
