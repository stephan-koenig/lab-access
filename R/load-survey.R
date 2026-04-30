#' Load Qualtrics survey data
#'
#' Load data frame of Qualtrics survey data. If no local copy exists, also
#' downloads survey from Qualtrics and saves a local copy.
#'
#' @param survey_name Survey name on Qualtrics.
#' @param file File to write to or load from.
#'
#' @returns A data frame.
#'
#' @export
load_survey <- function(survey_name, file) {
  survey_file <- here::here(file)

  if (fs::file_exists(survey_file)) {
    survey <- qs2::qs_read(survey_file) |>
      janitor::clean_names()
    return(survey)
  }

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
  survey |>
    janitor::clean_names()
}
