#' Load CfA students
#'
#' Load data frame of Faculty of Science students registered with Centre for
#' Accessiblity from September 2024 to August 2025.
#'
#' @returns A data frame.
#'
#' @export
load_students <- function() {
  programs <- c(
    "Cellular, Anatomical and Physiological Sciences",
    "Astronomy",
    "Behavioural Neuroscience",
    "Biochemistry",
    "Biology",
    "Biophysics",
    "Biotechnology",
    "Cellular",
    "Chemical Biology",
    "Chemistry",
    "Cognitive Systems",
    "Computer Science",
    "Data Science",
    "Earth and Ocean Sciences",
    "Economics",
    "Environmental Sciences",
    "Forensic Science",
    "General Science in Earth Science", #check
    "General Science in Life Science", #check
    "Geographical Sciences",
    "Geology",
    "Geophysics",
    "Integrated Sciences",
    "Mathematics",
    "Microbiology",
    "Microbiology and Immunology",
    "Neuroscience",
    "Oceanography",
    "Pharmacology",
    "Physics",
    "Science",
    "Statistics"
  ) |>
    stringr::str_flatten(collapse = "|")

  students <- here::here(
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
    ) |>
    dplyr::mutate(
      program_to_be_processed = stringr::str_remove(
        program_of_study_name,
        stringr::coll(" (Vancouver)")
      ),
      department_name = stringr::str_remove(
        department_name,
        stringr::coll(" (Vancouver)")
      )
    ) |>
    tidyr::separate_wider_regex(
      program_to_be_processed,
      c(
        degree = ".*",
        ", ",
        degree_option = "(?:Combined )?Major|(?:Combined )?Honours",
        " in ",
        program_one = programs,
        "(?:, )?",
        option = "(?:Option in .*)?",
        program_two = programs
      ),
      too_few = "align_start"
    ) |>
    dplyr::mutate(
      program_two = dplyr::replace_when(
        program_two,
        # Track `Chemical Biology` and `Biophysics` as `Chemistry`/`Physics`
        # and `Biology` instead
        stringr::str_detect(program_one, "Chemical Biology") ~ "Biology",
        stringr::str_detect(program_one, "Biophysics") ~ "Biology"
      ),
      program_one = dplyr::replace_when(
        program_one,
        stringr::str_detect(
          program_of_study_name,
          "General Science in Earth Science"
        ) ~ "General Science in Earth Science",
        stringr::str_detect(
          program_of_study_name,
          "General Science in Life Science"
        ) ~ "General Science in Life Science",
        stringr::str_detect(
          program_one,
          "Microbiology"
        ) ~ "Microbiology and Immunology",
        # Track `Chemical Biology` and `Biophysics` as `Chemistry`/`Physics`
        # and `Biology` instead
        stringr::str_detect(program_one, "Chemical Biology") ~ "Chemistry",
        stringr::str_detect(program_one, "Biophysics") ~ "Physics",
        stringr::str_detect(degree, "Bachelor of Science") ~ "First year",
        is.na(program_one) ~ degree
      ),
      dplyr::across(
        tidyselect::where(is.character),
        ~ dplyr::na_if(.x, "")
      )
    )

  department_lookup <- students |>
    dplyr::distinct(department_name, program_one) |>
    dplyr::rename(program = program_one) |>
    tidyr::drop_na()

  students |>
    dplyr::mutate(
      department_two = dplyr::recode_values(
        program_two,
        from = department_lookup$program,
        to = department_lookup$department_name
      ),
      .before = program_two
    )
}
