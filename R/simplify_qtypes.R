#' Simplify Question Types
#' @description Sometimes it's nice to categorize the questions of a Qualtrics
#' survey. This takes the results of \code{flatten_survey} and builds a table
#' that lists:
#' \itemize{
#'   \item \code{question_id}
#'   \item \code{column_number} for side-by-side questions
#'   \item \code{question_style} as one of \code{c('radio', 'checkbox', 'text', 'signature', 'descriptive')}
#'   \item \code{question_matrix} indicating whether the question is a matrix/side-by-side
#' }
#' @param survey_id string of the survey id, begins with 'SV_'
#' @return a \code{tibble}
#' @export "simplify_qtypes"

simplify_qtypes = function(survey_id){

  # objective is to return a data frame with these columns:
  # question_id
  # column_number
  # question_style: c('radio', 'checkbox', 'text', 'signature', 'descriptive')
  # question_matrix: binary, is the question a matrix?

  # argument check:
  stopifnot(
    all(
      valid_survey_id(survey_id),
      valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
      valid_base_url(Sys.getenv('QUALTRICS_BASE_URL'))
    )
  )

  flatten_survey(survey_id) %>%
    purrr::pluck('questions') %>%
    dplyr::left_join(qtype_cross) %>%
    # note we're taking distinct here since the column numbers will be
    # repeated for each subquestion. Want to be able to smothly join onto
    # the flattened questions if that's desired.
    dplyr::distinct(question_id, column_number, question_style, question_matrix)



}

# TODO: delete this eventually when we're satisfied with the mapping:
update_qtype_cross = function(){

  qtype_cross = readr::read_csv(
    here::here('data/qtype_cross.csv')
  )

  usethis::use_data(qtype_cross, internal = TRUE)

  invisible(NULL)

}
