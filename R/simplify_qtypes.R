#' Simplify Question Types
#' @description Sometimes it's nice to categorize the questions of a Qualtrics
#' survey. This takes the results of \code{flatten_survey} and builds a table
#' that lists:
#' \itemize{
#'   \item \code{question_id}: internal Qualtrics question id
#'   \item \code{column_number}: for side-by-side questions
#'   \item \code{question_style}: as one of \code{c('radio', 'checkbox', 'text', 'signature', 'descriptive')}
#'   \item \code{question_matrix}: indicating whether the question is a matrix. Note that all
#'   side-by-side questions are considered matrices even if they have only one column.
#'   \item \code{question_sbs}: indicating whether the question is a side-by-side
#'   \item \code{question_loop}: marks whether the question is in a block that
#'   has the "loop and merge" feature enabled.
#' }
#' @param survey_id string of the survey id, begins with 'SV_'
#' @param survey_flat the result of \code{qualtreats::flatten_survey}
#' @note Only one of \code{survey_id} or \code{survey_flat} is necessary.
#' Providing a flattened survey is offered just to save computational time.
#' @return a \code{tibble}
#' @export "simplify_qtypes"

simplify_qtypes = function(survey_id, survey_flat){

  # objective is to return a data frame with these columns:
  # question_id
  # column_number
  # question_style: c('radio', 'checkbox', 'text', 'signature', 'descriptive')
  # question_matrix: binary, is the question a matrix?
  # question_sbs: binary, is the question a side-by-side?

  # argument check:
  if(!missing(survey_flat)){
    stopifnot(
      all(
        identical(names(survey_flat), c("blocks", "questions", "choices")),
        purrr::map_lgl(survey_flat, is.data.frame)
      )
    )
  }

  if(!missing(survey_id)){
    stopifnot(
      all(
        valid_survey_id(survey_id),
        valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
        valid_base_url(Sys.getenv('QUALTRICS_BASE_URL'))
      )
    )
  }

  if(missing(survey_flat)){
    survey_flat = flatten_survey(survey_id)
  }

  simplified_qtypes = survey_flat %>%
    purrr::pluck('questions') %>%
    dplyr::left_join(
      qtype_cross,
      by = c(
        "question_type",
        "question_selector",
        "question_subselector",
        "column_type",
        "column_selector",
        "column_subselector"
      )
    ) %>%
    # note we're taking distinct here since the column numbers will be
    # repeated for each subquestion. Want to be able to smothly join onto
    # the flattened questions if that's desired.
    dplyr::distinct(question_id, column_number, question_style, question_matrix, question_sbs)

  # Join loop and merge from the blocks table:
  looped_question_ids = survey_flat[['questions']] %>%
    dplyr::distinct(question_id, block_id) %>%
    dplyr::left_join(
      y = survey_flat[['blocks']],
      by = 'block_id'
    ) %>%
    dplyr::transmute(
      question_id,
      question_loop = loop_and_merge
    )


  simplified_qtypes = dplyr::left_join(
      x = simplified_qtypes,
      y = looped_question_ids,
      by = 'question_id'
    )

  if(
    any(
      is.na(
        c(
          simplified_qtypes[['question_style']],
          simplified_qtypes[['question_matrix']],
          simplified_qtypes[['question_sbs']]
        )
      )
    )
  ){
    warning('Some questions have no associated question_style/matrix/sbs. DEV FIX THIS!')
  }

  simplified_qtypes

}

# TODO: delete this eventually when we're satisfied with the mapping:
# update_qtype_cross = function(){
#
#   qtype_cross = readr::read_csv(
#     here::here('in_progress/in_progress_data/qtype_cross.csv')
#   )
#
#   usethis::use_data(qtype_cross, internal = TRUE, overwrite = TRUE)
#
#   invisible(NULL)
#
# }
